(ns datamorgana.core
  (:require
   [clojure.string :as str]
   [clojure.spec.alpha :as s]
   [clojure.spec.gen.alpha :as gen]
   [datascript.core :as d]
   [datascript.db :as db-internals]))

(def schema
  {:level/ident    {:db/unique :db.unique/identity}
   :level/rank     {}
   :doc/name       {:db/unique :db.unique/identity}
   :doc/level      {:db/valueType   :db.type/ref
                    :db/cardinality :db.cardinality/one}
   :user/name      {:db/unique :db.unique/identity}
   :user/clearance {:db/valueType   :db.type/ref
                    :db/cardinality :db.cardinality/one}})

(s/def ::eid (s/and integer? pos?))
(s/def ::attribute (s/with-gen
                     (s/and keyword? #(contains? schema %))
                     #(s/gen (set (keys schema)))))
(s/def :level/ident #{:top-secret :restricted :official})
(s/def :level/rank (s/int-in 0 3))
(s/def :doc/name string?)
(s/def :doc/level :level/ident)
(s/def :user/name (s/with-gen string? #(s/gen #{"Alice" "Bob" "Mabel" "Dipper" "Nadine" "Lesli" "Arianne" "Elidia" "Dina" "Laurena" "Ricky" "Laveta" "Veola" "Ellie" "Keneth" "Tomika" "Gwenn" "Aletha" "Jama" "Yasuko" "Latonia" "Clarita" "Caroll" "Delfina" "Hanna" "Eden" "Alesha" "Essie" "Lorette" "Greg" "Minda" "Natasha" "Geneva" "Taneka" "Rosita" "Oma" "Devorah" "Roxann" "Alec" "Colton" "Malena" "Laurinda" "Wendolyn" "Jarrod" "Denis" "Hana" "Melanie" "Danika" "Bettyann" "Lorinda" "Arlen" "Verdie" "Kristine" "Tameka"})))
(s/def :user/clearance :level/ident)

(def av-gen
  (gen/bind (s/gen ::attribute)
            (fn [a]
              (gen/fmap (fn [v] [a v]) (s/gen a)))))

(def eav-gen
  (gen/bind (s/gen (s/tuple ::eid ::attribute))
            (fn [[e a]]
              (gen/fmap (fn [v] [e a v]) (s/gen a)))))

(defn- components->pattern [index [c0 c1 c2]]
  (case index
    :eavt [c0 c1 c2]
    :aevt [c1 c0 c2]
    :avet [c2 c0 c1]))

(defn- generate-pattern [pattern volume]
  (let [sample  #(gen/sample % volume)
        [e a v] pattern]
    (db-internals/case-tree
     [e a (some? v)]                
     [[(db-internals/datom e a v)] ; e a v
      (sample (gen/fmap #(db-internals/datom e a %) (s/gen a))) ; e a _
      (sample (gen/fmap #(db-internals/datom e % v) ::attribute)) ; e _ v
      (sample (gen/fmap (fn [[a v]] (db-internals/datom e a v)) av-gen)) ; e _ _
      (sample (gen/fmap #(db-internals/datom % a v) ::eid)) ; _ a v
      (sample (gen/fmap (fn [[e v]] (db-internals/datom e a v)) (s/gen (s/tuple ::eid a)))) ; _ a _
      (sample (gen/fmap (fn [[e v]] (db-internals/datom e a v)) (s/gen (s/tuple ::eid ::attribute)))) ; _ _ v
      (sample (gen/fmap #(apply db-internals/datom %) eav-gen)) ; _ _ _
      ])))

(comment
  
  (gen/sample (s/gen ::attribute))
  (take 15 (gen/sample (s/gen ::eid)))
  (gen/sample (s/gen :doc/name) 5)
  (gen/sample (s/gen :user/name) 5)

  (gen/sample (s/gen (s/tuple ::eid :doc/name)))

  (gen/sample av-gen)
  (gen/sample eav-gen)

  (macroexpand
   '(db-internals/case-tree [e a v]
                            ["e a v" "e a _" "e _ v" "e _ _" "_ a v" "_ a _" "_ _ v" "_ _ _"])))

(defrecord FnDB [schema rschema volume]
  db-internals/IDB
  (-schema [db] (.-schema db))
  (-attrs-by [db property] ((.-rschema db) property))

  db-internals/ISearch
  (-search [db pattern] (generate-pattern pattern (.-volume db)))

  db-internals/IIndexAccess
  (-datoms [db index cs]
    (drop (rand-int 10) (generate-pattern (components->pattern index cs) (.-volume db))))
  (-seek-datoms [db index cs]
    (throw (ex-info "Not implemented: -seek-datoms" {})))
  (-index-range [db attr start end]
    (throw (ex-info "Not implemented: -index-range" {}))))

(defn create-db
  ([schema] (create-db schema 10))
  ([schema volume]
   (let [db (d/empty-db schema)]
     (FnDB. (.-schema db) (.-rschema db) volume))))

(comment

  (def test-db (create-db schema))
  (satisfies? db-internals/IDB test-db)
  (satisfies? db-internals/ISearch test-db)
  (satisfies? db-internals/IIndexAccess test-db)
  (d/db? test-db)

  (count (db-internals/-search (create-db schema 100) [100]))
  (db-internals/-search (create-db schema) [100 :doc/name "BUS SCHEDULE"])
  (db-internals/-search (create-db schema) [100 :doc/name])

  (d/q '[:find [?name ...] :where [?lvl :level/rank _] [?lvl :level/ident ?name]] (create-db schema))
  (d/q '[:find [?name ...] :where [_ :doc/name ?name]] (create-db schema))
  (d/q '[:find [?name ...]
         :where
         [_ :user/name ?name]
         [(str/starts-with? ?name "Ros")]] (create-db schema 10))
  (d/q '[:find ?uname ?dname
         :where
         [_ :user/name ?uname]
         [_ :doc/name ?dname]
         [(clojure.core/first ?dname) ?dchar]
         [(clojure.core/first ?uname) ?uchar]
         [(= ?uchar ?dchar)]] (create-db schema 100))

  (d/pull-many (create-db schema) '[:doc/name :doc/level] (range 5)))
