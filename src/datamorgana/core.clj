(ns datamorgana.core
  (:require
   [clojure.string :as str]
   [clojure.spec.alpha :as s]
   [clojure.spec.gen.alpha :as gen]
   [datascript.core :as d]
   [datascript.db :as db-internals]))

(s/def :db/id (s/and integer? pos?))

(defrecord Generators [eids attributes values avs eavs])

(defn make-generators [schema]
  (let [eids       (s/gen :db/id)
        attributes (s/gen (set (keys schema)))
        values     (fn [a] (s/gen a))
        avs        (gen/bind attributes
                             (fn [a]
                               (gen/fmap (fn [v] [a v]) (values a))))
        eavs       (gen/bind (gen/tuple eids attributes)
                             (fn [[e a]]
                               (gen/fmap (fn [v] [e a v]) (values a))))]
    (Generators. eids attributes values avs eavs)))

(defn generate-pattern [^Generators generators pattern volume]
  (let [sample                                    #(gen/sample % volume)
        [e a v]                                   pattern
        {:keys [eids attributes values avs eavs]} generators]
    (db-internals/case-tree
     [e a (some? v)]
     [[(db-internals/datom e a v)] ; e a v
      (sample (gen/fmap #(db-internals/datom e a %) (values a))) ; e a _
      (sample (gen/fmap #(db-internals/datom e % v) attributes)) ; e _ v @TODO correct type
      (sample (gen/fmap (fn [[a v]] (db-internals/datom e a v)) avs)) ; e _ _
      (sample (gen/fmap #(db-internals/datom % a v) eids)) ; _ a v
      (sample (gen/fmap (fn [[e v]] (db-internals/datom e a v)) (gen/tuple eids (values a)))) ; _ a _
      (sample (gen/fmap (fn [[e v]] (db-internals/datom e a v)) (gen/tuple eids attributes))) ; _ _ v 
      (sample (gen/fmap #(apply db-internals/datom %) eavs)) ; _ _ _
      ])))

(defn- components->pattern [index [c0 c1 c2]]
  (case index
    :eavt [c0 c1 c2]
    :aevt [c1 c0 c2]
    :avet [c2 c0 c1]))

(defrecord FnDB [schema rschema volume generators]
  db-internals/IDB
  (-schema [db] (.-schema db))
  (-attrs-by [db property] ((.-rschema db) property))

  db-internals/ISearch
  (-search [db pattern] (generate-pattern (.-generators db) pattern (.-volume db)))

  db-internals/IIndexAccess
  (-datoms [db index cs]
    (drop (rand-int 10) (generate-pattern (.-generators db) (components->pattern index cs) (.-volume db))))
  (-seek-datoms [db index cs]
    (throw (ex-info "Not implemented: -seek-datoms" {})))
  (-index-range [db attr start end]
    (throw (ex-info "Not implemented: -index-range" {}))))

(defn create-db
  ([schema] (create-db schema 10))
  ([schema volume]
   (let [db         (d/empty-db schema)
         generators (make-generators schema)]
     (FnDB. (.-schema db) (.-rschema db) volume generators))))

;; TEST

(comment
  (let [db (create-db {:user/name  {:db/unique :db.unique/identity}
                       :user/knows {:db/valueType   :db.type/ref
                                    :db/cardinality :db.cardinality/many}})]
    (satisfies? db-internals/IDB db)
    (satisfies? db-internals/ISearch db)
    (satisfies? db-internals/IIndexAccess db)
    (d/db? db))
  )

;; DEMO

(comment

  (def schema
    {:level/ident    {:db/unique :db.unique/identity}
     :level/rank     {}
     :doc/name       {:db/unique :db.unique/identity}
     :doc/level      {:db/valueType   :db.type/ref
                      :db/cardinality :db.cardinality/one}
     :user/name      {:db/unique :db.unique/identity}
     :user/clearance {:db/valueType   :db.type/ref
                      :db/cardinality :db.cardinality/one}})

  (s/def :level/ident #{:top-secret :restricted :official})
  (s/def :level/rank (s/int-in 0 3))
  (s/def :doc/name string?)
  (s/def :doc/level :level/ident)
  (s/def :user/name (s/with-gen string? #(s/gen #{"Alice" "Bob" "Mabel" "Dipper" "Nadine" "Lesli" "Arianne" "Elidia" "Dina" "Laurena" "Ricky" "Laveta" "Veola" "Ellie" "Keneth" "Tomika" "Gwenn" "Aletha" "Jama" "Yasuko" "Latonia" "Clarita" "Caroll" "Delfina" "Hanna" "Eden" "Alesha" "Essie" "Lorette" "Greg" "Minda" "Natasha" "Geneva" "Taneka" "Rosita" "Oma" "Devorah" "Roxann" "Alec" "Colton" "Malena" "Laurinda" "Wendolyn" "Jarrod" "Denis" "Hana" "Melanie" "Danika" "Bettyann" "Lorinda" "Arlen" "Verdie" "Kristine" "Tameka"})))
  (s/def :user/clearance :level/ident)

  (count (db-internals/-search (create-db schema 100) [100]))
  (db-internals/-search (create-db schema) [100 :doc/name "BUS SCHEDULE"])
  (db-internals/-search (create-db schema) [100 :doc/name])
  (db-internals/-search (create-db schema 3) [1 nil "Something"])

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

  (d/pull-many (create-db schema) '[:doc/name :doc/level] (range 5))
  )

(comment

  (def schema
    {:user/name    {:db/unique :db.unique/identity}
     :user/partner {:db/valueType   :db.type/ref
                    :db/cardinality :db.cardinality/one}
     :user/knows   {:db/valueType   :db.type/ref
                    :db/cardinality :db.cardinality/many}})

  (s/def :user/name (s/with-gen string? #(s/gen #{"Alice" "Bob" "Mabel" "Dipper" "Nadine" "Lesli" "Arianne" "Elidia" "Dina" "Laurena" "Ricky" "Laveta" "Veola" "Ellie" "Keneth" "Tomika" "Gwenn" "Aletha" "Jama" "Yasuko" "Latonia" "Clarita" "Caroll" "Delfina" "Hanna" "Eden" "Alesha" "Essie" "Lorette" "Greg" "Minda" "Natasha" "Geneva" "Taneka" "Rosita" "Oma" "Devorah" "Roxann" "Alec" "Colton" "Malena" "Laurinda" "Wendolyn" "Jarrod" "Denis" "Hana" "Melanie" "Danika" "Bettyann" "Lorinda" "Arlen" "Verdie" "Kristine" "Tameka"})))
  (s/def :user/knows :db/id)
  (s/def :user/partner :db/id)

  (d/pull-many (create-db schema) '[:user/name {:user/partner [:user/name]}] (range 10))

  (def db0
    (let [tx (d/pull-many (create-db schema) '[:user/name {:user/knows [:user/name]}] (range 100))]
      (d/db-with (d/empty-db schema) tx)))

  (d/q '[:find ?u1 ?u2 ?x
         :where
         [?x :user/name "Alice"]
         [?u1 :user/knows ?x]
         [?u2 :user/knows ?x]
         [(not= ?u1 ?u2)]] db0)

  (d/pull-many db0 '[:db/id :user/name {:user/knows [:user/name]}] [36])

  )
