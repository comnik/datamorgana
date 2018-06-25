# DataMorgana

DataMorgana crams [core.spec](https://clojure.org/guides/spec)
generators into a
[DataScript](https://github.com/tonsky/datascript)-compatible
interface, allowing you to run Datalog queries against a completely
fake "dataset", that is generated on the fly. DataMorgana is (at best)
in the "cool that it works, but leave me alone" category, but might
illustrate how easy it is to plug into DataScript.

Can't wait to try it out? Define your schema as you would anyways.

``` clojure
(def schema
  {:level/ident    {:db/unique :db.unique/identity}
   :level/rank     {}
   :doc/name       {:db/unique :db.unique/identity}
   :doc/level      {:db/valueType   :db.type/ref
                    :db/cardinality :db.cardinality/one}
   :user/name      {:db/unique :db.unique/identity}
   :user/clearance {:db/valueType   :db.type/ref
                    :db/cardinality :db.cardinality/one}})
```

Annotate it with specs.

``` clojure
(s/def :level/ident #{:top-secret :restricted :official})
(s/def :level/rank (s/int-in 0 3))
(s/def :doc/name string?)
(s/def :doc/level :level/ident)
(s/def :user/name (s/with-gen string? #(s/gen #{"Alice" "Bob"})))
(s/def :user/clearance :level/ident)
```

And then start querying to your heart's content.

``` clojure
(d/q '[:find [?name ...] 
       :where [_ :doc/name ?name]] 
     (create-db schema))
	 
(d/q '[:find [?name ...]
	   :where 
	   [_ :user/name ?name]
       [(str/starts-with? ?name "Ros")]] 
     (create-db schema 100))
	   
(d/q '[:find ?uname ?dname
       :where
       [_ :user/name ?uname]
       [_ :doc/name ?dname]
       [(clojure.core/first ?dname) ?dchar]
       [(clojure.core/first ?uname) ?uchar]
       [(= ?uchar ?dchar)]] 
     (create-db schema 100))
```

Note that you can specify the volume of data to generate, via the
second parameter to `create-db`. This increases the likelihood of
producing results for more complex queries.

Pull queries are supported as well:

``` clojure
(d/pull-many (create-db schema) 
             '[:doc/name :doc/level] 
			 (range 100))
```
