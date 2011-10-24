;;; See the find-all docs below. You need to (re)define *db-conn*.
;;;
;;; See also http://github.com/duelinmarkers/clj-record

(ns jimm.orm
  (:use [clojure.contrib.sql :as sql]))

(def *db-conn* {:classname "com.mysql.jdbc.Driver"
                :subprotocol "mysql"
                :subname "//localhost/my_db"
                :database "my_db"
                :user "the_user"
                :password "the_pwd"})

(defn- add-option
  "Given a query vector, add the value from opts for key. The value may be a
  scalar or a vector. See find-all for details."
  [query key opts]
  (if-let [val (key opts)]
    (let [clause-name (.replaceAll (name key) "_" " ")
          query-start (str (first query) " " clause-name " ")]
      (into []
            (flatten
             (if (vector? val)
               (vector (str query-start (first val)) (rest query) (rest val))
               (vector (str query-start val) (rest query))))))
    query))

(defn build-query
  [opts]
  (reduce #(add-option %1 %2 opts)
          [""]
          [:select :from :joins :where :group_by :having :order_by :limit :offset]))

(defn find-raw
  "Runs SQL built from sql-map and returns a seq containing all matching
  records. sql-map must be a map containing at least :select and :from
  keys. See find-all for list of keys."
  [sql-map]
  (sql/with-connection *db-conn*
    (sql/with-query-results res (build-query sql-map) (into [] res))))

(defn find-all
  "Returns a seq containing all records in a table that match the options.
  If specified, opts is a map containing optional key/value pairs. Each
  pair's value is either a scalar or a vector containing a parameterized
  string and the values to use. For example,

    {:where [\"color = ?\", \"blue\"] :limit 10}.

  The keys used are:

    :select    ; select clause; default = \"*\"
    :from      ; from clause; default is table name passed in
    :where     ; where clause; default is no where clause
    :group_by  ; group by clause; default is none
    :having    ; having clause; default is none
    :order_by  ; order by clause; default is none
    :limit     ; limit clause; default is none
    :offset    ; offset clause; default is none

  See also find-raw, find-first, and find-by-id."
  ([table] (find-all table {}))
  ([table & opts] 
     (find-raw (merge {:select "*" :from table} opts))))

(defn find-first
  "Returns the first record in a table that matches the options. See
  find-all for list of option keys."
  ([table] (find-first table {}))
  ([table opts]
     (sql/with-connection *db-conn*
       (sql/with-query-results res
         (build-query (merge {:select "*" :from table} opts {:limit 1}))
         (first res)))))

(defn find-first-by
  "Returns the record that match the specified val in column col."
  [table col val]
  (find-first table {:where [(str col " = ?") val]}))

(defn find-by
  "Returns all records that match the specified val in column col."
  [table col val]
  (find-all table {:where [(str col " = ?") val]}))

(defn find-by-id
  "Returns the record that has the specified id."
  [table id]
  (find-first table {:where ["id = ?" id]}))

(defn schema-version
  "Returns schema version number from schema_version table."
  []
  (:version (find-first "schema_version" {:select "max(version) as version"})))
