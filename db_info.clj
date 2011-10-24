(ns db-info
  (:use [clojure.contrib.sql :as sql]))

(def db {:classname "com.mysql.jdbc.Driver"
         :subprotocol "mysql"
         :subname "//localhost/brain_development"
         :database "brain_development"
         :user "brainiac"
         :password "does_not_kompute"})

(defn column-info
  [table-name]
  (sql/do-prepared "select * from information_schema.columns where table_schema = ? and table_name = ?" (list (:database db) table-name)))

(defn table-info
  [table-row]
  (merge table-row {:columns (column-info (:table_name table-row))}))

(defn db-info
  "Returns a seq containing table info maps for all tables in db. Each map
  contains :table_name and :columns."
  []
  (with-connection db
    (map table-info (do-prepared "select * from information_schema.tables where table_schema = ?" (list (:database db))))))
