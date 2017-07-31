;;; Drops tables from a database.
;;;
;;; usage: drop_tables.clj username password db

(ns drop-tables
  (:use [clojure.contrib.sql :as sql]))

(when (< (count *command-line-args*) 3)
  (println "usage: drop_tables.clj username password db")
  (System/exit 0))

(def username (nth *command-line-args* 0))
(def password (nth *command-line-args* 1))
(def db-name (nth *command-line-args* 2))

(def db {:classname "com.mysql.jdbc.Driver"
         :subprotocol "mysql"
         :subname (str "//localhost/" db-name)
         :database db-name
         :user username
         :password password})

(sql/with-connection db
  (do-commands "set foreign_key_checks = 0")
  (sql/with-query-results r ["select table_name from information_schema.tables where table_schema = ?" db-name]
    (doall (map #(drop-table (:table_name %)) r)))
  (do-commands "set foreign_key_checks = 1"))
