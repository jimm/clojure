(ns mysql-test
  (:use [clojure.contrib.sql :as sql]
        [clojure.contrib.json :as json]))

(def db-metainfo {:json-file "/tmp/database.json" ; created from Rails config file
                  :environment "development"})

(defn db-info
  []
  (get (json/read-json (slurp (:json-file db-metainfo)))
       (keyword (:environment db-metainfo))))

;; Map of adapter name to driver name
(def db-drivers {"mysql" "org.gjt.mm.mysql.Driver"})

(def db (let [info (db-info)]
          {:classname  (get db-drivers (:adapter info))
           :subprotocol (:adapter info)
           :subname (str "//" (:host info) "/" (:database info))
           :user (:username info)
           :password (:password info)}))

(defn sql-test []
  (sql/with-connection db
    (sql/with-query-results rset ["select count(*) as num_users from users"]
      (doseq [row rset] (println row)))
    (sql/with-query-results rset ["select login, email from users where email like ?", "%example.com"]
      (doseq [row rset] (println row)))))
