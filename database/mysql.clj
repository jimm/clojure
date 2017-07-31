; Note: you will need the MySQL driver on your class path.

(use 'clojure.contrib.sql)

;;; See also clojure.core/resultset-seq

;;; This database doesn't exist anymore, and besides the db name, user name,
;;; and password below were only for dev mode. All three were completely
;;; different on staging and production.
;;;
;;; That said, see drop_tables.clj for how to read those values from the
;;; coomand line.
(def db {:classname "com.mysql.jdbc.Driver"
         :subprotocol "mysql"
         :subname "//localhost/icarly_cms_development"
         :database "icarly_cms_development"
         :user "jo"
         :password "happyx2joyx2"})

(with-connection db
  (with-query-results res ["select * from pages limit 2"] 
                      (doall res)))
