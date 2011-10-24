; Note: you will need the MySQL driver on your class path.

(use 'clojure.contrib.sql)

(def db {:classname "com.mysql.jdbc.Driver"
         :subprotocol "mysql"
         :subname "//localhost/icarly_cms_development"
         :database "icarly_cms_development"
         :user "jo"
         :password "happyx2joyx2"})

(with-connection db
  (with-query-results res ["select * from pages limit 2"] 
                      (doall res)))
