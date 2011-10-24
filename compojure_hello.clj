;;; In Emacs, use swank-clojure-project.
;;;
;;; The olds way: tart this up with C-u C-c C-z. That will ask you for the
;;; program to run. Enter "compojure --nowrap", which runs a script I wrote
;;; that launches Compojure. Then you can either load this file or define
;;; everything separately. The server will run in the background. When you
;;; re-define functions the changes take effect immediately.

(ns hello-world
  (:use compojure.core
        ring.adapter.jetty))

(defmacro xhtml-page
  [& body]
  `(html
    (doctype :xhtml-transitional)
    [:html [:body ~@body]]))

(defroutes greeter
  ; either (route :id) or (:id route)
  (GET "/a" (xhtml-page [:h1 "A"] [:p "you didn't give me an id"]))
  (GET "/a/:id" (xhtml-page [:h1 "A"] [:p "id param = " (params :id)]))
  (GET "/b" (xhtml-page [:h1 "B"]))
  (GET "/c" (xhtml-page [:h1 "C"]))
  (GET "/request" request (xhtml-page (str request)))
  ; Serve a file. First arg is optional; default is "./public".
  (GET "/self" (serve-file "." "compojure_hello.clj"))
  ; Serve any file. If not found, fall through.
  (GET "/*" (or (serve-file "." (params :*)) :next))
  ; (page-not-found) serves public/404.html, so we customize
  (ANY "*" [404 (xhtml-page [:h1 "Not Found"] [:p "Not Found: " (params :*)])])
)

; Create server, save it, and start it. This way you can stop it, too.
(def hello-server (http-server {:port 8080} "/*" (servlet greeter)))

(start hello-server)

(stop hello-server)


; Starting the Web server

(def my-jetty-server (ref {}))

(defn server-start
  "Starts a server and returns it. If env is :development, starts the server
  in the background; you can call server-stop to stop it."
 [env]
 (let [jetty-args (condp = env
                    :development {:join? false :port 8080}
                    :production {:port 8080})]
   (dosync
    (ref-set my-jetty-server
             {:environment env
              :server (run-jetty main-routes jetty-args)}))))

(defn server-stop
  "Stops a server. If no server is specified, uses the server stored in the
   ref my-jetty-server."
  ([] (server-stop (:server @my-jetty-server)))
  ([server] (.stop server)))

(defn server-restart
  "Restart a running server. If no server is specified, uses the server and
   environment stored in the contents of the ref my-jetty-server."
  []
  (server-stop)
  (server-start (:environment @my-jetty-server)))
