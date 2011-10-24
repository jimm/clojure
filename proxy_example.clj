;; From The Joy of Clojure by Fogus and Houser

(ns joy.web
  (:import (com.sun.net.httpserver HttpHandler HttpExchange HttpServer)
           (java.net InetSocketAddress HttpURLConnection)
           (java.io IOException FilterOutputStream)
           #_(java.util Arrays)))

(defn default-handler
  [txt]
  (proxy [HttpHandler] []
    (handle [exchange]
      (.sendResponseHeaders exchange HttpURLConnection/HTTP_OK (.length txt))
      (doto (.getResponseBody exchange)
        (.write (.getBytes txt))
        (.close)))))

(defn new-server
  [port path handler]
  (doto (HttpServer/create (InetSocketAddress. port) 0)
    (.createContext path handler)
    (.setExecutor nil)
    (.start)))

;; Bug: length sent via headers before text is modified via filter.
(defn change-message
  "Convenience method to change a proxy's output message"
  ([p txt] (change-message p identity txt))
  ([p fltr txt]
     (update-proxy p
       {"handle"
        (fn [this exchange]
          (let [b (.getBytes txt)]
            (.sendResponseHeaders exchange
                                  HttpURLConnection/HTTP_OK
                                  (count b))
            (doto (fltr (.getResponseBody exchange))
              (.write b)
              (.close))))})))

;; Exposes a bug in change-message: change-message sends text length before
;; the filter function is called.
(defn silly-filter
  [o]
  (proxy [FilterOutputStream] [o]
    (write [b]
           (proxy-super write (.getBytes (.replaceAll (String. b) "Hello" "Greetings")))
           (.flush this))
    (flush [] (proxy-super flush))
    (close [] (proxy-super close))))

(def server (new-server 8123 "/joy/hello" (default-handler "Hello Cleveland")))
