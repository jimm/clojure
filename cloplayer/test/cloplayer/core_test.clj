(ns cloplayer.core-test
  (:require [clojure.test :refer :all]
            [cloplayer.core :refer :all]
            [cloplayer.parser :as parser]
            [clojure.java.io :as io]))

(def line "70.189.110.45 - - [20/Jun/2014:07:59:59 +0000] \"GET /some/url HTTP/1.1\" 200 3116 \"http://example.com/referrer/url\" \"user-agent\" 99699")

(deftest core

  (testing "connection creation"
    (let [request (parser/parse-log-file-line line)
          conn (make-http-connection "localhost" request)]
      (is (= (io/as-url "http://localhost/some/url") (.getURL conn)))
      (is (= "GET" (.getRequestMethod conn)))
      (is (= "user-agent" (.getRequestProperty conn "User-Agent")))
      (is (= "http://example.com/referrer/url" (.getRequestProperty conn "Referer")))))) ; [sic]
