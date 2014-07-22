(ns cloplayer.parser-test
  (:require [clojure.test :refer :all]
            [cloplayer.parser :refer :all]))

(def line "70.189.110.45 - - [20/Jun/2014:07:59:59 +0000] \"GET /some/url HTTP/1.1\" 200 3116 \"http://example.com/referrer/url\" \"user-agent\" 99699")
(def datetime 1403251199000)

(deftest parser

  (testing "date parsing"
    (is (= datetime (parse-datetime "xyzzy [20/Jun/2014:07:59:59 +0000]"))))

  (testing "line parsing"
    (let [r (parse-log-file-line line)]
      (is (= datetime (:time r)))
      (is (= "/some/url" (:uri r)))
      (is (= "GET" (:method r)))
      (is (= "user-agent" (:user-agent r)))
      (is (= "http://example.com/referrer/url" (:referrer r))))))
