(ns cryptopals.set2-test
  (:require [clojure.test :refer :all]
            [cryptopals.util :refer :all]
            [cryptopals.set1 :refer :all]
            [cryptopals.set2 :refer :all]))

(deftest decrypt-cbc-test
  (testing "CBC decryption"
    (let [retval (decrypt-cbc-in-file "data/10.txt" (str-to-bytes "YELLOW SUBMARINE"))]
      (is (= (take 35 retval)
             (str-to-bytes "I'm back and I'm ringin' the bell \n"))))))

(deftest encrypt-cbc-test
  (testing "CBC encryption"
    (let [bytes (pad (str-to-bytes "Spongebob Squarepants lives in Bikini Bottom.") 16)
          key-bytes (str-to-bytes "YELLOW SUBMARINE")]
      (is (= (decrypt-cbc (encrypt-cbc bytes key-bytes) key-bytes)
             bytes)))))

(deftest cipher-mode-detector-test
  (testing "cipher mode detection"
    (let [plaintext (apply str (repeat 48 "x"))
          plaintext-bytes (str-to-bytes plaintext)]
      (dotimes [_ 10]
        (let [[alg enc-bytes] (encryption-oracle plaintext-bytes)]
          (is (= (detect-block-cipher-mode enc-bytes) alg)))))))

(deftest detect-repeated-blocks-test
  (testing "detection of repeated blocks"
    (is (= (detect-repeated-blocks '(1 2 3 1 2 3)) 3))
    (is (= (detect-repeated-blocks '(1 2 3 1 2 3 4 5 6 7 1 2 3)) 3))
    (is (= (detect-repeated-blocks '(1 2 3 4 5)) nil))
    (is (= (detect-repeated-blocks '()) nil))))

(deftest detect-block-size-test
  (testing "block size detection"
    (is (= (detect-block-size encrypt-ecb-unknown-key) 16))))

;; This takes around 2.5 second to run, but it works

;; (deftest decrypt-mystery-ecb-message-test
;;   (testing "decryption of ECB mystery message"
;;     ;; ignore padding at end
;;     (is (= (take (count mystery-message) (decrypt-mystery-ecb-message ))
;;            (str-to-bytes mystery-message)))))

(deftest url-munging-test
  (testing "decoding get params into map"
    (is (= (parse-get-params "foo=bar") {"foo" "bar"}))
    (is (= (parse-get-params "foo=bar+bar") {"foo" "bar bar"}))
    (is (= (parse-get-params "foo=bar%20bar") {"foo" "bar bar"}))
    (is (= (parse-get-params "") {}))
    (is (= (parse-get-params "foo=bar&baz=bletch") {"foo" "bar", "baz" "bletch"})))

  (testing "url encoding strings"
    (is (= (url-encode "") ""))
    (is (= (url-encode "abc") "abc"))
    (is (= (url-encode "abc&def") "abc%26def"))
    (let [has-space (url-encode "abc def")]
      (is (or (= has-space "abc%20def")
              (= has-space "abc+def")))))

  (testing "encoding map into get params"
    (is (= (to-get-params {}) ""))
    (is (= (to-get-params {"a" "b"}) "a=b"))
    (is (= (to-get-params {"a" 42}) "a=42"))
    (let [m {"foo" "bar", "baz" "bletch"}
          params (to-get-params m)
          s1 "foo=bar&baz=bletch"
          s2 "baz=bletch&foo=bar"]
      (is (or (= params s1)
              (= params s2))))))
