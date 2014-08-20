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
