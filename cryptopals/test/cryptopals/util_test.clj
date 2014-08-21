(ns cryptopals.util-test
  (:require [clojure.test :refer :all]
            [cryptopals.util :refer :all]))

(deftest conversion-test
  (testing "string to byte conversion"
    (is (= (str-to-bytes "abc")
           [(byte \a) (byte \b) (byte \c)])))

  (testing "byte to string conversion"
    (is (= (bytes-to-str [97 98 99])
           "abc")))

  (testing "hex to byte conversion"
    (is (= (hex-str-to-bytes "deadbeef")
           [0xde 0xad 0xbe 0xef])))

  (testing "byte to hex conversion"
    (is (= (bytes-to-hex-str [0xde 0xad 0xbe 0xef])
           "deadbeef")))

  (testing "round trip base64 decoding"
    (is (= (bytes-to-hex-str (base64-to-bytes (hex-to-base64 "deadbeef"))))
        "deadbeef")))

(deftest base64-test
  (testing "str-to-base64"
    (is (= (str-to-base64 "deadbeef")
           "ZGVhZGJlZWY=")))

  (testing "hex-to-base64"
    (is (= (hex-to-base64 "deadbeef")
           "3q2+7w=="))
    (is (= (hex-to-base64 "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d")
           "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"))))

(deftest englishness-test
  (testing "englishness"
    (is (= (englishness (str-to-bytes "abc"))
           19/3))
    (is (= (englishness (str-to-bytes "ABC"))
           19/3))
    (is (= (englishness (str-to-bytes "a?"))
           11/2))
    (is (= (englishness (str-to-bytes "This is a test. Fun? Yes!"))
           161/25))
    (is (= (englishness (str-to-bytes "Level 42"))
           48/8))
    (is (= (englishness (str-to-bytes "th*"))
           18/3))))

(deftest hamming-distance-test
  (testing "counting one bits"
    (is (= (count-one-bits 0) 0))
    (is (= (count-one-bits 1) 1))
    (is (= (count-one-bits 2) 1))
    (is (= (count-one-bits 3) 2))
    (is (= (count-one-bits 0x83) 3)))

  (testing "hamming distance"
    (is (= (hamming-distance (str-to-bytes "this is a test")
                         (str-to-bytes "wokka wokka!!!"))
           37))
    (is (= (hamming-distance [1 2 3] [1 2 2])
           1))))

(deftest pad-test
  (testing "padding"
    (is (= (pad [] 3) []))
    (is (= (pad [1 2 3] 2) [1 2 3 1]))
    (is (= (pad [1 2 3] 3) [1 2 3]))
    (is (= (pad [1 2 3] 4) [1 2 3 1]))
    (is (= (pad [1 2 3] 5) [1 2 3 2 2]))
    (is (= (pad [1 2 3] 10) [1 2 3 7 7 7 7 7 7 7]))
    (is (= (pad [1 2 3 4 5] 3) [1 2 3 4 5 1])))
    
  (testing "padding with default length"
    (is (= (pad []) []))
    (is (= (pad [1 2 3]) [1 2 3 13 13 13 13 13 13 13 13 13 13 13 13 13]))
    (is (= (pad [1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17])
           [1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 15 15 15 15 15 15 15 15 15 15 15 15 15 15 15]))))

(deftest nth-block-test
  (testing "first block"
    (is (= (first-block [1 2 3 4] 2) [1 2]))
    (is (= (first-block [1 2] 2) [1 2]))
    (is (= (first-block [1] 2) [1]))
    (is (nil? (first-block [] 2))))

  (testing "nth block"
    (is (= (nth-block [1 2 3 4] 2 0) [1 2]))
    (is (= (nth-block [1 2 3 4] 2 1) [3 4]))
    (is (nil? (nth-block [1 2 3 4] 2 2)))
    (is (nil? (nth-block [1 2 3 4] 2 99)))))
    