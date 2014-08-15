(ns cryptopals.set1-test
  (:require [clojure.test :refer :all]
            [cryptopals.set1 :refer :all]))

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

(deftest fixed-or-test
  (testing "fixed-xor"
    (is (= (fixed-xor "1c0111001f010100061a024b53535009181c" "686974207468652062756c6c277320657965")
           "746865206b696420646f6e277420706c6179"))))

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

(deftest single-byte-xor-cipher-test
  (testing "single byte XOR cipher"
    (is (= (single-byte-xor-cipher
            ;; input is bytes
            (hex-str-to-bytes "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"))
           ;; output is a list of (key byte, string bytes)
           (list (byte \X) (str-to-bytes "Cooking MC's like a pound of bacon")))))

  (testing "hex encoded single byte XOR cipher"
    (is (= (hex-str-single-byte-xor-cipher
            "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736")
           "Cooking MC's like a pound of bacon"))))

;; This works, but takes a long time to run

;; (deftest finding-xored-in-file
;;   (testing "finding xored line in a hex-encoded files"
;;     (is (= (find-xored-in-file "data/4.txt")
;;            "Now that the party is jumping\n"))))

(deftest repeating-key-xor-test
  (testing "repeating key XOR"
    (let [plaintext-bytes (str-to-bytes "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal")
          key-bytes (str-to-bytes "ICE")]
      (is (= (repeating-key-xor plaintext-bytes key-bytes)
             (hex-str-to-bytes "0B3637272A2B2E63622C2E69692A23693A2A3C6324202D623D63343C2A26226324272765272A282B2F20430A652E2C652A3124333A653E2B2027630C692B20283165286326302E27282F"))))))

(deftest hamming-distance-test
  (testing "counting one bits"
    (is (= (count-one-bits 0) 0))
    (is (= (count-one-bits 1) 1))
    (is (= (count-one-bits 2) 1))
    (is (= (count-one-bits 3) 2))
    (is (= (count-one-bits 0x83) 3)))

  (testing "hamming distance"
    (is (= (hamming-dist (str-to-bytes "this is a test")
                         (str-to-bytes "wokka wokka!!!"))
           37))
    (is (= (hamming-dist [1 2 3] [1 2 2])
           1))))

;; This works, but takes a relatively long time (a few seconds) to run.

;; (deftest break-repeating-key-xor-in-file-test
;;   (testing "figured out the repeating XOR key"
;;     (let [retval (break-repeating-key-xor-in-file "data/6.txt")]
;;       (is (= (first retval)             ; key
;;              (str-to-bytes "Terminator X: Bring the noise")))
;;       (is (= (take 35 (second retval))
;;              (str-to-bytes "I'm back and I'm ringin' the bell \n"))))))

(deftest aes-in-ecb-mode-test
  (testing "AES in ECB decryption"
    (is (.startsWith (aes-in-ecb-mode-in-file "data/7.txt" (str-to-bytes "YELLOW SUBMARINE"))
                     "I'm back and I'm ringin' the bell \n"))))
