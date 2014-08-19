(ns cryptopals.set1-test
  (:require [clojure.test :refer :all]
            [cryptopals.util :refer [str-to-bytes hex-str-to-bytes pad]]
            [cryptopals.set1 :refer :all]))

(deftest fixed-or-test
  (testing "fixed-xor"
    (is (= (fixed-xor "1c0111001f010100061a024b53535009181c" "686974207468652062756c6c277320657965")
           "746865206b696420646f6e277420706c6179"))))

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

;; This works, but takes a few seconds to run

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

;; This works, but takes a relatively long time (a few seconds) to run.

;; (deftest break-repeating-key-xor-in-file-test
;;   (testing "figured out the repeating XOR key"
;;     (let [retval (break-repeating-key-xor-in-file "data/6.txt")]
;;       (is (= (first retval)             ; key
;;              (str-to-bytes "Terminator X: Bring the noise")))
;;       (is (= (take 35 (second retval))
;;              (str-to-bytes "I'm back and I'm ringin' the bell \n"))))))

(deftest decrypt-aes-in-ecb-mode-test
  (testing "AES in ECB decryption"
    (is (.startsWith (decrypt-aes-in-ecb-mode-in-file "data/7.txt" (str-to-bytes "YELLOW SUBMARINE"))
                     "I'm back and I'm ringin' the bell \n"))))

(deftest encrypt-aes-in-ecb-mode-test
  (testing "make sure AES in ECB encryption works"
    (let [plaintext "Spongebob Squarepants lives in Bikini Bottom!!!!"
          bytes (str-to-bytes plaintext)
          key (str-to-bytes "lock-and-key!!?!")]
    (is (= (decrypt-aes-in-ecb-mode (encrypt-aes-in-ecb-mode bytes key) key)
           plaintext)))))
