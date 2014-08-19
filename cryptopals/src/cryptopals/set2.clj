(ns cryptopals.set2
  (:use [cryptopals.util])
  (:require [cryptopals.set1 :as set1 :only [encrypt-aes-in-ecb-mode
                                             decrypt-aes-in-ecb-mode]]))

;; For the REPL:
;; (use '[cryptopals.util])
;; (use '[clojure.repl :only [doc find-doc]])

;;; ================ 9 ================

;;; See cryptopals.util/pad in util.clj

;;; ================ 10 ================
;;; Implement CBC mode

(defn encrypt-cbc
  "Encrypt bytes using key-bytes and IV. If not specified, IV is set to all
  zero bytes."
  ([bytes key-bytes]
     (encrypt-cbc bytes key-bytes (repeat (count key-bytes) 0)))
  ([bytes key-bytes iv]
     (let [blocks (partition (count key-bytes) bytes)]
       (loop [plaintext-blocks blocks
              prev-block iv
              encrypted-blocks []]
         (if (nil? plaintext-blocks)
           (flatten encrypted-blocks)
           (let [pt-block (first plaintext-blocks)
                 xored-blocks (map bit-xor prev-block pt-block)
                 enc-block (set1/encrypt-aes-in-ecb-mode xored-blocks key-bytes)]
           (recur (next plaintext-blocks)
                  enc-block
                  (conj encrypted-blocks (byte-array-to-bytes enc-block)))))))))

(defn decrypt-cbc
  "Decrypt bytes using key-bytes and IV and return plaintext string. If not
  specified, IV is set to all zero bytes. Returns bytes."
  ([bytes key-bytes]
     (decrypt-cbc bytes key-bytes (repeat (count key-bytes) 0)))
  ([bytes key-bytes iv]
     (let [blocks (partition (count key-bytes) bytes)]
       (loop [encrypted-blocks blocks
              prev-block iv
              decrypted-blocks []]
         (if (nil? encrypted-blocks)
           (byte-array-to-bytes (flatten decrypted-blocks))
           ;; else
           (let [enc-block (first encrypted-blocks)
                 ;; We call set1/aes-in-ecb-mode directly instead of calling
                 ;; set1/decrypt-aes-in-ecb-mode because the latter turns
                 ;; the decrypted bytes into a String, and we want to keep
                 ;; the bytes.
                 dec-block (set1/aes-in-ecb-mode enc-block key-bytes :decrypt)
                 xored-with-prev (map bit-xor prev-block (byte-array-to-bytes dec-block))]
           (recur (next encrypted-blocks)
                  enc-block
                  (conj decrypted-blocks xored-with-prev))))))))

#_(decrypt-cbc-in-file "data/10.txt" (str-to-bytes "YELLOW SUBMARINE"))

(defn decrypt-cbc-in-file
  "Decrypt base64-encoded file using key-bytes and return bytes."
  [path key-bytes]
  (decrypt-cbc (base64-file-bytes path) key-bytes))
