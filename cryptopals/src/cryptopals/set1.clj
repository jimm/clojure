(ns cryptopals.set1
  (:use [cryptopals.util])
  (:require [clojure.string :as str])
  (:import [javax.crypto Cipher]
           [javax.crypto.spec SecretKeySpec]))

;; For the REPL:
;; (use '[clojure.repl :only [doc find-doc]])
;; (require '[clojure.string :as str])
;; (import '[javax.crypto Cipher] '[javax.crypto.spec SecretKeySpec])

;;; ================ 2 ================

(defn fixed-xor
  "XOR each byte of two strings and return the resultant string."
  [s0 s1]
  (let [bs0 (hex-str-to-bytes s0)
        bs1 (hex-str-to-bytes s1)
        xored (map bit-xor bs0 bs1)]
  (bytes-to-hex-str xored)))

;;; ================ 3 ================

(defn single-byte-xor-cipher
  "Find the best (most \"English\") match for bytes that has been XOR-ed
  with a mystery byte and return a list containing the key and the string."
  [bytes]
  (let [all-tries (for [key (range 256)] (list key (map #(bit-xor key %) bytes)))]
    (apply max-key (comp englishness second) all-tries)))

(defn hex-str-single-byte-xor-cipher
  "Find the best (most \"English\") match for a hex-encoded String that has
  been XOR-ed with a mystery byte and return the string."
  [^String s]
  (-> s
      hex-str-to-bytes
      single-byte-xor-cipher
      second
      bytes-to-str))

;;; ================ 4 ================

(defn find-xored-in-file
  "Find which string has been XOR-ed with a single byte in a file."
 [path]
 (let [lines (-> path slurp str/split-lines)
       best-xored-bytes (pmap #(-> % hex-str-to-bytes single-byte-xor-cipher second)
                              lines)
       ;; TODO this calls englishness a second time
       bestest (apply max-key englishness best-xored-bytes)]
   (bytes-to-str bestest)))

;;; ================ 5 ================

(defn repeating-key-xor
  "XOR bytes with a key, which may be shorter than the string."
  [bytes key-bytes]
  (map bit-xor bytes (cycle key-bytes)))

;;; ================ 6 ================

(defn pairs
  "Returns all combinations of pairs of items in coll."
  [coll]
  (loop [coll coll
         combis ()]
    (println "coll" coll "combis" combis) ;DEBUG
    (cond
     (or (nil? coll) (nil? (second coll))) (partition 2 combis)
     :else (recur (next coll) (apply concat combis (map #(list (first coll) %) (next coll)))))))

(defn keysize-fitness
  "Given bytes and a key size, return a key size fitness value. Lower
  numbers are better."
  [bytes keysize]
  (let [test-blocks (pairs (take 4 (partition keysize bytes)))
        dists (apply + (map #(hamming-distance %1 %2) test-blocks))]
    (/ dists keysize)))

(defn break-repeating-key-xor
  "Return a list of (repeating key bytes, decoded data bytes) for the given
  bytes."
  [bytes]
  (let [likely-keysize (apply min-key #(keysize-fitness bytes %) (range 2 41))
        blocks (partition likely-keysize bytes)
        transposed (apply map list blocks)
        best-xored-keys-and-bytes (pmap single-byte-xor-cipher transposed)
        key-bytes (pmap first best-xored-keys-and-bytes)]
    (list key-bytes (repeating-key-xor bytes key-bytes))))

(defn break-repeating-key-xor-in-file
  "Return a list of (repeating key bytes, decoded data bytes) for hex a
  encoded file."
  [path]
  (break-repeating-key-xor (base64-file-bytes path)))

;;; ================ 7 ================

(defn aes-in-ecb-mode
  "Encrypt/decrypt bytes using key-bytes. Mode must be :encrypt or :decrypt."
  [bytes key-bytes mode]
  (let [c (Cipher/getInstance "AES/ECB/NoPadding")
        mode (if (= mode :encrypt) Cipher/ENCRYPT_MODE Cipher/DECRYPT_MODE)
        key (SecretKeySpec. (byte-array key-bytes) "AES")]
    (.init c mode key)
    (.doFinal c (byte-array bytes))))

(defn decrypt-aes-in-ecb-mode
  "Decrypt bytes using key-bytes and return plaintext string."
  [bytes key-bytes]
  (String. (aes-in-ecb-mode bytes key-bytes :decrypt)))

(defn encrypt-aes-in-ecb-mode
  "Encrypt bytes using key-bytes and return encrypted bytes."
  [bytes key-bytes]
  (byte-array-to-bytes (aes-in-ecb-mode bytes key-bytes :encrypt)))

(defn decrypt-aes-in-ecb-mode-in-file
  "Decrypt base64-encoded file using key-bytes and return plaintext string."
  [path key-bytes]
  (decrypt-aes-in-ecb-mode (base64-file-bytes path) key-bytes))

;;; ================ 8 ================

(defn repeatedness-factor
  "Low number means more repeats."
  [coll]
  (/ (count (set coll)) (count coll)))

(defn block-sets
  "Create all blocks of various lengths. Return a seq of vectors containing
  the bytes, block length, and the seq of blocks of that length."
  [bytes]
  (let [max-block-len (min 24
                           (inc (int (/ (count bytes) 3))))]
    (for [block-len (range 3 max-block-len) ; block lens to try
          offset (range 0 block-len)]       ; check starting at offset
      [bytes block-len (partition block-len (drop offset bytes))])))

(defn detect-aes-in-ecb-mode
  "Find the collection item that is most likely encrypted using AES in ECB
  mode and returns the plaintext string. Items should be seqs of bytes.
  Return a seq containing plaintext, block length, and blocks."
  [coll]
  (let [b-sets (map block-sets coll)
        candidate (apply min-key
                         (fn [[_ _ blocks]] (repeatedness-factor blocks))
                         b-sets)]
    candidate))

#_(detect-aes-in-ecb-mode-in-file "data/8.txt")

(defn detect-aes-in-ecb-mode-in-file
  "Return a triplet containing the block length, repeatedness factor, and
  bytes of the line in a hex encoded file that is most likely to be
  encoded using AES in ECB."
  [path]
  (let [lines (-> path slurp str/split-lines)
        [bytes block-len blocks] (detect-aes-in-ecb-mode (pmap str-to-bytes lines))]
    [block-len (repeatedness-factor blocks) bytes]))
