(ns cryptopals.set1
  (:require [clojure.string :as str])
  (:import [javax.crypto Cipher]
           [javax.crypto.spec SecretKeySpec]))

;; For the REPL:
;; (require '[clojure.string :as str])
;; (import '[javax.crypto Cipher] '[javax.crypto.spec SecretKeySpec])

;;; ================ conversion utilities ================

(def hex-chars {\0 0, \1 1, \2 2, \3 3,
                \4 4, \5 5, \6 6, \7 7, \8 8, \9 9,
                \a 10, \b 11, \c 12, \d 13, \e 14, \f 15,
                \A 10, \B 11, \C 12, \D 13, \E 14, \F 15})

(defn str-to-bytes
  "Convert a string into a seq of bytes."
  [^String s]
  (map byte s))

(defn bytes-to-str
  "Convert a seq of bytes into a string."
  [bytes]
  (String. (byte-array bytes)))

(defn hex-str-to-bytes
  "Convert a hex encoded number to a seq of bytes."
  [^String hs]
  (for [[c0 c1] (partition 2 (.toLowerCase hs))]
    (+ (* 16 (hex-chars c0))
       (hex-chars c1))))

(defn bytes-to-hex-str
  "Convert a seq of bytes into a hex encoded string."
  [bytes]
  (apply str (for [b bytes] (format "%02x" b))))

;;; ================ 1 ================

(defn bytes-to-base64
  "Convert a byte array to a base64 String."
  [bytes]
  (String. (.encode (java.util.Base64/getEncoder) (byte-array bytes))))

(defn str-to-base64
  "Convert a string to base64."
  [^String s]
  (-> s
      str-to-bytes
      bytes-to-base64))

(defn hex-to-base64
  "Convert a hex encoded string to base64."
  [^String hex-str]
  (-> hex-str
      hex-str-to-bytes
      bytes-to-base64))

(defn base64-to-bytes
  "Convert a base64 string to a byte sequence."
  [^String b64]
  (into [] (.decode (java.util.Base64/getDecoder) b64)))

;;; ================ file utility functions ================

(defn base64-file-bytes
  "Read file at path and convert contents from base64 to bytes."
  [path]
  (-> path slurp (str/replace "\n" "") base64-to-bytes))

;;; ================ 2 ================

(defn fixed-xor
  [s0 s1]
  (let [bs0 (hex-str-to-bytes s0)
        bs1 (hex-str-to-bytes s1)
        xored (map bit-xor bs0 bs1)]
  (bytes-to-hex-str xored)))

;;; ================ 3 ================

(def relative-freqs {
    (byte \e) 13, (byte \t) 9, (byte \a) 8, (byte \o) 8, (byte \i) 7, (byte \n) 7,
    (byte \s)  6, (byte \h) 6, (byte \r) 6, (byte \d) 4, (byte \l) 4, (byte \c) 3,
    (byte \u)  3, (byte \m) 2, (byte \w) 2, (byte \f) 2, (byte \g) 2, (byte \y) 2,
    (byte \p)  2, (byte \b) 2, (byte \v) 1, (byte \k) 1, (byte \j) 1, (byte \x) 1,
    (byte \q) 1, (byte \z), 1,
    (byte \E) 13, (byte \T) 9, (byte \A) 8, (byte \O) 8, (byte \I) 7, (byte \N) 7,
    (byte \S)  6, (byte \H) 6, (byte \R) 6, (byte \D) 4, (byte \L) 4, (byte \C) 3,
    (byte \U)  3, (byte \M) 2, (byte \W) 2, (byte \F) 2, (byte \G) 2, (byte \Y) 2,
    (byte \P)  2, (byte \B) 2, (byte \V) 1, (byte \K) 1, (byte \J) 1, (byte \X) 1,
    (byte \Q) 1, (byte \Z), 1})

(def punctuation #{(byte \space)
                   (byte \.)
                   (byte \,)
                   (byte \!)
                   (byte \?)
                   (byte \:)
                   (byte \;)
                   (byte \()
                   (byte \))})

(defn byte-englishness
  "Calculate a number that determines how likely that the given byte is in
  english text. Higher numbers are better. Letters get two points plus a
  bonus for how popular they are (\"etaion shrdlu\"); whitespace, most
  punctuation, and numbers get one. Other characters get a negative one."
  [b]
  (cond
   (or (<= 65 b 90)                     ; A - Z
       (<= 97 b 122))                   ; a - z
   (+ 2 (relative-freqs b))

   (or (<= 48 b 57)                     ; 0 - 9
       (some #{b} punctuation))         ; punctuation
   1

   :else
   -1))

(defn englishness
  "Calculate a number that determines how likely that the given bytes are
  English text. See byte-englishness. The final value is normalized by
  dividing by the length of bytes."
  [bytes]
  (/ (reduce + (map byte-englishness bytes))
     (count bytes)))

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
       best-xored-bytes (map #(-> % hex-str-to-bytes single-byte-xor-cipher second)
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

(defn count-one-bits
  "Compute the number of one bits in an integer."
  [^Integer n]
  (loop [n n
         sum 0]
    (cond (zero? n) sum
          (even? n) (recur (bit-shift-right n 1) sum)
          :else     (recur (bit-shift-right n 1) (inc sum)))))

(def count-one-bits (memoize count-one-bits))

(defn hamming-dist
  "Compute the Hamming distance between two bit sequences."
  [bytes0 bytes1]
  (reduce + (map (comp count-one-bits bit-xor) bytes0 bytes1)))

(defn keysize-fitness
  "Given bytes and a key size, return a key size fitness value. Lower
  numbers are better."
  [bytes keysize]
  (let [test-blocks (take 4 (partition keysize bytes))
        dists (+ (hamming-dist (nth test-blocks 0) (nth test-blocks 1))
                 (hamming-dist (nth test-blocks 0) (nth test-blocks 2))
                 (hamming-dist (nth test-blocks 0) (nth test-blocks 3))
                 (hamming-dist (nth test-blocks 1) (nth test-blocks 2))
                 (hamming-dist (nth test-blocks 1) (nth test-blocks 3))
                 (hamming-dist (nth test-blocks 2) (nth test-blocks 3)))]
    (/ dists (* keysize 6))))

(defn break-repeating-key-xor
  "Return a list of (repeating key bytes, decoded data bytes) for the given
  bytes."
  [bytes]
  (let [likely-keysize (apply min-key #(keysize-fitness bytes %) (range 2 41))
        blocks (partition likely-keysize bytes)
        transposed (apply map list blocks)
        best-xored-keys-and-bytes (map single-byte-xor-cipher transposed)
        key-bytes (map first best-xored-keys-and-bytes)]
    (list key-bytes (repeating-key-xor bytes key-bytes))))

(defn break-repeating-key-xor-in-file
  "Return a list of (repeating key bytes, decoded data bytes) for hex a
  encoded file."
  [path]
  (break-repeating-key-xor (base64-file-bytes path)))

;;; ================ 6 ================

(defn aes-in-ecb-mode
  "Decrypt bytes using key-bytes and return plaintext string."
  [bytes key-bytes]
  (let [c (Cipher/getInstance "AES/ECB/NoPadding")
        key (SecretKeySpec. (byte-array key-bytes) "AES")]
    (.init c Cipher/DECRYPT_MODE key)
    (String. (.doFinal c (byte-array bytes)))))

(defn aes-in-ecb-mode-in-file
  [path key-bytes]
  (aes-in-ecb-mode (base64-file-bytes path) key-bytes))
