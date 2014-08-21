(ns cryptopals.set2
  (:use [cryptopals.util])
  (:require [cryptopals.set1 :as set1]))

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

;;; ================ 11 ================

;;; An ECB/CBC detection oracle

;; Now that you have ECB and CBC working:

;; Write a function to generate a random AES key; that's just 16 random bytes.

(defn random-bytes
  ([] (random-bytes 16))
  ([len] (repeatedly len #(rand-int 256))))

;; Write a function that encrypts data under an unknown key --- that is, a
;; function that generates a random key and encrypts under it.
;;
;; The function should look like:
;;
;; encryption_oracle(your-input)
;; => [MEANINGLESS JIBBER JABBER]
;; Under the hood, have the function append 5-10 bytes (count chosen randomly)
;; before the plaintext and 5-10 bytes after the plaintext.
;;
;; Now, have the function choose to encrypt under ECB 1/2 the time, and under
;; CBC the other half (just use random IVs each time for CBC). Use rand(2) to
;; decide which to use.

(defn encryption-oracle
  "Encrypt plaintext-bytes using either ECB or CBC. Surround plaintext-bytes
  with 5-10 random bytes on each end first. Use a random IV for CBC.

  Return a vector containing the algorithm used (which should ONLY be used
  for testing, not for cheating when detecting) and the encrypted bytes."
  [plaintext-bytes]
  (let [bytes (pad (concat (random-bytes (+ 5 (rand-int 6)))
                           plaintext-bytes
                           (random-bytes (+ 5 (rand-int 6)))))
        key-bytes (random-bytes)
        algorithm (if (zero? (rand-int 2)) :ecb :cbc)]
    (case algorithm
      :ecb [:ecb (set1/encrypt-aes-in-ecb-mode bytes key-bytes)]
      :cbc [:cbc (encrypt-cbc bytes key-bytes (random-bytes))])))

;; Detect the block cipher mode the function is using each time. You should end
;; up with a piece of code that, pointed at a block box that might be
;; encrypting ECB or CBC, tells you which one is happening.

(defn detect-block-cipher-mode
  [bytes]
  (let [b-sets (map (fn [[_ _ bs]] bs) (set1/block-sets bytes))
        r-factors (map set1/repeatedness-factor b-sets)
        min-repeat-factor (apply min r-factors)]
    (if (< min-repeat-factor 1)
      :ecb
      :cbc)))

;;; ================ 12 ================

;; Copy your oracle function to a new function that encrypts buffers under ECB
;; mode using a consistent but unknown key (for instance, assign a single
;; random key, once, to a global variable).

(def consistent-rand-key (random-bytes))

;; Now take that same function and have it append to the plaintext, BEFORE
;; ENCRYPTING, the following string:

;; Um9sbGluJyBpbiBteSA1LjAKV2l0aCBteSByYWctdG9wIGRvd24gc28gbXkg
;; aGFpciBjYW4gYmxvdwpUaGUgZ2lybGllcyBvbiBzdGFuZGJ5IHdhdmluZyBq
;; dXN0IHRvIHNheSBoaQpEaWQgeW91IHN0b3A/IE5vLCBJIGp1c3QgZHJvdmUg
;; YnkK

;; ** Spoiler alert. Do not decode this string now. Don't do it.

(def mystery-message
  (base64-to-bytes
   "Um9sbGluJyBpbiBteSA1LjAKV2l0aCBteSByYWctdG9wIGRvd24gc28gbXkgaGFpciBjYW4gYmxvdwpUaGUgZ2lybGllcyBvbiBzdGFuZGJ5IHdhdmluZyBqdXN0IHRvIHNheSBoaQpEaWQgeW91IHN0b3A/IE5vLCBJIGp1c3QgZHJvdmUgYnkK"))

;; Base64 decode the string before appending it. Do not base64 decode the
;; string by hand; make your code do it. The point is that you don't know its
;; contents.

(defn encrypt-ecb-unknown-key
  [bytes]
  (set1/encrypt-aes-in-ecb-mode (pad (concat bytes mystery-message)) consistent-rand-key))

;; What you have now is a function that produces:

;; AES-128-ECB(your-string || unknown-string, random-key)

;; It turns out: you can decrypt "unknown-string" with repeated calls to the
;; oracle function!

;; Here's roughly how:

;; 1. Feed identical bytes of your-string to the function 1 at a time --- start
;; with 1 byte ("A"), then "AA", then "AAA" and so on. Discover the block size
;; of the cipher. You know it, but do this step anyway.

(defn detect-repeated-blocks
  "Returns the size of the two repeated blocks at the beginning of bytes, or
  nil if there is no such repetition."
  [bytes]
  (if (empty? bytes)
    nil
    (first (filter #(= (nth-block bytes % 0)
                       (nth-block bytes % 1))
                   (range 1 2048)))))

(defn detect-block-size
  "Given an encryption function, detect the block size of the cipher."
  [f]
  (loop [bytes '(120)]
    (let [enc (f bytes)
          repeated-block-size (detect-repeated-blocks enc)]
      (cond
       (not (nil? repeated-block-size)) repeated-block-size
       (> (count bytes) 2048) (throw (Exception. "oops, block too big"))
       :else (recur (conj bytes 120))))))

;; 2. Detect that the function is using ECB. You already know, but do this step
;; anyways.

;; 3. Knowing the block size, craft an input block that is exactly 1 byte
;; short (for instance, if the block size is 8 bytes, make "AAAAAAA"). Think
;; about what the oracle function is going to put in that last byte position.

;; 4. Make a dictionary of every possible last byte by feeding different
;; strings to the oracle; for instance, "AAAAAAAA", "AAAAAAAB", "AAAAAAAC",
;; remembering the first block of each invocation.

;; 5. Match the output of the one-byte-short input to one of the entries in
;; your dictionary. You've now discovered the first byte of unknown-string.

;; 6. Repeat for the next byte.

;; Notes:
;; * blocks can be shuffled
;;
;; blocks 0000 1111 2222 3333 4444
;;   text AAAR esto fthe mess age*
;;   text AARe stof them essa ge**
;;   text ARes toft heme ssag e***
;;   text Rest ofth emes sage
;;   text Rest Rest ofth emes sage
;;
;;   text est? AAAR esto fthe mess age*

(defn make-ecb-decrypt-map-entry
  "Create a seq of two elements that can be used to create a map later:
  encrypted block (key) and probe byte (value)."
  [block-size input-block probe-byte]
  (let [probe (conj (vec input-block) probe-byte)
        encoded (encrypt-ecb-unknown-key probe)
        block (first-block encoded block-size)]
    (list block probe-byte)))

(defn decrypt-mystery-message
  "Decrypt the mystery message using an ECB known text block attack."
  []
  (let [block-size (detect-block-size encrypt-ecb-unknown-key)
        algorithm (detect-block-cipher-mode
                   (encrypt-ecb-unknown-key (repeat 48 120)))] ; "X" * 48
    (assert (= :ecb algorithm))
    (loop [decrypted-bytes []]
      (if (= (count decrypted-bytes) block-size)
        decrypted-bytes
        (let [n (- block-size (count decrypted-bytes) 1)
              input-block (repeat n 120) ; XXX...
              byte-block-dict (apply hash-map
                                     (mapcat #(make-ecb-decrypt-map-entry
                                               block-size
                                               ; XXXAnswerSoFar (length = block size - 1)
                                               (concat input-block decrypted-bytes)
                                               %)
                                             (range 0 256)))
              mystery-byte (get byte-block-dict
                                (first-block (encrypt-ecb-unknown-key input-block) block-size))]
          (recur (conj decrypted-bytes mystery-byte)))))))


;; ** Congratulations.

;; This is the first challenge we've given you whose solution will break real
;; crypto. Lots of people know that when you encrypt something in ECB mode, you
;; can see penguins through it. Not so many of them can decrypt the contents of
;; those ciphertexts, and now you can. If our experience is any guideline, this
;; attack will get you code execution in security tests about once a year.

;;; ================ 13 ================

;;; ECB cut-and-paste

;; Write a k=v parsing routine, as if for a structured cookie. The routine
;; should take:

;; foo=bar&baz=qux&zap=zazzle
;; ... and produce:

;; {
;;   foo: 'bar',
;;   baz: 'qux',
;;   zap: 'zazzle'
;; }

;; (you know, the object; I don't care if you convert it to JSON).

;; Now write a function that encodes a user profile in that format, given an
;; email address. You should have something like:

;; profile_for("foo@bar.com")
;; ... and it should produce:

;; {
;;   email: 'foo@bar.com',
;;   uid: 10,
;;   role: 'user'
;; }
;; ... encoded as:

;; email=foo@bar.com&uid=10&role=user

;; Your "profile_for" function should not allow encoding metacharacters (&
;; and =). Eat them, quote them, whatever you want to do, but don't let
;; people set their email address to "foo@bar.com&role=admin".

;; Now, two more easy functions. Generate a random AES key, then:

;; Encrypt the encoded user profile under the key; "provide" that to
;; the "attacker".

;; Decrypt the encoded user profile and parse it.

;; Using only the user input to profile_for() (as an oracle to
;; generate "valid" ciphertexts) and the ciphertexts themselves, make a
;; role=admin profile.
