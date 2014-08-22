(ns cryptopals.set2
  (:use [cryptopals.util])
  (:require [clojure.string :as str])
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
;;
;; blocks 0000 1111 2222 3333 4444
;;
;;        AAAR esto fthe mess age*
;;        AARe stof them essa ge**
;;        ARes toft heme ssag e***
;;        Rest ofth emes sage
;;
;;        est? AAAR esto fthe mess age*
;;        sto? AARe stof them essa ge**
;;        tof? ARes toft heme ssag e**
;;        oft? Rest ofth emes sage

(defn make-ecb-decrypt-map-entry
  "Create a seq of two elements that can be used to create a map later:
  encrypted block (key) and probe byte (value)."
  [block-size probe probe-byte]
  (let [encoded (encrypt-ecb-unknown-key probe)
        block (first-block encoded block-size)]
    (list block probe-byte)))

(defn decrypt-mystery-ecb-message-block
  ;; TODO pre-calculate encryption of "xxxx", "xxx", "xx", "x and pass that in to this func
  [block-num block-size decrypted-bytes encrypted-x-blocks]
  (loop [n (dec block-size)
         decrypted-bytes (vec decrypted-bytes)]
    (if (neg? n)
      decrypted-bytes
      (let [probe-prefix (vec (if (zero? block-num)
                                (concat (repeat n 120) ; XXX...
                                        (take (- (dec block-size) n) decrypted-bytes))
                                (drop (+ (* (dec block-num) block-size)
                                         (- block-size n))
                                      decrypted-bytes)))
            byte-block-dict (apply hash-map
                                   (mapcat #(make-ecb-decrypt-map-entry
                                             block-size
                                             (conj probe-prefix %)
                                             %)
                                           (range 0 256)))
            mystery-byte (get byte-block-dict
                              ;; TODO use passed-in pre-encrypted block
                              (nth-block (nth encrypted-x-blocks n)
                                         block-size
                                         block-num))]
        (if (nil? mystery-byte)
          decrypted-bytes
          (recur (dec n)
                 (conj decrypted-bytes mystery-byte)))))))

(defn decrypt-mystery-ecb-message
  "Decrypt the mystery message using an ECB known text block attack."
  []
  (let [ciphertext-size (count (encrypt-ecb-unknown-key '()))
        block-size (detect-block-size encrypt-ecb-unknown-key)
        algorithm (detect-block-cipher-mode
                   (encrypt-ecb-unknown-key (repeat 48 120))) ; "X" * 48
        encrypted-x-blocks (map #(encrypt-ecb-unknown-key (repeat % 120))
                                (range block-size))]
    (assert (= :ecb algorithm))
    (assert (zero? (rem ciphertext-size block-size)))

  ;; TODO pre-calculate encryption of "xxxx", "xxx", "xx", "x" before the
  ;; loop and pass that in to decrypt-mystery-ecb-message-block.

    (loop [block-num 0
           ciphertext-size ciphertext-size
           decrypted-bytes []]
      (if (zero? ciphertext-size)
        decrypted-bytes
        (recur (inc block-num)
               (- ciphertext-size block-size)
               (decrypt-mystery-ecb-message-block block-num
                                                  block-size
                                                  decrypted-bytes
                                                  encrypted-x-blocks))))))

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

(defn parse-get-params
  [^String s]
  (if (empty? s)
    {}
    (let [kvs (str/split s #"&")]
      (apply hash-map (mapcat #(let [[k v] (str/split % #"=")]
                                 (list k (java.net.URLDecoder/decode v)))
                              kvs)))))

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

(defn url-encode
  [^String s]
  (java.net.URLEncoder/encode s))

(defn to-get-params
  [m]
  (str/join "&"
            (map (fn [[k v]] (str (url-encode k) "=" (url-encode (str v))))
                 m)))

(defn email-to-user
  [email]
  {"email" email, "uid" (rand-int 1000), "role" "user"})

(defn email-to-user-params
  [email]
  (-> email email-to-user to-get-params))

;; Now, two more easy functions. Generate a random AES key, then:

;; Encrypt the encoded user profile under the key; "provide" that to
;; the "attacker".

;; Decrypt the encoded user profile and parse it.

;; Using only the user input to profile_for() (as an oracle to
;; generate "valid" ciphertexts) and the ciphertexts themselves, make a
;; role=admin profile.

;;; ================ 14 ================

;;; Byte-at-a-time ECB decryption (Harder)

;; Take your oracle function from #12. Now generate a random count of random
;; bytes and prepend this string to every plaintext. You are now doing:

;; AES-128-ECB(random-prefix || attacker-controlled || target-bytes, random-key)
;; Same goal: decrypt the target-bytes.

;; Stop and think for a second.

;; What's harder than challenge #12 about doing this? How would you overcome
;; that obstacle? The hint is: you're using all the tools you already have;
;; no crazy math is required.

;; Think "STIMULUS" and "RESPONSE".

;;; ================ 15 ================

;; PKCS#7 padding validation
;; Write a function that takes a plaintext, determines if it has valid PKCS#7 padding, and strips the padding off.

;; The string:

;; "ICE ICE BABY\x04\x04\x04\x04"
;; ... has valid padding, and produces the result "ICE ICE BABY".

;; The string:

;; "ICE ICE BABY\x05\x05\x05\x05"
;; ... does not have valid padding, nor does:

;; "ICE ICE BABY\x01\x02\x03\x04"

;; If you are writing in a language with exceptions, like Python or Ruby,
;; make your function throw an exception on bad padding.

;; Crypto nerds know where we're going with this. Bear with us.

;;; ================ 16 ================

;;; CBC bitflipping attacks

;; Generate a random AES key.

;; Combine your padding code and CBC code to write two functions.

;; The first function should take an arbitrary input string, prepend the
;; string:

;; "comment1=cooking%20MCs;userdata="
;; .. and append the string:
;; ";comment2=%20like%20a%20pound%20of%20bacon"

;; The function should quote out the ";" and "=" characters.

;; The function should then pad out the input to the 16-byte AES block
;; length and encrypt it under the random AES key.

;; The second function should decrypt the string and look for the
;; characters ";admin=true;" (or, equivalently, decrypt, split the string
;; on ";", convert each resulting string into 2-tuples, and look for
;; the "admin" tuple).

;; Return true or false based on whether the string exists.

;; If you've written the first function properly, it should not be possible
;; to provide user input to it that will generate the string the second
;; function is looking for. We'll have to break the crypto to do that.

;; Instead, modify the ciphertext (without knowledge of the AES key) to
;; accomplish this.

;; You're relying on the fact that in CBC mode, a 1-bit error in a
;; ciphertext block:

;; * Completely scrambles the block the error occurs in
;; * Produces the identical 1-bit error(/edit) in the next ciphertext block.

;; Stop and think for a second.
;; Before you implement this attack, answer this question: why does CBC mode
;; have this property?
