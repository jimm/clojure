;;; http://www.4clojure.com/
;;;
;;; This file is broken up into sections: solved, solved and not yet
;;; submitted, and unsolved. Within each section, problems aren't in any
;;; particular order.
;;;
;;; Each problem defines a variable called "__" that is usually a function.
;;; After the definition is an expression that contains the tests for the
;;; problem, using __.

;;; ****************************************************************
;;; Blank template
;;; ****************************************************************

;;; ****************************************************************
;;; http://www.4clojure.com/problem/NNNN

(def __
  (fn [s]
    )
  )

(and
 )

;;; ****************************************************************
;;; Solved
;;; ****************************************************************

;;; ****************************************************************
;;; http://www.4clojure.com/problem/82#prob-title
(def __
  (fn [words]
    (letfn [(swap [coll i j]
              (assoc coll i (nth coll j) j (nth coll i)))
            (lex-permutations [coll] ; See http://en.wikipedia.org/wiki/Permutation
              (let [sorted-coll (vec (sort coll))
                    len (count coll)]
                (loop [coll sorted-coll
                       answer (vec (list sorted-coll))]
                  (if-let [k (first (take 1 (drop-while #(>= (nth coll %) (nth coll (inc %))) (reverse (range (dec len))))))]
                    (let [kth (nth coll k)
                          l (first (filter #(< kth (nth coll %)) (reverse (range (inc k) len))))
                          swapped (swap coll k l)
                          next-permutation (vec (concat (take (inc k) swapped) (reverse (drop (inc k) swapped))))]
                      (recur next-permutation (conj answer next-permutation)))
                    (reverse answer)))))
            (permutations [items] ; clojure.contrib.combinatorics/permutations
              (let [v (vec items)]
                (map #(map v %) (lex-permutations (range (count v))))))
            ;; Should probably refactor these obtain-by* methods
            (obtain-by? [w0 w1 w0-expected-count w1-expected-count f-recur-w0 f-recur-w1]
              (and (== w0-expected-count w1-expected-count)
                   (loop [w0 (seq w0)
                          w1 (seq w1)
                          num-insertions 0]
                     (cond
                      (> num-insertions 1) false
                      (and (nil? w0) (== 1 num-insertions)) true
                      (nil? w0) false
                      (= (first w0) (first w1)) (recur (next w0) (next w1) num-insertions)
                      :else (recur (f-recur-w0 w0) (f-recur-w1 w1) (inc num-insertions))))))
            (obtain-by-insertion? [w0 w1]
              (obtain-by? w0 w1 (count w1) (inc (count w0)) identity next))
            (obtain-by-deletion? [w0 w1]
              (obtain-by? w0 w1 (count w1) (dec (count w0)) next identity))
            (obtain-by-substitution? [w0 w1]
              (obtain-by? w0 w1 (count w1) (count w0) next next))
            (chain? [words]
              (loop [words words]
                (let [w0 (first words), w1 (second words)]
                  (cond
                   (== 1 (count words)) true
                   (and (not (obtain-by-insertion? w0 w1))
                        (not (obtain-by-deletion? w0 w1))
                        (not (obtain-by-substitution? w0 w1))) false
                        :else (recur (next words))))))
            ]
      (or (some chain? (permutations words))
          false)))
)

(and
 ;; A word chain consists of a set of words ordered so that each word
 ;; differs by only one letter from the words directly before and after it.
 ;; The one letter difference can be either an insertion, a deletion, or a
 ;; substitution. Here is an example word chain:
 ;;
 ;; cat -> cot -> coat -> oat -> hat -> hot -> hog -> dog
 ;;
 ;; Write a function which takes a sequence of words, and returns true if
 ;; they can be arranged into one continous word chain, and false if they
 ;; cannot.
 (= true (__ #{"hat" "coat" "dog" "cat" "oat" "cot" "hot" "hog"}))
 (= false (__ #{"cot" "hot" "bat" "fat"}))
 (= false (__ #{"to" "top" "stop" "tops" "toss"}))
 (= true (__ #{"spout" "do" "pot" "pout" "spot" "dot"}))
 (= true (__ #{"share" "hares" "shares" "hare" "are"}))
 (= false (__ #{"share" "hares" "hare" "are"}))
)

;;; ****************************************************************
;;; http://www.4clojure.com/problem/83#prob-title

(def __
  (fn [& bools]
    (if (and (seq (filter identity bools)) (seq (filter not bools))) true false))
)

(and
 ;; Write a function which takes a variable number of booleans. Your
 ;; function should return true if some of the parameters are true, but not
 ;; all of the parameters are true. Otherwise your function should return
 ;; false.
 (= false (__ false false))
 (= true (__ true false))
 (= false (__ true))
 (= true (__ false true false))
 (= false (__ true true true))
 (= true (__ true true true false)))

;;; ****************************************************************
;;; http://www.4clojure.com/problem/84#prob-title
;;; Http://en.wikipedia.org/wiki/Transitive_closure
;;; http://en.wikipedia.org/wiki/Binary_relation

(def __
  (fn [input]
    (letfn [(find-next-gen [mset k]     ; Return list of values that match key
              (let [found (filter #(= (first %) k) mset)]
                (when (seq found)
                  (map second found))))]
      (loop [m input]
        ;; for each key/value pair, find key/(list of (value of value))
        (let [next-gen (filter second
                               (map #(vector (first %) (find-next-gen m (second %))) m))
              ;; next-gen contains ([key1 (val1 val2)] [key2 (val2 val3)])
              next-gen-expanded (for [item next-gen
                                      :let [k (first item)]
                                      v (second item)]
                                  (vector k v))
              merged (into #{} (concat m next-gen-expanded))]
          (if (= m merged) m
              (recur merged))))))
)

(and
 ;; Write a function which generates the transitive closure of a binary
 ;; relation. The relation will be represented as a set of 2 item vectors.
 (let [divides #{[8 4] [9 3] [4 2] [27 9]}]
   (= (__ divides) #{[4 2] [8 4] [8 2] [9 3] [27 9] [27 3]}))
 (let [more-legs
       #{["cat" "man"] ["man" "snake"] ["spider" "cat"]}]
   (= (__ more-legs)
      #{["cat" "man"] ["cat" "snake"] ["man" "snake"]
        ["spider" "cat"] ["spider" "man"] ["spider" "snake"]}))
 (let [progeny
       #{["father" "son"] ["uncle" "cousin"] ["son" "grandson"]}]
   (= (__ progeny)
      #{["father" "son"] ["father" "grandson"]
        ["uncle" "cousin"] ["son" "grandson"]}))
 )

;;; ****************************************************************

(def __
  (fn [coll1 coll2]
    (into #{} (for [i2 coll2 i1 coll1] [i1 i2])))
  )

(and
 ;; Write a function which calculates the Cartesian product of two sets.
 (= (__ #{"ace" "king" "queen"} #{":a" ":b" ":c" ":d"})
    #{["ace"   ":a"] ["ace"   ":b"] ["ace"   ":c"] ["ace"   ":d"]
      ["king"  ":a"] ["king"  ":b"] ["king"  ":c"] ["king"  ":d"]
      ["queen" ":a"] ["queen" ":b"] ["queen" ":c"] ["queen" ":d"]})
 (= (__ #{1 2 3} #{4 5})
    #{[1 4] [2 4] [3 4] [1 5] [2 5] [3 5]})
 (= 300 (count (__ (into #{} (range 10))
                   (into #{} (range 30)))))
 )

;;; ****************************************************************

(for [x (range 40)
      :when (= 1 (rem x 4))]
  x)

(and
  ;; Clojure's for macro is a tremendously versatile mechanism for producing
  ;; a sequence based on some other sequence(s). It can take some time to
  ;; understand how to use it properly, but that investment will be paid
  ;; back with clear, concise sequence-wrangling later. With that in mind,
  ;; read over these for expressions and try to see how each of them
  ;; produces the same result.
 (= __ (for [x (range 40)
             :when (= 1 (rem x 4))]
         x))
 (= __ (for [x (iterate #(+ 4 %) 0)
             :let [z (inc x)]
             :while (< z 40)]
         z))
 (= __ (for [[x y] (partition 2 (range 20))]
         (+ x y)))

;;; ****************************************************************
;;; http://www.4clojure.com/problem/107

(def __
  (fn [n] (fn [x] (Math/pow x n)))
)

(and
  ;; Lexical scope and first-class functions are two of the most basic
  ;; building blocks of a functional language like Clojure. When you combine
  ;; the two together, you get something very powerful called lexical
  ;; closures. With these, you can exercise a great deal of control over the
  ;; lifetime of your local bindings, saving their values for use later,
  ;; long after the code you're running now has finished.
  ;;
  ;; It can be hard to follow in the abstract, so let's build a simple
  ;; closure. Given a positive integer n, return a function (f x) which
  ;; computes xn. Observe that the effect of this is to preserve the value
  ;; of n for use outside the scope in which it is defined.
 (= 256 ((__ 2) 16),
    ((__ 8) 2))
 (= [1 8 27 64] (map (__ 3) [1 2 3 4]))
 (= [1 2 4 8 16] (map #((__ %) 2) [0 1 2 3 4])))

;;; ****************************************************************
;;; http://www.4clojure.com/problem/88

(def __
  (fn [s1 s2]
    (set (for [thing (set (concat s1 s2))
               :when (or (not (s1 thing))
                         (not (s2 thing)))]
           thing)))
  )

(and
  ;; Write a function which returns the symmetric difference of two sets.
  ;; The symmetric difference is the set of items belonging to one but not
  ;; both of the two sets.
 (= (__ #{1 2 3 4 5 6} #{1 3 5 7}) #{2 4 6 7})
 (= (__ #{:a :b :c} #{}) #{:a :b :c})
 (= (__ #{} #{4 5 6}) #{4 5 6})
 (= (__ #{[1 2] [2 3]} #{[2 3] [3 4]}) #{[1 2] [3 4]}))

;;; ****************************************************************
;;; http://www.4clojure.com/problem/97

(def __
  (fn [n]
    (loop [n n
           row [1]]
      (cond (= n 1) row
            :else (recur (dec n)
                         (into [] (concat [1]
                                          (map #(apply + %) (partition 2 1 row))
                                          [1]))))))
)

(and
  ;; Pascal's triangle is a triangle of numbers computed using the following rules:
  ;;
  ;; - The first row is 1.
  ;; - Each successive row is computed by adding together adjacent numbers
  ;;   in the row above, and adding a 1 to the beginning and end of the row.
  ;;
  ;; Write a function which returns the nth row of Pascal's Triangle. 
 (= (__ 1) [1])
 (= (map __ (range 1 6))
     [     [1]
          [1 1]
         [1 2 1]
        [1 3 3 1]
       [1 4 6 4 1]])
 (= (__ 11)
    [1 10 45 120 210 252 210 120 45 10 1])
 )

;;; ****************************************************************
;;; http://www.4clojure.com/problem/122

(def __
  (fn [s]
    (Integer/parseInt s 2))
)

(and
 ;; Convert a binary number, provided in the form of a string, to its
 ;; numerical value.
 (= 0     (__ "0"))
 (= 7     (__ "111"))
 (= 8     (__ "1000"))
 (= 9     (__ "1001"))
 (= 255   (__ "11111111"))
 (= 1365  (__ "10101010101"))
 (= 65535 (__ "1111111111111111"))
)

;;; ****************************************************************
;;; http://www.4clojure.com/problem/126

(def __
  java.lang.Class
)

;; Enter a value which satisfies the following:
(let [x __]
  (and (= (class x) x) x))

;;; ****************************************************************
;;; http://www.4clojure.com/problem/120

(def __
  (fn [coll]
    (letfn [(digit [ch] (- (int ch) (int \0)))
            (squared-digit [ch] (* (digit ch) (digit ch)))
            (sum-squared-digits [n] (reduce + (map squared-digit (str n))))]
      (count (for [n coll
                   :when (< n (sum-squared-digits n))]
             n))))
)

(and
 ;; Write a function which takes a collection of integers as an argument.
 ;; Return the count of how many elements are smaller than the sum of their
 ;; squared component digits. For example: 10 is larger than 1 squared plus
 ;; 0 squared; whereas 15 is smaller than 1 squared plus 5 squared.
 (= 8 (__ (range 10)))
 (= 19 (__ (range 30)))
 (= 50 (__ (range 100)))
 (= 50 (__ (range 1000)))
)

;;; ****************************************************************
;;; http://www.4clojure.com/problem/100

(def __
  (fn [& nums]
    (letfn [(gcd [a b] (if (zero? b) a (gcd b (mod a b))))]
      (/ (reduce * nums) (reduce gcd nums))))
)

(and
 ;; Write a function which calculates the least common multiple. Your
 ;; function should accept a variable number of positive integers or ratios.
 (== (__ 2 3) 6)
 (== (__ 5 3 7) 105)
 (== (__ 1/3 2/5) 2)
 (== (__ 3/4 1/6) 3/2)
 (== (__ 7 5/7 2 3/5) 210)
)

;;; ****************************************************************
;;; http://www.4clojure.com/problem/95

(def __
  (fn [coll]
    (letfn [(node? [t] (and (coll? t)
                            (= 3 (count t))
                            (not (nil? (first t)))))
            (tree? [t]
              (cond (nil? t) true
                    (node? t) (and (tree? (nth t 1))
                                   (tree? (nth t 2)))
                    :else false))]
      (tree? coll)))
)

(and
 ;; Write a predicate which checks whether or not a given sequence
 ;; represents a binary tree. Each node in the tree must have a value, a
 ;; left child, and a right child.
 (= (__ '(:a (:b nil nil) nil))
    true)
 (= (__ '(:a (:b nil nil)))
   false)
 (= (__ [1 nil [2 [3 nil nil] [4 nil nil]]])
   true)
 (= (__ [1 [2 nil nil] [3 nil nil] [4 nil nil]])
   false)
 (= (__ [1 [2 [3 [4 nil nil] nil] nil] nil])
   true)
 (= (__ [1 [2 [3 [4 false nil] nil] nil] nil])
   false)
 (= (__ '(:a nil ()))
   false)
)

;;; ****************************************************************
;;; http://www.4clojure.com/problem/118

(def __
  (fn [f coll]
    (letfn [(new-map [f coll]
              (lazy-seq
               (when-let [s (seq coll)]
                 (cons (f (first s)) (new-map f (rest s))))))]
       (new-map f (seq coll))))
)

(and
 ;; Map is one of the core elements of a functional programming language.
 ;; Given a function f and an input sequence s, return a lazy sequence of (f
 ;; x) for each element x in s.
 ;;
 ;; Special restrictions: map, map-indexed, mapcat, for
 (= [3 4 5 6 7]
    (__ inc [2 3 4 5 6]))
 (= (repeat 10 nil)
    (__ (fn [_] nil) (range 10)))
 (= [1e6 (inc 1e6)]
    (->> (__ inc (range))
         (drop (dec 1e6))
         (take 2)))
)

;;; ****************************************************************
;;; http://www.4clojure.com/problem/135

(def __
  (fn [& args]
    (loop [arg-pairs (partition 2 (next args)) ; '((+ 2) (+ 4) (+ 8) ...)
           answer (first args)]                ; 20
      (if (empty? arg-pairs) answer
          (let [[op arg] (first arg-pairs)] ; +, 2
            (recur (next arg-pairs)
                   (op answer arg))))))
)

(and
 ;; Your friend Joe is always whining about Lisps using the prefix notation
 ;; for math. Show him how you could easily write a function that does math
 ;; using the infix notation. Is your favorite language that flexible, Joe?
 ;; Write a function that accepts a variable length mathematical expression
 ;; consisting of numbers and the operations +, -, *, and /. Assume a simple
 ;; calculator that does not do precedence and instead just calculates left
 ;; to right.
 (= 7  (__ 2 + 5))
 (= 42 (__ 38 + 48 - 2 / 2))
 (= 8  (__ 10 / 2 - 1 * 2))
 (= 72 (__ 20 / 2 + 2 + 4 + 8 - 6 - 10 * 9))
)

;;; ****************************************************************
;;; http://www.4clojure.com/problem/128

(def __
  (fn [s]
    {:suit ({\D :diamond, \S :spade, \H :heart, \C :club} (first s))
     :rank ({\2 0, \3 1, \4 2, \5 3, \6 4, \7 5, \8 6, \9 7, \T 8, \J 9, \Q 10, \K 11, \A 12} (second s))})
)

(and
 ;; A standard American deck of playing cards has four suits - spades,
 ;; hearts, diamonds, and clubs - and thirteen cards in each suit. Two is
 ;; the lowest rank, followed by other integers up to ten; then the jack,
 ;; queen, king, and ace.
 ;;
 ;; It's convenient for humans to represent these cards as suit/rank pairs,
 ;; such as H5 or DQ: the heart five and diamond queen respectively. But
 ;; these forms are not convenient for programmers, so to write a card game
 ;; you need some way to parse an input string into meaningful components.
 ;; For purposes of determining rank, we will define the cards to be valued
 ;; from 0 (the two) to 12 (the ace)
 ;;
 ;; Write a function which converts (for example) the string "SJ" into a map
 ;; of {:suit :spade, :rank 9}. A ten will always be represented with the
 ;; single character "T", rather than the two characters "10".
 (= {:suit :diamond :rank 10} (__ "DQ"))
 (= {:suit :heart :rank 3} (__ "H5"))
 (= {:suit :club :rank 12} (__ "CA"))
 (= (range 13) (map (comp :rank __ str)
                    '[S2 S3 S4 S5 S6 S7
                      S8 S9 ST SJ SQ SK SA]))
)

;;; ****************************************************************
;;; http://www.4clojure.com/problem/143

(def __
  (fn [c1 c2]
    (reduce + (map * c1 c2)))
)

(and
 ;; Create a function that computes the dot product of two sequences. You
 ;; may assume that the vectors will have the same length.
 (= 0 (__ [0 1 0] [1 0 0]))
 (= 3 (__ [1 1 1] [1 1 1]))
 (= 32 (__ [1 2 3] [4 5 6]))
 (= 256 (__ [2 5 6] [100 10 1]))
)

;;; ****************************************************************
;;; http://www.4clojure.com/problem/147

(def __
  (fn [v]
    (letfn [(next-pascal-row [v]
              (into [] (concat [(first v)]
                               (map #(apply + %) (partition 2 1 v))
                               [(last v)])))]
      (lazy-seq (iterate next-pascal-row v))))
)

(and
 ;; Write a function that, for any given input vector of numbers, returns an
 ;; infinite lazy sequence of vectors, where each next one is constructed
 ;; from the previous following the rules used in Pascal's Triangle. For
 ;; example, for [3 1 2], the next row is [3 4 3 2].
 (= (second (__ [2 3 2])) [2 5 5 2])
 (= (take 5 (__ [1])) [[1] [1 1] [1 2 1] [1 3 3 1] [1 4 6 4 1]])
 (= (take 2 (__ [3 1 2])) [[3 1 2] [3 4 3 2]])
 (= (take 100 (__ [2 4 2])) (rest (take 101 (__ [2 2]))))
)

;;; ****************************************************************
;;; http://www.4clojure.com/problem/85

(def __
  (fn [s]
    ;; The following functions are from clojure.contrib.combinatorics/permutations
    (letfn [(index-combinations [n cnt]
              (lazy-seq
               (let [c (vec (cons nil (for [j (range 1 (inc n))] (+ j cnt (- (inc n)))))),
                     iter-comb
                     (fn iter-comb [c j]
                       (if (> j n) nil
                           (let [c (assoc c j (dec (c j)))]
                             (if (< (c j) j) [c (inc j)]
                                 (loop [c c, j j]
                                   (if (= j 1) [c j]
                                       (recur (assoc c (dec j) (dec (c j))) (dec j)))))))),
                     step
                     (fn step [c j]
                       (cons (rseq (subvec c 1 (inc n)))
                             (lazy-seq (let [next-step (iter-comb c j)]
                                         (when next-step (step (next-step 0) (next-step 1)))))))]
                 (step c 1))))
            (combinations [items n]      
              (let [v-items (vec (reverse items))]
                (if (zero? n) (list ())
                    (let [cnt (count items)]
                      (cond (> n cnt) nil
                            (= n cnt) (list (seq items))
                            :else
                            (map #(map v-items %) (index-combinations n cnt)))))))]
      ;; Here we go
      (set (for [i (range (inc (count s)))
                 c (combinations s i)]
             (set c)))))
)

(and
 ;; Write a function which generates the power set of a given set. The power
 ;; set of a set x is the set of all subsets of x, including the empty set
 ;; and x itself.
 (= (__ #{1 :a}) #{#{1 :a} #{:a} #{} #{1}})
 (= (__ #{}) #{#{}})
 (= (__ #{1 2 3})
    #{#{} #{1} #{2} #{3} #{1 2} #{1 3} #{2 3} #{1 2 3}})
 (= (count (__ (into #{} (range 10)))) 1024)
)

;;; ****************************************************************
;;; http://www.4clojure.com/problem/96

(def __
  (fn [coll]
    (letfn [(node? [t] (and (coll? t)
                            (= 3 (count t))
                            (not (nil? (first t)))))
            (node-val [t] (nth t 0))
            (lchild [t] (nth t 1))
            (rchild [t] (nth t 2))
            (mirrors? [t1 t2]
              (cond
               (and (nil? t1)
                    (nil? t2)) true
               (and (node? t1)
                    (node? t2)) (and (= (node-val t1) (node-val t2))
                                     (mirrors? (lchild t1) (rchild t2))
                                     (mirrors? (rchild t1) (lchild t2)))
               (and (not (node? t1))
                    (not (node? t2))) (= (t1 t2))
               :else false))]
      (mirrors? (lchild coll) (rchild coll))))
)

(and
 ;; Let us define a binary tree as "symmetric" if the left half of the tree
 ;; is the mirror image of the right half of the tree. Write a predicate to
 ;; determine whether or not a given binary tree is symmetric. (see To Tree,
 ;; or not to Tree for a reminder on the tree representation we're using).
 (= (__ '(:a (:b nil nil) (:b nil nil))) true)
 (= (__ '(:a (:b nil nil) nil)) false)
 (= (__ '(:a (:b nil nil) (:c nil nil))) false)
 (= (__ [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
           [2 [3 nil [4 [6 nil nil] [5 nil nil]]] nil]])
    true)
 (= (__ [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
           [2 [3 nil [4 [5 nil nil] [6 nil nil]]] nil]])
    false)
 (= (__ [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
         [2 [3 nil [4 [6 nil nil] nil]] nil]])
    false)
)

;;; ****************************************************************
;;; http://www.4clojure.com/problem/146

(def __
  (fn [m]
    (apply merge
           (for [k (keys m)
                 :let [sub-map (get m k)]
                 v (keys sub-map)]
             {(vector k v) (get sub-map v)})))
  )

(and
 ;; Because Clojure's for macro allows you to "walk" over multiple sequences
 ;; in a nested fashion, it is excellent for transforming all sorts of
 ;; sequences. If you don't want a sequence as your final output (say you
 ;; want a map), you are often still best-off using for, because you can
 ;; produce a sequence and feed it into a map, for example.

 ;; For this problem, your goal is to "flatten" a map of hashmaps. Each key
 ;; in your output map should be the "path" [1] that you would have to take
 ;; in the original map to get to a value, so for example {1 {2 3}} should
 ;; result in {[1 2] 3}. You only need to flatten one level of maps: if one
 ;; of the values is a map, just leave it alone.

 ;; [1] That is, (get-in original [k1 k2]) should be the same as (get result
 ;; [k1 k2])
 (= (__ '{a {p 1, q 2}
          b {m 3, n 4}})
    '{[a p] 1, [a q] 2
      [b m] 3, [b n] 4})
 (= (__ '{[1] {a b c d}
          [2] {q r s t u v w x}})
    '{[[1] a] b, [[1] c] d,
      [[2] q] r, [[2] s] t,
      [[2] u] v, [[2] w] x})
 (= (__ '{m {1 [a b c] 3 nil}})
    '{[m 1] [a b c], [m 3] nil})
 )

;;; ****************************************************************
;;; Solved, not yet submitted
;;; ****************************************************************

;;; ****************************************************************
;;; http://www.4clojure.com/problem/102

(def __
  (fn [s]
    (apply str (concat (list (first s))
                       (for [pair (partition 2 1 s)]
                         (cond
                          (= \- (first pair)) (.toUpperCase (str (second pair)))
                          (= \- (second pair)) ""
                          :else (second pair)))))
    )
  )

(and
 ;; When working with java, you often need to create an object with
 ;; fieldsLikeThis, but you'd rather work with a hashmap that has
 ;; :keys-like-this until it's time to convert. Write a function which takes
 ;; lower-case hyphen-separated strings and converts them to camel-case
 ;; strings.
 (= (__ "something") "something")
 (= (__ "multi-word-key") "multiWordKey")
 (= (__ "leaveMeAlone") "leaveMeAlone")
 )

;;; ****************************************************************
;;; http://www.4clojure.com/problem/86

(def __
  (fn [n]
    (let [char-to-digit (fn [n] (- (int n) (int \0)))
          sum-of-squares-of-digits (fn [n] (apply + (map #(* (char-to-digit %) (char-to-digit %)) (str n))))]
      (loop [n n
             seen #{}]
        (cond (= n 1) true
              (some seen #{n}) false
              :else (recur (sum-of-squares-of-digits n) (conj seen n))))))
  )

(and
 ;; Happy numbers are positive integers that follow a particular formula:
 ;; take each individual digit, square it, and then sum the squares to get a
 ;; new number. Repeat with the new number and eventually, you might get to
 ;; a number whose squared sum is 1. This is a happy number. An unhappy
 ;; number (or sad number) is one that loops endlessly. Write a function
 ;; that determines if a number is happy or not.
 (= (__ 7) true)
 (= (__ 986543210) true)
 (= (__ 2) false)
 (= (__ 3) false)
 )

;;; ****************************************************************
;;; http://www.4clojure.com/problem/98

(def __
  (fn [f d]
    (into #{} (map #(into #{} %) (vals (group-by f d)))))
  )

(and
 ;; A function f defined on a domain D induces an equivalence relation on D,
 ;; as follows: a is equivalent to b with respect to f if and only if (f a)
 ;; is equal to (f b). Write a function with arguments f and D that computes
 ;; the equivalence classes of D with respect to f.
 (= (__ #(* % %) #{-2 -1 0 1 2})
    #{#{0} #{1 -1} #{2 -2}})
 (= (__ #(rem % 3) #{0 1 2 3 4 5 })
    #{#{0 3} #{1 4} #{2 5}})
 (= (__ identity #{0 1 2 3 4})
    #{#{0} #{1} #{2} #{3} #{4}})
 (= (__ (constantly true) #{0 1 2 3 4})
    #{#{0 1 2 3 4}})
 )

;;; ****************************************************************
;;; http://www.4clojure.com/problem/105

(def __
  (fn [coll]
    (loop [coll coll
           key nil
           m {}]
      (let [val (first coll)]
        (cond (empty? coll) m
              (keyword? val) (recur (next coll) val (assoc m val []))
              :else (recur (next coll) key (assoc m key (conj (get m key) val)))))))
  )

(and
 ;; Given an input sequence of keywords and numbers, create a map such that
 ;; each key in the map is a keyword, and the value is a sequence of all the
 ;; numbers (if any) between it and the next keyword in the sequence.
 (= {} (__ []))
 (= {:a [1]} (__ [:a 1]))
 (= {:a [1], :b [2]} (__ [:a 1, :b 2]))
 (= {:a [1 2 3], :b [], :c [4]} (__ [:a 1 2 3 :b :c 4]))
 )

;;; ****************************************************************
;;; http://www.4clojure.com/problem/115

(def __
  (fn [n]
    (letfn [(char-to-digit [c] (- (int c) (int \0)))
            (sum-of-digits [s] (apply + (map char-to-digit s)))]
      (let [s (str n)
            len (.length s)]
        (= (sum-of-digits (.substring s 0 (/ len 2)))
           (sum-of-digits (.substring (str s) (/ (if (even? len) len (inc len)) 2)))))))
  )

(and
 ;; A balanced number is one whose component digits have the same sum on the
 ;; left and right halves of the number. Write a function which accepts an
 ;; integer n, and returns true iff n is balanced.
 (= true (__ 11))
 (= true (__ 121))
 (= false (__ 123))
 (= true (__ 0))
 (= false (__ 88099))
 (= true (__ 89098))
 (= true (__ 89089))
 (= (take 20 (filter __ (range)))
    [0 1 2 3 4 5 6 7 8 9 11 22 33 44 55 66 77 88 99 101])
 )

;;; ****************************************************************
;;; Unsolved
;;; ****************************************************************

;;; ****************************************************************
;;; http://www.4clojure.com/problem/93

(def __
  (fn [s]
    )
  )

(and
 ;; Write a function which flattens any nested combination of sequential
 ;; things (lists, vectors, etc.), but maintains the lowest level sequential
 ;; items. The result should be a sequence of sequences with only one level
 ;; of nesting.
 (= (__ [["Do"] ["Nothing"]])
    [["Do"] ["Nothing"]])
 (= (__ [[[[:a :b]]] [[:c :d]] [:e :f]])
    [[:a :b] [:c :d] [:e :f]])
 (= (__ '((1 2)((3 4)((((5 6)))))))
    '((1 2)(3 4)(5 6)))
 )

;;; ****************************************************************
;;; http://www.4clojure.com/problem/110

(def __
  (fn [s]
    )
  )

(and
 ;; Write a function that returns a lazy sequence of "pronunciations" of a
 ;; sequence of numbers. A pronunciation of each element in the sequence
 ;; consists of the number of repeating identical numbers and the number
 ;; itself. For example, [1 1] is pronounced as [2 1] ("two ones"), which in
 ;; turn is pronounced as [1 2 1 1] ("one two, one one").
 ;;
 ;; Your function should accept an initial sequence of numbers, and return
 ;; an infinite lazy sequence of pronunciations, each element being a
 ;; pronunciation of the previous element.
 (= [[1 1] [2 1] [1 2 1 1]] (take 3 (__ [1])))
 (= [3 1 2 4] (first (__ [1 1 1 4 4])))
 (= [1 1 1 3 2 1 3 2 1 1] (nth (__ [1]) 6))
 (= 338 (count (nth (__ [3 2]) 15)))
 )

;;; ****************************************************************
;;; http://www.4clojure.com/problem/104

(def __
  (fn [s]
    )
  )

(and
 ;; This is the inverse of Problem 92, but much easier. Given an integer
 ;; smaller than 4000, return the corresponding roman numeral in uppercase,
 ;; adhering to the subtractive principle.
 (= "I" (__ 1))
 (= "XXX" (__ 30))
 (= "IV" (__ 4))
 (= "CXL" (__ 140))
 (= "DCCCXXVII" (__ 827))
 (= "MMMCMXCIX" (__ 3999))
 (= "XLVIII" (__ 48))
 )

;;; ****************************************************************
;;; http://www.4clojure.com/problem/108

(def __
  (fn [s]
    )
  )

(and
 ;; Given any number of sequences, each sorted from smallest to largest,
 ;; find the smallest number which appears in each sequence. The sequences
 ;; may be infinite, so be careful to search lazily.
	
 (= 3 (__ [3 4 5]))
 (= 4 (__ [1 2 3 4 5 6 7] [0.5 3/2 4 19]))
 (= 7 (__ (range) (range 0 100 7/6) [2 3 5 7 11 13]))
 (= 64 (__ (map #(* % % %) (range)) ;; perfect cubes
           (filter #(zero? (bit-and % (dec %))) (range)) ;; powers of 2
           (iterate inc 20))) ;; at least as large as 20
 )

;;; ****************************************************************
;;; http://www.4clojure.com/problem/114

(def __
  (fn [s]
    )
  )

(and
 ;; take-while is great for filtering sequences, but it limited: you can
 ;; only examine a single item of the sequence at a time. What if you need
 ;; to keep track of some state as you go over the sequence?
 ;;
 ;; Write a function which accepts an integer n, a predicate p, and a
 ;; sequence. It should return a lazy sequence of items in the list up to,
 ;; but not including, the nth item that satisfies the predicate.
 (= [2 3 5 7 11 13]
    (__ 4 #(= 2 (mod % 3))
        [2 3 5 7 11 13 17 19 23]))
 (= ["this" "is" "a" "sentence"]
    (__ 3 #(some #{\i} %)
        ["this" "is" "a" "sentence" "i" "wrote"]))
 (= ["this" "is"]
    (__ 1 #{"a"}
        ["this" "is" "a" "sentence" "i" "wrote"]))
 )

;;; ****************************************************************
;;; http://www.4clojure.com/problem/137

(def __
  (fn [s]
    )
  )

(and
 ;; Write a function which returns a sequence of digits of a non-negative
 ;; number (first argument) in numerical system with an arbitrary base
 ;; (second argument). Digits should be represented with their integer
 ;; values, e.g. 15 would be [1 5] in base 10, [1 1 1 1] in base 2 and [15]
 ;; in base 16.
 (= [1 2 3 4 5 0 1] (__ 1234501 10))
 (= [0] (__ 0 11))
 (= [1 0 0 1] (__ 9 2))
 (= [1 0] (let [n (rand-int 100000)](__ n n)))
 (= [16 18 5 24 15 1] (__ Integer/MAX_VALUE 42))
 )

;;; ****************************************************************
;;; http://www.4clojure.com/problem/

(def __
  (fn [s]
    )
  )

(and
 ;; A balanced prime is a prime number which is also the mean of the primes
 ;; directly before and after it in the sequence of valid primes. Create a
 ;; function which takes an integer n, and returns true iff it is a balanced
 ;; prime.
 (= false (__ 4))
 (= true (__ 563))
 (= 1103 (nth (filter __ (range)) 15))
 )

;;; ****************************************************************
;;; http://www.4clojure.com/problem/132

(def __
  (fn [s]
    )
  )

(and
 ;; Write a function that takes a two-argument predicate, a value, and a
 ;; collection; and returns a new collection where the value is inserted
 ;; between every two items that satisfy the predicate.
 (= '(1 :less 6 :less 7 4 3) (__ < :less [1 6 7 4 3]))
 (= '(2) (__ > :more [2]))
 (= [0 1 :x 2 :x 3 :x 4]  (__ #(and (pos? %) (< % %2)) :x (range 5)))
 (empty? (__ > :more ()))
 (= [0 1 :same 1 2 3 :same 5 8 13 :same 21]
    (take 12 (->> [0 1]
                  (iterate (fn [[a b]] [b (+ a b)]))
                  (map first) ; fibonacci numbers
                  (__ (fn [a b] ; both even or both odd
                        (= (mod a 2) (mod b 2)))
                      :same))))
 )

;;; ****************************************************************
;;; http://www.4clojure.com/problem/103

(def __
  (fn [s]
    )
  )

(and
 ;; Given a sequence S consisting of n elements generate all k-combinations
 ;; of S, i. e. generate all possible sets consisting of k distinct elements
 ;; taken from S. The number of k-combinations for a sequence is equal to
 ;; the binomial coefficient.
 (= (__ 1 #{4 5 6}) #{#{4} #{5} #{6}})
 (= (__ 10 #{4 5 6}) #{})
 (= (__ 2 #{0 1 2}) #{#{0 1} #{0 2} #{1 2}})
 (= (__ 3 #{0 1 2 3 4}) #{#{0 1 2} #{0 1 3} #{0 1 4} #{0 2 3} #{0 2 4}
                          #{0 3 4} #{1 2 3} #{1 2 4} #{1 3 4} #{2 3 4}})
 (= (__ 4 #{[1 2 3] :a "abc" "efg"}) #{#{[1 2 3] :a "abc" "efg"}})
 (= (__ 2 #{[1 2 3] :a "abc" "efg"}) #{#{[1 2 3] :a} #{[1 2 3] "abc"} #{[1 2 3] "efg"}
                                       #{:a "abc"} #{:a "efg"} #{"abc" "efg"}})
 )
