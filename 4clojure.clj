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
;;; How about code to load the problems? Here we go:
;;; ****************************************************************

(def unsolved-problems
  [92 91 101 94 106 89 113 117 119 125 111 130 124 127 138 140])

(def url-prefix "http://www.4clojure.com/problem/")

(defn problem-url [n] (str url-prefix n))

(defn match-groups
  [html regex]
  (let [pattern (java.util.regex.Pattern/compile regex java.util.regex.Pattern/DOTALL)
        matcher (.matcher pattern html)]
    (loop [f (re-find matcher)
           matches []]
      (if (nil? f)
        matches
        (recur (re-find matcher) (conj matches (second f)))))))

(defn problem-parts
  [html]
  (let [desc (first (match-groups html "<div id=\"prob-desc\">(.*?)<table class=\"testcases\">"))
        answer-table (first (match-groups html "<table class=\"testcases\">(.*?)</table"))
        answers (match-groups answer-table "<pre class=\"brush[^>]+>(.*?)</pre>")]
    [desc answers]))

(defn read-unsolved-problem
  [n]
  (let [html (slurp (problem-url n))
        [desc tests] (problem-parts html)]
    (str ";;; ****************************************************************\n"
         ";;; " (problem-url n) "\n"
         "\n"
         "(def __\n"
         "  (fn [s]\n"
         "    )\n"
         "  )\n"
         "\n"
         "(and\n"
         " ;; " (.replaceAll (.replaceAll desc "<br /><br />" "") "[\n\r]+" "\n ;;\n ;; ")
         (apply str (interpose "\n " tests))
         " )\n")))

(defn read-unsolved-problems
  "Read unsolved problems and print them in the format I use for solving them."
  []
  (map (comp println read-unsolved-problem) unsolved-problems))

;;; ****************************************************************
;;; Solved
;;; ****************************************************************

;;; ****************************************************************
;;; http://www.4clojure.com/problem/82
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
;;; http://www.4clojure.com/problem/110

(def __
  (fn [coll]
    (let [pronunciation (fn [c]
                          (loop [c c
                                 prev-val nil
                                 count 0
                                 p []]
                            (cond (nil? c) (concat p [count prev-val])
                                  (= (first c) prev-val) (recur (next c) prev-val (inc count) p)
                                  (nil? prev-val) (recur (next c) (first c) 1 [])
                                  :else (recur (next c) (first c) 1 (concat p [count prev-val])))))]
      (drop 1 (iterate pronunciation coll))))
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
  (fn [n]
    (let [place-digit (fn [place] (mod (int (/ n place)) 10))
          thousands (place-digit 1000)
          hundreds (place-digit 100)
          tens (place-digit 10)
          ones (place-digit 1)
          to-roman (fn [x ones fives tens]
                     (cond (= x 9) (str ones tens)
                           (>= x 5) (apply str fives (repeat (- x 5) ones))
                           (= x 4) (str ones fives)
                           :else (apply str (repeat x ones)))
                     )]
    (str (to-roman thousands "M" "?" "?")
         (to-roman hundreds "C" "D" "M")
         (to-roman tens "X" "L" "C")
         (to-roman ones "I" "V" "X"))))
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
;;; http://www.4clojure.com/problem/93

(def __
  (fn [coll]
    (let [nseq? (complement sequential?)
          terminal? (fn [x] (or (nseq? x)
                                (every? nseq? x)))]
      (letfn [(mostly-flatten [c]
                (loop [c c
                       answer []]
                  (cond (nil? c) answer
                        (terminal? (first c)) (recur (next c) (conj answer (first c)))
                        :else (recur (concat (mostly-flatten (first c)) (rest c)) answer))))]
        (mostly-flatten coll))))
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
;;; http://www.4clojure.com/problem/137

(def __
  (fn [n radix]
    (reverse (loop [n n
                    digits []]
               (if (zero? n) (if (empty? digits) [0] digits)
                   (recur (int (/ n radix)) (conj digits (mod n radix)))))))
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
;;; http://www.4clojure.com/problem/114

(def __
  (fn [n pred coll]
    (lazy-seq (loop [n (dec n)
                     vals []
                     coll coll]
                (cond (or (and (<= n 0) (pred (first coll))) (nil? coll)) vals
                      (pred (first coll)) (recur (dec n) (conj vals (first coll)) (next coll))
                      :else (recur n (conj vals (first coll)) (next coll))))))
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
;;; http://www.4clojure.com/problem/148

(def __
  (fn [n a b]
    (letfn [(num-factors [n fac]
              (+ (long (/ n fac))
                 (if (zero? (rem n fac)) 0 1)))
            (sum-factors [n fac]
              (let [num-facs (num-factors n fac)]
                (* (/ (* num-facs (dec num-facs)) 2) ; (sum (range num-factors))
                   fac)))]
      (- (+ (sum-factors n a) (sum-factors n b))
         (sum-factors n (* a b)))))
)

(and
 ;; Write a function which calculates the sum of all natural numbers under n
 ;; (first argument) which are evenly divisible by at least one of a and b
 ;; (second and third argument). Numbers a and b are guaranteed to be
 ;; coprimes.
 ;;
 ;; Note: Some test cases have a very large n, so the most obvious solution
 ;; will exceed the time limit.
 (= 0 (__ 3 17 11))
 (= 23 (__ 10 3 5))
 (= 233168 (__ 1000 3 5))
 (= "2333333316666668" (str (__ 100000000 3 5)))
 (= "110389610389889610389610"
    (str (__ (* 10000 10000 10000) 7 11)))
 (= "1277732511922987429116"
    (str (__ (* 10000 10000 10000) 757 809)))
 (= "4530161696788274281"
    (str (__ (* 10000 10000 1000) 1597 3571)))
 )

;;; ****************************************************************
;;; http://www.4clojure.com/problem/116

(def __
  (fn [n]
    (letfn [(prime?
              ;; Returns true if n is prime. Does this by seeing if there is any divisor
              ;; other than 1 in the numbers up to (sqrt n).
              [x]
              (cond (< x 2) false
                    (== x 2) true
                    (even? x) false
                    :else (let [max-divisor-check (/ (- (int (Math/sqrt x)) 2) 2)] ; subtract 2 because we start iterating at 3
                            (not-any? #(zero? (unchecked-remainder x %))
                                      (take max-divisor-check (iterate #(+ % 2) 3))))))
            (prev-prime [n] (first (drop-while #(not (prime? %)) (iterate dec (dec n)))))
            (next-prime [n] (first (drop-while #(not (prime? %)) (iterate inc (inc n)))))]
      (cond (not (prime? n)) false
            (<= n 2) false
            (= n (/ (+ (prev-prime n) (next-prime n)) 2)) true
            :else false)))
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
;;; http://www.4clojure.com/problem/144

(def __
  (fn [val & fs]
    (let [fs (concat (list identity) (flatten (repeat fs)))]
      (letfn [(f [val fs] (lazy-seq (let [new-val ((first fs) val)]
                                      (cons new-val (f new-val (next fs))))))]
        (f val fs))))
  )

(and
 ;; Write an oscillating iterate: a function that takes an initial value and
 ;; a variable number of functions. It should return a lazy sequence of the
 ;; functions applied to the value in order, restarting from the first
 ;; function after it hits the end.
 (= (take 3 (__ 3.14 int double)) [3.14 3 3.0])
 (= (take 5 (__ 3 #(- % 3) #(+ 5 %))) [3 0 5 2 7])
 (= (take 12 (__ 0 inc dec inc dec inc)) [0 1 0 1 0 1 2 1 2 1 2 3])
 )

;;; ****************************************************************
;;; http://www.4clojure.com/problem/92

(def __
  (fn [s]
    (let [vals {\M 1000, \D 500, \C 100, \L 50, \X 10, \V 5, \I 1}]
      (loop [s (seq s)
             prev-dig 0
             digs []]
        (cond (nil? s) (reduce + digs)
              :else (let [dig (get vals (first s))]
                      (recur (next s)
                             dig
                             (conj digs (cond (= dig prev-dig) dig
                                                    (> dig prev-dig) (+ dig (- (* prev-dig 2)))
                                                    (< dig prev-dig) dig))))))))
  )

(and
 ;; Roman numerals are easy to recognize, but not everyone knows all the
 ;; rules necessary to work with them. Write a function to parse a
 ;; Roman-numeral string and return the number it represents.
 ;;
 ;; You can assume that the input will be well-formed, in upper-case, and
 ;; follow the <a
 ;; href="http://en.wikipedia.org/wiki/Roman_numerals#Subtractive_principle">subtractive
 ;; principle</a>. You don't need to handle any numbers greater than
 ;; MMMCMXCIX (3999), the largest number representable with ordinary
 ;; letters.
 (= 14 (__ "XIV"))
 (= 827 (__ "DCCCXXVII"))
 (= 3999 (__ "MMMCMXCIX"))
 (= 48 (__ "XLVIII"))
 )

;;; ****************************************************************
;;; http://www.4clojure.com/problem/108

;; loop
;; - find min value in all sequences
;; - if all other seqs have same value at front, done
;; - else they are all higher, so strip min value from min value sequence

(def __
  (fn [& seqs]
    (letfn [(all-equal-at-front [seqs] (apply = (map first seqs)))
            (find-min [coll] ; return index of seq with min value, along with min value
              (apply min-key (comp min second)
                     (map-indexed #(list %1 (first %2)) coll)))
            (remove-first-from-nth-seq [seqs idx]
              (map-indexed #(if (= %1 idx) (next %2) %2) seqs))]
      (loop [seqs seqs]
        (if (all-equal-at-front seqs)
          (first (first seqs))
          (let [[idx _] (find-min seqs)]
            (recur (remove-first-from-nth-seq seqs idx)))))))
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
;;; http://www.4clojure.com/problem/132

(def __
  (fn [pred interject-me coll]
    (letfn [(lazy-seq-interject [f prev-val coll]
              (if (empty? coll) nil
                  (let [next-val (first coll)]
                    (lazy-seq (if (and f (f prev-val next-val))
                                (cons interject-me (lazy-seq-interject nil prev-val coll))
                                (cons next-val (lazy-seq-interject pred next-val (next coll))))))))]
      (if (empty? coll) ()
          (concat (list (first coll))
                  (lazy-seq-interject pred (first coll) (next coll))))))
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
  (fn [n coll]
    (letfn [(index-combinations
              [n cnt]
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
            (combinations
              [items n]      
              (let [v-items (vec (reverse items))]
                (if (zero? n) (list ())
                    (let [cnt (count items)]
                      (cond (> n cnt) nil
                            (= n cnt) (list (seq items))
                            :else
                            (map #(map v-items %) (index-combinations n cnt)))))))]
      (into #{} (map #(into #{} %) (combinations coll n)))))
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

;;; ****************************************************************
;;; http://www.4clojure.com/problem/131

(def __
  (fn [& sets]
    (letfn [;; Borrowing from clojure.contrib.combinatorics
            (index-combinations [n cnt]
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
                            (map #(map v-items %) (index-combinations n cnt)))))))
            (subsets [items]
              (mapcat (fn [n] (combinations items n))
                      (range (inc (count items)))))
            ;; Borrowing from clojure.set
            (bubble-max-key [k coll]
              (let [max (apply max-key k coll)]
                (cons max (remove #(identical? max %) coll))))
            (intersection
              ([s1] s1)
              ([s1 s2]
                 (if (< (count s2) (count s1))
                   (recur s2 s1)
                   (reduce (fn [result item]
                             (if (contains? s2 item)
                               result
                               (disj result item)))
                           s1 s1)))
              ([s1 s2 & sets] 
                 (let [bubbled-sets (bubble-max-key #(- (count %)) (conj sets s2 s1))]
                   (reduce intersection (first bubbled-sets) (rest bubbled-sets)))))
            ;; Now for some functions of our own
            (sums [s] (map #(apply + %) (filter not-empty (subsets s))))]
      (not (empty? (apply intersection
                          (map #(into #{} (sums %)) sets))))))
  )

(and
 ;; Given a variable number of sets of integers, create a function which
 ;; returns true iff all of the sets have a non-empty subset with an
 ;; equivalent summation.
 (= true  (__ #{-1 1 99}
              #{-2 2 888}
              #{-3 3 7777})) ; ex. all sets have a subset which sums to zero
 (= false (__ #{1}
              #{2}
              #{3}
              #{4}))
 (= true  (__ #{1}))
 (= false (__ #{1 -3 51 9}
              #{0}
              #{9 2 81 33}))
 (= true  (__ #{1 3 5}
              #{9 11 4}
              #{-3 12 3}
              #{-3 4 -2 10}))
 (= false (__ #{-1 -2 -3 -4 -5 -6}
              #{1 2 3 4 5 6 7 8 9}))
 (= true  (__ #{1 3 5 7}
              #{2 4 6 8}))
 (= true  (__ #{-1 3 -5 7 -9 11 -13 15}
              #{1 -3 5 -7 9 -11 13 -15}
              #{1 -1 2 -2 4 -4 8 -8}))
 (= true  (__ #{-10 9 -8 7 -6 5 -4 3 -2 1}
              #{10 -9 8 -7 6 -5 4 -3 2 -1}))
 )

;;; ****************************************************************
;;; http://www.4clojure.com/problem/121

(def __
  (fn [formula]
    (fn [bindings]
      (letfn [(f [expr]
                (cond (coll? expr) (let [sym (first expr)
                                         args (next expr)]
                                     (cond (= sym '+) (apply + (map f args))
                                           (= sym '-) (apply - (map f args))
                                           (= sym '*) (apply * (map f args))
                                           (= sym '/) (apply / (map f args))))
                      (get bindings expr) (get bindings expr)
                      :else (int expr)))]
        (f formula))))
  )

(and
 ;; Given a mathematical formula in prefix notation, return a function that
 ;; calculates the value of the formula. The formula can contain nested
 ;; calculations using the four basic mathematical operators, numeric
 ;; constants, and symbols representing variables. The returned function has
 ;; to accept a single parameter containing the map of variable names to
 ;; their values.
 (= 2 ((__ '(/ a b))
       '{b 8 a 16}))
 (= 8 ((__ '(+ a b 2))
       '{a 2 b 4}))
 (= [6 0 -4]
    (map (__ '(* (+ 2 a)
                 (- 10 b)))
         '[{a 1 b 8}
           {b 5 a -2}
           {a 2 b 11}]))
 (= 1 ((__ '(/ (+ x 2)
               (* 3 (+ y 1))))
       '{x 4 y 1}))
 )

;;; ****************************************************************
;;; Solved, not yet submitted
;;; ****************************************************************

;;; ****************************************************************
;;; Unsolved
;;; ****************************************************************

;;; ****************************************************************
;;; http://www.4clojure.com/problem/125

;; This is a solution, but it's not mine.
(fn []
 (let [a ["(fn [] (let [a "
          "] (apply str (a 0) a (a 1))))"]] (apply str (a 0) a (a 1))))


;; Note: this test won't work in the REPL. You need to literally substitute
;; the text of the function's definition for each "__".
(and
 ;; Create a function of no arguments which returns a string that is an
 ;; exact copy of the function itself.
 ;;
 ;; Hint: read up on Quines if you get stuck (this question is harder than
 ;; it first appears); but it's worth the effort to solve it independently
 ;; if you can!
 ;;
 (= (str '__) (__))
)

;;; ****************************************************************
;;; http://www.4clojure.com/problem/112

;; infinite loop because not lazy
;; need to take from front, not from back

(def __
  (fn [n itree]
    (letfn [(take-flat [n tree]
              (let [all-but-last-node (butlast tree)
                    last-node (last tree)]
                (if (coll? last-node)
                  (assoc (into [] tree) (dec (count tree)) (remove-last-leaf-node last-node))
                  (into [] all-but-last-node))))]
      (lazy-seq
       (loop [itree itree]
         (if (<= (apply + (flatten itree)) n)
           itree
           (recur (remove-last-leaf-node itree)))))))
  )

(and
 ;; Create a function which takes an integer and a nested collection of
 ;; integers as arguments. Analyze the elements of the input collection and
 ;; return a sequence which maintains the nested structure, and which
 ;; includes all elements starting from the head whose sum is less than or
 ;; equal to the input integer.
 (=  (__ 10 [1 2 [3 [4 5] 6] 7])
     '(1 2 (3 (4))))
 (=  (__ 30 [1 2 [3 [4 [5 [6 [7 8]] 9]] 10] 11])
     '(1 2 (3 (4 (5 (6 (7)))))))
 (=  (__ 9 (range))
     '(0 1 2 3))
 (=  (__ 1 [[[[[1]]]]])
     '(((((1))))))
 (=  (__ 0 [1 2 [3 [4 5] 6] 7])
     '())
 (=  (__ 0 [0 0 [0 [0]]])
     '(0 0 (0 (0))))
 (=  (__ 1 [-10 [1 [2 3 [4 5 [6 7 [8]]]]]])
     '(-10 (1 (2 3 (4)))))
 )

;;; ****************************************************************
;;; http://www.4clojure.com/problem/141

(def __
  (fn [trump-suit]
    (fn [cards] (reduce {(:suit (first cards)) 0} ; starting value is suit that was led, 0 face value
                        (fn [best card]
                          (cond (= trump-suit (:suit card)) {:suit trump-suit :rank (max (:rank card) (:rank best))}
                                (= trump-suit (:suit best)) best
                                (= (:suit card) (:suit (first cards))) {:suit (:suit card) :rank (max (:suit card (:suit best)))} ; ??
                                :else (if (> (:rank card) (:rank best))
                                        {:suit (:suit card) :rank (:rank card)}
                                        best)))
                        cards)))
  )

(and
 ;; In trick-taking card games such as bridge, spades, or hearts, cards are
 ;; played in groups known as "tricks" - each player plays a single card, in
 ;; order; the first player is said to "lead" to the trick. After all
 ;; players have played, one card is said to have "won" the trick. How the
 ;; winner is determined will vary by game, but generally the winner is the
 ;; highest card played in the suit that was led. Sometimes (again varying
 ;; by game), a particular suit will be designated "trump", meaning that its
 ;; cards are more powerful than any others: if there is a trump suit, and
 ;; any trumps are played, then the highest trump wins regardless of what
 ;; was led.
 ;;
 ;; Your goal is to devise a function that can determine which of a number
 ;; of cards has won a trick. You should accept a trump suit, and return a
 ;; function winner. Winner will be called on a sequence of cards, and
 ;; should return the one which wins the trick. Cards will be represented in
 ;; the format returned by Problem 128, Recognize Playing Cards: a hash-map
 ;; of :suit and a numeric :rank. Cards with a larger rank are stronger.
 (let [notrump (__ nil)]
   (and (= {:suit :club :rank 9}  (notrump [{:suit :club :rank 4}
                                            {:suit :club :rank 9}]))
        (= {:suit :spade :rank 2} (notrump [{:suit :spade :rank 2}
                                            {:suit :club :rank 10}]))))
 (= {:suit :club :rank 10} ((__ :club) [{:suit :spade :rank 2}
                                        {:suit :club :rank 10}]))
 (= {:suit :heart :rank 8}
    ((__ :heart) [{:suit :heart :rank 6} {:suit :heart :rank 8}
                  {:suit :diamond :rank 10} {:suit :heart :rank 4}]))
 )

;;; ****************************************************************
;;; http://www.4clojure.com/problem/150

(def __
  (fn [n]
    (letfn [(palindrome? [n] (= (str n) (apply str (reverse (str n)))))
            (split-num [n] (let [s (str n)
                                 len (count s)
                                 half (/ len 2)]
                             [(subs s 0 half)
                              (if (even? len) "" (subs s half (inc half)))
                              (subs s (if (odd? len) (inc half) half))]))]
      (let [[beg mid end] (split-num n)
            start-num (if (palindrome? n)
                        n
                        (Integer/parseInt (apply str (concat beg mid (reverse beg)))))]
;; Nope. Turns 1222 into 1221.
        (println "start-num =" start-num))))
  )

(and
 ;; A palindromic number is a number that is the same when written forwards
 ;; or backwards (e.g., 3, 99, 14341).
 ;;
 ;; Write a function which takes an integer n, as its only argument, and
 ;; returns an increasing lazy sequence of all palindromic numbers that are
 ;; not less than n.
 ;; 
 ;; The most simple solution will exceed the time limit!
 (= (take 26 (__ 0))
    [0 1 2 3 4 5 6 7 8 9
     11 22 33 44 55 66 77 88 99
     101 111 121 131 141 151 161])
 (= (take 16 (__ 162))
    [171 181 191 202
     212 222 232 242
     252 262 272 282
     292 303 313 323])
 (= (take 6 (__ 1234550000))
    [1234554321 1234664321 1234774321
     1234884321 1234994321 1235005321])
 (= (first (__ (* 111111111 111111111)))
    (* 111111111 111111111))
 (= (set (take 199 (__ 0)))
    (set (map #(first (__ %)) (range 0 10000))))
 (= true
    (apply < (take 6666 (__ 9999999))))
 (= (nth (__ 0) 10101)
    9102019)
 )

;;; ****************************************************************
;;; Thus endeth the medium problems. Here begin-eth the hard ones.
;;; ****************************************************************

;;; ****************************************************************
;;; http://www.4clojure.com/problem/91

(def __
  (fn [s]
    )
  )

(and
 ;; Given a graph, determine whether the graph is connected. A connected
 ;; graph is such that a path exists between any two given
 ;; nodes.
 ;;
 ;; - Your function must return true if the graph is connected and false
 ;; - otherwise.
 ;;
 ;; -You will be given a set of tuples representing the edges of a graph.
 ;;  Each member of a tuple being a vertex/node in the graph.
 ;;
 ;; - Each edge is undirected (can be traversed either direction).
 (= true (__ #{[:a :a]}))
 (= true (__ #{[:a :b]}))
 (= false (__ #{[1 2] [2 3] [3 1]
               [4 5] [5 6] [6 4]}))
 (= true (__ #{[1 2] [2 3] [3 1]
              [4 5] [5 6] [6 4] [3 4]}))
 (= false (__ #{[:a :b] [:b :c] [:c :d]
               [:x :y] [:d :a] [:b :e]}))
 (= true (__ #{[:a :b] [:b :c] [:c :d]
              [:x :y] [:d :a] [:b :e] [:x :a]})) )

;;; ****************************************************************
;;; http://www.4clojure.com/problem/101

(def __
  (fn [s]
    )
  )

(and
 ;; Given two sequences x and y, calculate the <a
 ;; href="https://secure.wikimedia.org/wikipedia/en/wiki/Levenshtein_distance">Levenshtein
 ;; distance</a> of x and y, i. e. the minimum number of edits needed to
 ;; transform x into y. The allowed edits are:
 ;;
 ;; - insert a single item
 ;; - delete a single item
 ;; - replace a single item with another item
 ;;
 ;; WARNING: Some of the test cases may timeout if you write an inefficient
 ;; solution!
 (= (__ "kitten" "sitting") 3)
 (= (__ "closure" "clojure") (__ "clojure" "closure") 1)
 (= (__ "xyx" "xyyyx") 2)
 (= (__ "" "123456") 6)
 (= (__ "Clojure" "Clojure") (__ "" "") (__ [] []) 0)
 (= (__ [1 2 3 4] [0 2 3 4 5]) 2)
 (= (__ '(:a :b :c :d) '(:a :d)) 2)
 (= (__ "ttttattttctg" "tcaaccctaccat") 10)
 (= (__ "gaattctaatctc" "caaacaaaaaattt") 9) )

;;; ****************************************************************
;;; http://www.4clojure.com/problem/94

(def __
  (fn [s]
    )
  )

(and
 ;; The <a href="http://en.wikipedia.org/wiki/Conway's_Game_of_Life">game of
 ;; life</a> is a cellular automaton devised by mathematician John Conway.
 ;;
 ;; The 'board' consists of both live (#) and dead ( ) cells. Each
 ;; cell interacts with its eight neighbours (horizontal, vertical,
 ;; diagonal), and its next state is dependent on the following
 ;; rules:
 ;;
 ;; 1) Any live cell with fewer than two live neighbours dies, as if caused
 ;; by under-population.
 ;;
 ;; 2) Any live cell with two or three live neighbours lives on to the next
 ;; generation.
 ;;
 ;; 3) Any live cell with more than three live neighbours dies, as if by
 ;; overcrowding.
 ;;
 ;; 4) Any dead cell with exactly three live neighbours becomes a live cell,
 ;; as if by reproduction.
 ;;
 ;; Write a function that accepts a board, and returns a board representing
 ;; the next generation of cells.
 (= (__ ["      "
        " ##   "
        " ##   "
        "   ## "
        "   ## "
        "      "])
   ["      "  
    " ##   "
    " #    "
    "    # "
    "   ## "
    "      "])
 (= (__ ["     "
        "     "
        " ### "
        "     "
        "     "])
   ["     "
    "  #  "
    "  #  "
    "  #  "
    "     "])
 (= (__ ["      "
        "      "
        "  ### "
        " ###  "
        "      "
        "      "])
   ["      "
    "   #  "
    " #  # "
    " #  # "
    "  #   "
    "      "]) )

;;; ****************************************************************
;;; http://www.4clojure.com/problem/106

(def __
  (fn [s]
    )
  )

(and
 ;; Given a pair of numbers, the start and end point, find a path between
 ;; the two using only three possible operations:<ul>
 ;;
 ;; <li>double</li>
 ;;
 ;; <li>halve (odd numbers cannot be halved)</li>
 ;;
 ;; <li>add 2</li></ul>
 ;;
 ;; Find the shortest path through the "maze". Because there are multiple
 ;; shortest paths, you must return the length of the shortest path, not the
 ;; path itself.
 (= 1 (__ 1 1))  ; 1
 (= 3 (__ 3 12)) ; 3 6 12
 (= 3 (__ 12 3)) ; 12 6 3
 (= 3 (__ 5 9))  ; 5 7 9
 (= 9 (__ 9 2))  ; 9 18 20 10 12 6 8 4 2
 (= 5 (__ 9 12)) ; 9 11 22 24 12
 )

;;; ****************************************************************
;;; http://www.4clojure.com/problem/89

(def __
  (fn [s]
    )
  )

(and
 ;; Starting with a graph you must write a function that returns true if it
 ;; is possible to make a tour of the graph in which every edge is visited
 ;; exactly once.
 ;;
 ;; The graph is represented by a vector of tuples, where each tuple
 ;; represents a single edge.
 ;;
 ;; The rules are:
 ;; - You can start at any node.
 ;; - You must visit each edge exactly once.
 ;; - All edges are undirected.
 (= true (__ [[:a :b]]))
 (= false (__ [[:a :a] [:b :b]]))
 (= false (__ [[:a :b] [:a :b] [:a :c] [:c :a]
               [:a :d] [:b :d] [:c :d]]))
 (= true (__ [[1 2] [2 3] [3 4] [4 1]]))
 (= true (__ [[:a :b] [:a :c] [:c :b] [:a :e]
              [:b :e] [:a :d] [:b :d] [:c :e]
              [:d :e] [:c :f] [:d :f]]))
 (= false (__ [[1 2] [2 3] [2 4] [2 5]])) )

;;; ****************************************************************
;;; http://www.4clojure.com/problem/113

(def __
  (fn [& is]
    (loop [is is
           seen #{}
           answer []]
      (let [i (first is)]
        (cond (empty? is) answer
              (some #{i} seen) (recur (next is) seen answer)
              :else (recur (next is) (conj seen i) (conj answer i))))))
  )

(and
 ;; Write a function that takes a variable number of integer arguments. If
 ;; the output is coerced into a string, it should return a comma (and
 ;; space) separated list of the inputs sorted smallest to largest. If the
 ;; output is coerced into a sequence, it should return a seq of unique
 ;; input elements in the same order as they were entered.
 (= "1, 2, 3" (str (__ 2 1 3)))
 (= '(2 1 3) (seq (__ 2 1 3)))
 (= '(2 1 3) (seq (__ 2 1 3 3 1 2)))
 (= '(1) (seq (apply __ (repeat 5 1))))
 (= "1, 1, 1, 1, 1" (str (apply __ (repeat 5 1))))
 (and (= nil (seq (__)))
     (=  "" (str (__)))) )

;;; ****************************************************************
;;; http://www.4clojure.com/problem/117

(def __
  (fn [s]
    )
  )

(and
 ;; A mad scientist with tenure has created an experiment tracking mice in a
 ;; maze. Several mazes have been randomly generated, and you've been tasked
 ;; with writing a program to determine the mazes in which it's possible for
 ;; the mouse to reach the cheesy endpoint. Write a function which accepts a
 ;; maze in the form of a collection of rows, each row is a string where:
 ;;
 ;; - spaces represent areas where the mouse can walk freely
 ;; - hashes (#) represent walls where the mouse can not walk
 ;; - M represents the mouse's starting point
 ;; - C represents the cheese which the mouse must reach
 ;;
 ;; The mouse is not allowed to travel diagonally in the maze (only
 ;; up/down/left/right), nor can he escape the edge of the maze. Your
 ;; function must return true iff the maze is solvable by the mouse.
 (= true (__ ["M C"]))
 (= false (__ ["M # C"]))
 (= true  (__ ["#######"
              "#     #"
              "#  #  #"
              "#M # C#"
              "#######"]))
 (= false (__ ["########"
              "#M  #  #"
              "#   #  #"
              "# # #  #"
              "#   #  #"
              "#  #   #"
              "#  # # #"
              "#  #   #"
              "#  #  C#"
              "########"]))
 (= false (__ ["M     "
              "      "
              "      "
              "      "
              "    ##"
              "    #C"]))
 (= true  (__ ["C######"
              " #     "
              " #   # "
              " #   #M"
              "     # "]))
 (= true  (__ ["C# # # #"
              "        "
              "# # # # "
              "        "
              " # # # #"
              "        "
              "# # # #M"])) )

;;; ****************************************************************
;;; http://www.4clojure.com/problem/119

(def __
  (fn [s]
    )
  )

(and
 ;; <p>As in <a href="/problem/73">Problem 73</a>, a tic-tac-toe board is
 ;; represented by a two dimensional vector. X is represented by :x, O is
 ;; represented by :o, and empty is represented by :e. Create a function
 ;; that accepts a game piece and board as arguments, and returns a set
 ;; (possibly empty) of all valid board placements of the game piece which
 ;; would result in an immediate win.</p>
 ;;
 ;; <p>Board coordinates should be as in calls to <code>get-in</code>. For
 ;; example, <code>[0 1]</code> is the topmost row, center position.</p><br
 ;; />(= (__ :x [[:o :e :e]
           [:o :x :o] 
           [:x :x :e]])
   #{[2 2] [0 1] [0 2]})
 (= (__ :x [[:x :o :o] 
           [:x :x :e] 
           [:e :o :e]])
   #{[2 2] [1 2] [2 0]})
 (= (__ :x [[:x :e :x] 
           [:o :x :o] 
           [:e :o :e]])
   #{[2 2] [0 1] [2 0]})
 (= (__ :x [[:x :x :o] 
           [:e :e :e] 
           [:e :e :e]])
   #{})
 (= (__ :o [[:x :x :o] 
           [:o :e :o] 
           [:x :e :e]])
   #{[2 2] [1 1]}) )

;;; ****************************************************************
;;; http://www.4clojure.com/problem/111

(def __
  (fn [s]
    )
  )

(and
 ;; Write a function that takes a string and a partially-filled crossword
 ;; puzzle board, and determines if the input string can be legally placed
 ;; onto the board.
 ;;
 ;; The crossword puzzle board consists of a collection of partially-filled
 ;; rows. Empty spaces are denoted with an underscore (_), unusable spaces
 ;; are denoted with a hash symbol (#), and pre-filled spaces have a
 ;; character in place; the whitespace characters are for legibility and
 ;; should be ignored.
 ;;
 ;; </br></br>
 ;;
 ;; For a word to be legally placed on the board:
 ;;
 ;; - It may use empty spaces (underscores)
 ;;
 ;; - It may use but must not conflict with any pre-filled characters.
 ;;
 ;; - It must not use any unusable spaces (hashes).
 ;;
 ;; - There must be no empty spaces (underscores) or extra characters before
 ;; - or after the word (the word may be bound by unusable spaces though).
 ;;
 ;; - Characters are not case-sensitive. 
 ;;
 ;; - Words may be placed vertically (proceeding top-down only), or
 ;; - horizontally (proceeding left-right only)
 (= true  (__ "the" ["_ # _ _ e"]))
 (= false (__ "the" ["c _ _ _"
                    "d _ # e"
                    "r y _ _"]))
 (= true  (__ "joy" ["c _ _ _"
                    "d _ # e"
                    "r y _ _"]))
 (= false (__ "joy" ["c o n j"
                    "_ _ y _"
                    "r _ _ #"]))
 (= true  (__ "clojure" ["_ _ _ # j o y"
                        "_ _ o _ _ _ _"
                        "_ _ f _ # _ _"]))
 )

;;; ****************************************************************
;;; http://www.4clojure.com/problem/130

(def __
  (fn [new-root tree]
    )
  )

(and
 ;; Every node of a tree is connected to each of its children as
 ;; well as its parent.  One can imagine grabbing one node of
 ;; a tree and dragging it up to the root position, leaving all
 ;; connections intact.  For example, below on the left is
 ;; a binary tree.  By pulling the "c" node up to the root, we
 ;; obtain the tree on the right.
 ;;
 ;; <img src="http://i.imgur.com/UtD2T.png">
 ;;
 ;; Note it is no longer binary as "c" had three connections
 ;; total -- two children and one parent.
 ;; Each node is represented as a vector, which always has at
 ;; least one element giving the name of the node as a symbol.
 ;; Subsequent items in the vector represent the children of the
 ;; node.  Because the children are ordered it's important that
 ;; the tree you return keeps the children of each node in order
 ;; and that the old parent node, if any, is appended on the
 ;; right.
 ;;
 ;; Your function will be given two args -- the name of the node
 ;; that should become the new root, and the tree to transform.
 (= '(n)
   (__ 'n '(n)))
 (= '(a (t (e)))
   (__ 'a '(t (e) (a))))
 (= '(e (t (a)))
   (__ 'e '(a (t (e)))))
 (= '(a (b (c)))
   (__ 'a '(c (b (a)))))
 (= '(d 
      (b
        (c)
        (e)
        (a 
          (f 
            (g) 
            (h)))))
  (__ 'd '(a
            (b 
              (c) 
              (d) 
              (e))
            (f 
              (g)
              (h)))))
 (= '(c 
      (d) 
      (e) 
      (b
        (f 
          (g) 
          (h))
        (a
          (i
          (j
            (k)
            (l))
          (m
            (n)
            (o))))))
   (__ 'c '(a
             (b
               (c
                 (d)
                 (e))
               (f
                 (g)
                 (h)))
             (i
               (j
                 (k)
                 (l))
               (m
                 (n)
                 (o))))))
 )

;;; ****************************************************************
;;; http://www.4clojure.com/problem/124

(def __
  (fn [s]
    )
  )

(and
 ;; <a href="http://en.wikipedia.org/wiki/Reversi">Reversi</a> is normally
 ;; played on an 8 by 8 board. In this problem, a 4 by 4 board is
 ;; represented as a two-dimensional vector with black, white, and empty
 ;; pieces represented by 'b, 'w, and 'e, respectively. Create a function
 ;; that accepts a game board and color as arguments, and returns a map of
 ;; legal moves for that color. Each key should be the coordinates of a
 ;; legal move, and its value a set of the coordinates of the pieces flipped
 ;; by that move.
 ;;
 ;; Board coordinates should be as in calls to get-in. For example, [0 1] is
 ;; the topmost row, second column from the left.
  (= {[1 3] #{[1 2]}, [0 2] #{[1 2]}, [3 1] #{[2 1]}, [2 0] #{[2 1]}}
   (__ '[[e e e e]
         [e w b e]
         [e b w e]
         [e e e e]] 'w))
 (= {[3 2] #{[2 2]}, [3 0] #{[2 1]}, [1 0] #{[1 1]}}
   (__ '[[e e e e]
         [e w b e]
         [w w w e]
         [e e e e]] 'b))
 (= {[0 3] #{[1 2]}, [1 3] #{[1 2]}, [3 3] #{[2 2]}, [2 3] #{[2 2]}}
   (__ '[[e e e e]
         [e w b e]
         [w w b e]
         [e e b e]] 'w))
 (= {[0 3] #{[2 1] [1 2]}, [1 3] #{[1 2]}, [2 3] #{[2 1] [2 2]}}
   (__ '[[e e w e]
         [b b w e]
         [b w w e]
         [b w w w]] 'b))
 )

;;; ****************************************************************
;;; http://www.4clojure.com/problem/127

(def __
  (fn [s]
    )
  )

(and
 ;; Everyone loves triangles, and it's easy to understand why---they're so
 ;; wonderfully symmetric (except scalenes, they suck).
 ;;
 ;; Your passion for triangles has led you to become a miner (and part-time
 ;; Clojure programmer) where you work all day to chip out isosceles-shaped
 ;; minerals from rocks gathered in a nearby open-pit mine. There are too
 ;; many rocks coming from the mine to harvest them all so you've been
 ;; tasked with writing a program to analyze the mineral patterns of each
 ;; rock, and determine which rocks have the biggest minerals.
 ;;
 ;; Someone has already written a <a
 ;; href="http://en.wikipedia.org/wiki/Computer_vision">computer-vision</a>
 ;; system for the mine. It images each rock as it comes into the processing
 ;; centre and creates a cross-sectional <a
 ;; href="http://en.wikipedia.org/wiki/Bit_array">bitmap</a> of mineral (1)
 ;; and rock (0) concentrations for each one.
 ;;
 ;; You must now create a function which accepts a collection of integers,
 ;; each integer when read in base-2 gives the bit-representation of the
 ;; rock (again, 1s are mineral and 0s are worthless scalene-like rock). You
 ;; must return the cross-sectional area of the largest harvestable mineral
 ;; from the input rock, as follows:
 ;;
 ;; - The minerals only have smooth faces when sheared vertically or
 ;;   horizontally from the rock's cross-section
 ;;
 ;; - The mine is only concerned with harvesting isosceles triangles (such
 ;;   that one or two sides can be sheared)
 ;;
 ;; - If only one face of the mineral is sheared, its opposing vertex must
 ;;   be a point (ie. the smooth face must be of odd length), and its two
 ;;   equal-length sides must intersect the shear face at 45&deg; (ie. those
 ;;   sides must cut even-diagonally)
 ;;
 ;; - The harvested mineral may not contain any traces of rock
 ;;
 ;; - The mineral may lie in any orientation in the plane
 ;;
 ;; - Area should be calculated as the sum of 1s that comprise the mineral
 ;;
 ;; - Minerals must have a minimum of three measures of area to be harvested
 ;;
 ;; - If no minerals can be harvested from the rock, your function should
 ;;   return nil
 ;;
 ;; </ul>
(= 10 (__ [15 15 15 15 15]))
; 1111      1111
; 1111      *111
; 1111  ->  **11
; 1111      ***1
; 1111      ****
 (= 15 (__ [1 3 7 15 31]))
; 00001      0000*
; 00011      000**
; 00111  ->  00***
; 01111      0****
; 11111      *****
 (= 3 (__ [3 3]))
; 11      *1
; 11  ->  **
 (= 4 (__ [7 3]))
; 111      ***
; 011  ->  0*1
 (= 6 (__ [17 22 6 14 22]))
; 10001      10001
; 10110      101*0
; 00110  ->  00**0
; 01110      0***0
; 10110      10110
 (= 9 (__ [18 7 14 14 6 3]))
; 10010      10010
; 00111      001*0
; 01110      01**0
; 01110  ->  0***0
; 00110      00**0
; 00011      000*1
 (= nil (__ [21 10 21 10]))
; 10101      10101
; 01010      01010
; 10101  ->  10101
; 01010      01010
 (= nil (__ [0 31 0 31 0]))
; 00000      00000
; 11111      11111
; 00000  ->  00000
; 11111      11111
; 00000      00000 )

;;; ****************************************************************
;;; http://www.4clojure.com/problem/138

(def __
  (fn [start end]
    (let [square-vals (take-while #(<= % end) (iterate #(* % %) start))
          val-string (apply str square-vals)
          len-val-string (count val-string)
          next-power-of-2-above-length (first (drop-while #(< % len-val-string) (iterate #(* 2 %) 1)))
          num-stars (- next-power-of-2-above-length len-val-string)
          padded-val-string (str val-string (apply str (repeat num-stars "*")))]
      padded-val-string))
  )

(and
 ;; Create a function of two integer arguments: the start and end,
 ;; respectively. You must create a vector of strings which renders a 45
 ;; degree rotated square of integers which are successive squares from the
 ;; start point up to and including the end point. If a number comprises
 ;; multiple digits, wrap them around the shape individually. If there are
 ;; not enough digits to complete the shape, fill in the rest with asterisk
 ;; characters. The direction of the drawing should be clockwise, starting
 ;; from the center of the shape and working outwards, with the initial
 ;; direction being down and to the right.
 (= (__ 2 2) ["2"])
 (= (__ 2 4) [" 2 "
              "* 4"
              " * "])
 (= (__ 3 81) [" 3 "
               "1 9"
               " 8 "])
 (= (__ 4 20) [" 4 "
               "* 1"
               " 6 "])
 (= (__ 2 256) ["  6  "
                " 5 * "
                "2 2 *"
                " 6 4 "
                "  1  "])
 (= (__ 10 10000) ["   0   "
                   "  1 0  "
                   " 0 1 0 "
                   "* 0 0 0"
                   " * 1 * "
                   "  * *  "
                   "   *   "]) )

;;; ****************************************************************
;;; http://www.4clojure.com/problem/140

(defn char-to-power-of-2 [c num-bits]
  (let [diff (- (int c) (int \A))
        pos (dec (- num-bits diff))]
  (int (Math/pow 2 pos))))

(defn sym-to-power-of-2 [sym num-bits]
  (let [c (first (str sym))]
    (if (Character/isUpperCase c)
      (char-to-power-of-2 c num-bits)
      0)))

(defn func-input-value [s]
  "Given a set of input symbols, return the single value that corresponds to
1 bits where symbols are true and 0 bits where symbols are false. Assumes
symbles are in the alphabet #{'a, 'A, 'b, 'B, ...}."
  (let [num-bits (count s)]
    (reduce + (map #(sym-to-power-of-2 % num-bits) s))))

(def gray-codes {2 [0 1 3 2], 3 [0 1 3 2 6 7 5 4], 4 [0 1 3 2 6 7 5 4 12 13 15 14 10 11 9 8]})

;; (defn k-map [ss]
;;   (let [num-bits (count ss)
;;         gray-code (get gray-codes num-bits)
;;         true-inputs (map func-input-value ss)
;;         k-map 
;;     ;; 

(def __
  (fn [s]
    )
  )

(and
 ;; Create a function which accepts as input a boolean algebra function in
 ;; the form of a set of sets, where the inner sets are collections of
 ;; symbols corresponding to the input boolean variables which satisfy the
 ;; function (the inputs of the inner sets are conjoint, and the sets
 ;; themselves are disjoint... also known as canonical minterms). Note:
 ;; capitalized symbols represent truth, and lower-case symbols represent
 ;; negation of the inputs. Your function must return the minimal function
 ;; which is logically equivalent to the input.
 ;;
 ;; PS --- You may want to give this a read before proceeding: <a
 ;; href="http://en.wikipedia.org/wiki/K_map">K-Maps</a>
 ;;
 ;; PPS --- If you're interested in logic programming more generally, you
 ;; should also check out: <a
 ;; href="https://github.com/clojure/core.logic">core.logic</a>
 (= (__ #{#{'a 'B 'C 'd}
         #{'A 'b 'c 'd}
         #{'A 'b 'c 'D}
         #{'A 'b 'C 'd}
         #{'A 'b 'C 'D}
         #{'A 'B 'c 'd}
         #{'A 'B 'c 'D}
         #{'A 'B 'C 'd}})
   #{#{'A 'c} 
     #{'A 'b}
     #{'B 'C 'd}})
 (= (__ #{#{'A 'B 'C 'D}
         #{'A 'B 'C 'd}})
   #{#{'A 'B 'C}})
 (= (__ #{#{'a 'b 'c 'd}
         #{'a 'B 'c 'd}
         #{'a 'b 'c 'D}
         #{'a 'B 'c 'D}
         #{'A 'B 'C 'd}
         #{'A 'B 'C 'D}
         #{'A 'b 'C 'd}
         #{'A 'b 'C 'D}})
   #{#{'a 'c}
     #{'A 'C}})
 (= (__ #{#{'a 'b 'c} 
         #{'a 'B 'c}
         #{'a 'b 'C}
         #{'a 'B 'C}})
   #{#{'a}})
 (= (__ #{#{'a 'B 'c 'd}
         #{'A 'B 'c 'D}
         #{'A 'b 'C 'D}
         #{'a 'b 'c 'D}
         #{'a 'B 'C 'D}
         #{'A 'B 'C 'd}})
   #{#{'a 'B 'c 'd}
     #{'A 'B 'c 'D}
     #{'A 'b 'C 'D}
     #{'a 'b 'c 'D}
     #{'a 'B 'C 'D}
     #{'A 'B 'C 'd}})
 (= (__ #{#{'a 'b 'c 'd}
         #{'a 'B 'c 'd}
         #{'A 'B 'c 'd}
         #{'a 'b 'c 'D}
         #{'a 'B 'c 'D}
         #{'A 'B 'c 'D}})
   #{#{'a 'c}
     #{'B 'c}})
 (= (__ #{#{'a 'B 'c 'd}
         #{'A 'B 'c 'd}
         #{'a 'b 'c 'D}
         #{'a 'b 'C 'D}
         #{'A 'b 'c 'D}
         #{'A 'b 'C 'D}
         #{'a 'B 'C 'd}
         #{'A 'B 'C 'd}})
   #{#{'B 'd}
     #{'b 'D}})
 (= (__ #{#{'a 'b 'c 'd}
         #{'A 'b 'c 'd}
         #{'a 'B 'c 'D}
         #{'A 'B 'c 'D}
         #{'a 'B 'C 'D}
         #{'A 'B 'C 'D}
         #{'a 'b 'C 'd}
         #{'A 'b 'C 'd}})
   #{#{'B 'D}
     #{'b 'd}}) )
