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
         (+ x y))))

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
;;; http://www.4clojure.com/problem/138

(def __
  (fn [start end]
    (let [square-vals (take-while #(<= % end) (iterate #(* % %) start))
          val-string (apply str square-vals)
          len-val-string (count val-string)
          next-square-above-length (first (drop-while #(< % len-val-string) (map #(* % %) (drop 1 (range)))))
          num-stars (- next-square-above-length len-val-string)
          padded-val-string (str val-string (apply str (repeat num-stars "*")))

          square-size (int (Math/sqrt next-square-above-length))
          canvas-size (+ square-size (dec square-size))

          compressed-y-deltas '((2 -1) (4 1) (6 -1) (8 1))
          compressed-x-deltas '((1 1) (3 -1) (5 1) (7 -1))
          uncompress-deltas (fn [cds] (flatten (map #(repeat (first %) (second %)) cds)))
          deltas (fn [] (map #(list %1 %2)
                             (uncompress-deltas compressed-x-deltas)
                             (uncompress-deltas compressed-y-deltas)))

          ;; coordinates for each character in order, starting at (0, 0)
          coords (take (count padded-val-string)
                       (reductions #(list (+ (first %1) (first %2))
                                          (+ (second %1) (second %2)))
                                   '(0 0)
                                   (deltas)))
          x-offset (Math/abs (apply min (map first coords)))
          y-offset (Math/abs (apply min (map second coords)))
          offset-coords (map #(list (+ x-offset (first %)) (+ y-offset (second %))) coords)
          spiral (zipmap offset-coords padded-val-string) ; key = coord, val = char

          paint-on-canvas (fn [canvas col row c]
                            (assoc canvas row
                                   (assoc (nth canvas row) col c)))
          ]
      ;; Turn sprial map {coord1 char1, coord2 char2, ...} into strings
      (loop [keys (keys spiral)
             canvas (into [] (repeat canvas-size (into [] (repeat canvas-size \space))))]
        (if (empty? keys)
          (reverse (map #(apply str %) canvas))
          (let [key (first keys)
                [col row] key]
            (recur (next keys) (paint-on-canvas canvas col row (get spiral key))))))))
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
;;; http://www.4clojure.com/problem/112

(def __
  (fn [n itree]
    (letfn [(remove-last-leaf-node [tree]
              (let [all-but-last-node (butlast tree)
                    last-node (last tree)]
                (if (coll? last-node)
                  (if (empty? last-node)
                    (into [] all-but-last-node)
                    (assoc (into [] tree) (dec (count tree)) (remove-last-leaf-node last-node)))
                  (into [] all-but-last-node))))
            (max-top-level-elements-needed [tree]
              (count (take-while #(<= % n)
                                 (reductions + (flatten tree)))))]
       (loop [itree (take (max-top-level-elements-needed itree) itree)] ; at most n top-level will be used
          (if (<= (apply + (flatten itree)) n)
            itree
            (recur (remove-last-leaf-node itree))))))
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
;;; http://www.4clojure.com/problem/125

;; This solution is not mine.
(fn [] (let [a ["(fn [] (let [a " "] (apply str (a 0) a (a 1))))"]] (apply str (a 0) a (a 1))))

;; Note: this test won't work in the REPL. You need to literally substitute
;; the text of the function's definition for each "__" instead of just
;; assigning the function to a var named "__".
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
;;; http://www.4clojure.com/problem/141

(def __
  (fn [trump-suit]
    (fn [cards]
      (let [eligible-cards (filter #(= (or trump-suit (:suit (first cards))) (:suit %)) cards)]
        (apply max-key :rank eligible-cards))))
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
                              (subs s (if (odd? len) (inc half) half))]))
            (incstr [s] (if (empty? s)
                          "1"
                          (str (inc (read-string s)))))

            (palindrome-after [pal]
              (let [[beg mid end] (split-num pal)
                    all-nines? (fn [str] (empty? (filter #(not= \9 %) str)))]
                (cond (all-nines? beg) (first (filter palindrome? (iterate inc (inc pal))))
                      (empty? mid) (let [s (incstr beg)] (read-string (str s (apply str (reverse s)))))
                      (= "9" mid)  (if (empty? beg)
                                     11
                                     (let [s (incstr beg)] (read-string (str s "0" (apply str (reverse s))))))
                      :else        (read-string (str beg (incstr mid) end)))))
            (lazy-palindrome-generator [pal]
              (let [next-pal (palindrome-after pal)]
                (lazy-seq
                 (cons next-pal (lazy-palindrome-generator next-pal)))))]
      (let [start-num (first (filter palindrome? (iterate inc n)))]
        (lazy-seq
         (cons start-num (lazy-palindrome-generator start-num))))))
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
;;; http://www.4clojure.com/problem/91

(def __
  (fn [edges]
    (letfn [(merge-if-intersection [vertex-partition [v1 v2]]
              (let [v1-partitions (filter #(some #{v1} %) vertex-partition)
                    v2-partitions (filter #(some #{v2} %) vertex-partition)]
                (if (= v1-partitions v2-partitions)
                  (let [joined (into #{} (flatten (map seq v1-partitions)))
                        not-in-joined (for [p vertex-partition :when (not (some p joined))] p)]
                    (into #{} (conj not-in-joined joined))
                    )
                  vertex-partition)))
            (add-edge-to-partition [vertex-partition edge]
              (let [[v1 v2] edge
                    edge-set (into #{} edge)
                    vp-as-seq (flatten (map seq vertex-partition))
                    new-partition (if (or (some #{v1} vp-as-seq)
                                          (some #{v2} vp-as-seq))
                                    (for [s vertex-partition]
                                      (if (some edge-set s)
                                        (conj (conj s v1) v2)
                                        s))
                                    ;; else
                                    (conj vertex-partition edge-set))]
                (merge-if-intersection new-partition edge)))]
      (= 1 (count
            (loop [edges edges
                   vertex-partition []]
              (if (empty? edges)
                vertex-partition
                (recur (next edges) (add-edge-to-partition vertex-partition (first edges)))))))))
  )

(and
 ;; Given a graph, determine whether the graph is connected. A connected
 ;; graph is such that a path exists between any two given
 ;; nodes.
 ;;
 ;; - Your function must return true if the graph is connected and false
 ;;   otherwise.
 ;;
 ;; - You will be given a set of tuples representing the edges of a graph.
 ;;   Each member of a tuple being a vertex/node in the graph.
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
              [:x :y] [:d :a] [:b :e] [:x :a]}))
)

;;; ****************************************************************
;;; http://www.4clojure.com/problem/101

(def __
  (fn [s t]
    (let [m (inc (count s))
          n (inc (count t))
          a (to-array-2d (repeat m (repeat n 0)))]
       (doseq [i (range m)
               j (range n)]
         (cond (zero? j) (aset a i 0 i)
               (zero? i) (aset a 0 j j)
               (= (nth s (dec i)) (nth t (dec j))) (aset a i j (aget a (dec i) (dec j)))
               :else
               (aset a i j (min (inc (aget a (dec i) j))
                                (inc (aget a i (dec j)))
                                (inc (aget a (dec i) (dec j)))))))
       (last (last a))))
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
;;; http://www.4clojure.com/problem/73

(def __
(fn [board]
    (letfn [(sym-at [row col] (nth (nth board row) col))
            (won? [sym]
              (let [sss [sym sym sym]]
                (or (= (nth board 0) sss) ; horizontal
                    (= (nth board 1) sss)
                    (= (nth board 2) sss)
                    (= (map #(nth % 0) board) sss) ; diagonal
                    (= (map #(nth % 1) board) sss)
                    (= (map #(nth % 2) board) sss)
                    (= (list (sym-at 0 0) (sym-at 1 1) (sym-at 2 2)) sss)
                    (= (list (sym-at 0 2) (sym-at 1 1) (sym-at 0 2)) sss))))]
      (cond (won? :x) :x
            (won? :o) :o
            :else nil)))
)

(and
 ;; A tic-tac-toe board is represented by a two dimensional vector. X is
 ;; represented by :x, O is represented by :o, and empty is represented by
 ;; :e. A player wins by placing three Xs or three Os in a horizontal,
 ;; vertical, or diagonal row. Write a function which analyzes a tic-tac-toe
 ;; board and returns :x if X has won, :o if O has won, and nil if neither
 ;; player has won.
 (= nil (__ [[:e :e :e] [:e :e :e] [:e :e :e]]))
 (= :x (__ [[:x :e :o]
            [:x :e :e]
            [:x :e :o]]))
 (= :o (__ [[:e :x :e]
            [:o :o :o]
            [:x :e :x]]))
 (= nil (__ [[:x :e :o]
             [:x :x :e]
             [:o :x :o]]))
 (= :x (__ [[:x :e :e]
            [:o :x :e]
            [:o :e :x]]))
 (= :o (__ [[:x :e :o]
            [:x :o :e]
            [:o :e :x]]))
 (= nil (__ [[:x :o :x]
             [:x :o :x]
             [:o :x :o]])))

;;; ****************************************************************
;;; http://www.4clojure.com/problem/94

(def __
  (fn [board]
    (let [width (count (nth board 0))
          height (count board)]
      (letfn [(cell [row col] (get-in board [(mod row height) (mod col width)]))
              (live? [c] (= \# c))
              (live-neighbors [row col]
                (-
                 (count (for [r (range (dec row) (+ 2 row))
                              c (range (dec col) (+ 2  col))
                              :when (live? (cell r c))]
                          1))
                 (if (live? (cell row col)) 1 0)))]
        ;; Could memoize cell but these boards are small and we're only
        ;; calculating one generation.
        (map #(apply str %)
             (partition width
                        (for [row (range height)
                              col (range width)
                              :let [is-live (live? (cell row col))
                                    ln (live-neighbors row col)]]
                          (if is-live
                            (cond (< ln 2) \space           ; underpopulation
                                  (or (= 2 ln) (= 3 ln)) \# ; continue living
                                  (> ln 3) \space)          ; overpopulation
                            (if (= ln 3)
                              \#        ; birth
                              \space)))))
    )))
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
  (fn [start end]
    ;; Wasteful because we don't avoid loops (double then halve, for
    ;; example). Good enough, though.
    (loop [path-length 1
           ns (list start)]
      (if (some #{end} ns)
        path-length
        (recur (inc path-length)
               (concat (map #(* % 2) ns)
                       (filter integer? (map #(/ % 2) ns))
                       (map #(+ % 2) ns))))))
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
;;; http://www.4clojure.com/problem/113

;; see reify

(def __
  (fn [& is]
    (reify clojure.lang.Seqable
      (toString [this]
        (apply str (interpose ", " (sort is))))
      (seq [this]
        (loop [is is
               seen #{}
               answer []]
          (cond (empty? is) (seq answer)
                (some #{(first is)} seen) (recur (next is) seen answer)
                :else (recur (next is) (conj seen (first is)) (conj answer (first is))))))))
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
      (=  "" (str (__))))
)

;;; ****************************************************************
;;; http://www.4clojure.com/problem/89

;; Eulerian path

(def __
  (fn [s]
    (letfn [(merge-if-intersection [vertex-partition [v1 v2]]
              (let [v1-partitions (filter #(some #{v1} %) vertex-partition)
                    v2-partitions (filter #(some #{v2} %) vertex-partition)]
                (if (= v1-partitions v2-partitions)
                  (let [joined (into #{} (flatten (map seq v1-partitions)))
                        not-in-joined (for [p vertex-partition :when (not (some p joined))] p)]
                    (into #{} (conj not-in-joined joined))
                    )
                  vertex-partition)))
            (add-edge-to-partition [vertex-partition edge]
              (let [[v1 v2] edge
                    edge-set (into #{} edge)
                    vp-as-seq (flatten (map seq vertex-partition))
                    new-partition (if (or (some #{v1} vp-as-seq)
                                          (some #{v2} vp-as-seq))
                                    (for [s vertex-partition]
                                      (if (some edge-set s)
                                        (conj (conj s v1) v2)
                                        s))
                                    ;; else
                                    (conj vertex-partition edge-set))]
                (merge-if-intersection new-partition edge)))
            (connected [edges]
              (= 1 (count
                    (loop [edges edges
                           vertex-partition []]
                      (if (empty? edges)
                        vertex-partition
                        (recur (next edges) (add-edge-to-partition vertex-partition (first edges))))))))]
      (and (connected s) 
           (<= (count (filter odd? (vals (frequencies (flatten s))))) 2))))
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
 (= false (__ [[1 2] [2 3] [2 4] [2 5]]))
)

;;; ****************************************************************
;;; http://www.4clojure.com/problem/117

;; http://www.astrolog.org/labyrnth/algrithm.htm
;;
;; TODO flood fill from mouse. If hit cheese, return true. If fill up with
;; wall and can't fill any furtyer, return false.
;;
;; get mouse loc
;; turn into space
;; flood fill starting at mouse loc
;;
;; algorithm
;;
;; Flood-fill (node, target-color, replacement-color):
;;  1. If the color of node is not equal to target-color, return.
;;  2. Set the color of node to replacement-color.
;;  3. Perform Flood-fill (one step to the west of node, target-color, replacement-color).
;;     Perform Flood-fill (one step to the east of node, target-color, replacement-color).
;;     Perform Flood-fill (one step to the north of node, target-color, replacement-color).
;;     Perform Flood-fill (one step to the south of node, target-color, replacement-color).
;;  4. Return.


(def __
  (fn [maze]
    ;; Assumes maze is simply connected. Flood-fills the maze starting at
    ;; the mouse position. If we hit the cheese, then the mouse can get
    ;; there, too.
    (let [width (count (nth maze 0))
          height (count maze)
          cheese-found (ref false)]
      (letfn [(in-bounds? [row col] (and (>= row 0) (>= col 0) (< row height) (< col width)))
              (cell [row col]           ; return wall if out of bounds
                (if (in-bounds? row col)
                  (get-in maze [row col] \#)))
              (find-in-maze [ch] ; return coords of first cell found containing ch
                (first
                 (for [r (range height), c (range width) :when (= ch (cell r c))]
                   [r c])))
              (flood-fill [row col]     ; look for cheese while flooding
                (let [visited (ref #{})]
                  (letfn [(do-flood-fill [row col]
                            (if (and (not @cheese-found)
                                     (in-bounds? row col)
                                     (nil? (some #{[row col]} @visited)))
                              (let [ch (cell row col)]
                                (cond (= ch \C) (dosync (ref-set cheese-found true)) ; cheese found
                                      (or (= ch \space) (= ch \M)) (do
                                                                     (dosync (alter visited conj [row col]))
                                                                     (do-flood-fill (dec row) col)
                                                                     (do-flood-fill (inc row) col)
                                                                     (do-flood-fill row (dec col))
                                                                     (do-flood-fill row (inc col)))))))]
                    (do-flood-fill row col))))]
                    
        ;; Here we go
        (let [mouse-pos (find-in-maze \M)]
          (flood-fill (first mouse-pos) (second mouse-pos))
          @cheese-found))))
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
              "# # # #M"]))
 )

;;; ****************************************************************
;;; http://www.4clojure.com/problem/119

(def __
  (fn [piece board]
    (letfn [(replace-in-board [piece row col]
              (assoc board row (assoc (nth board row) col piece)))
            (horiz-win? [board row col]
              (or (and (= piece (get-in board [0 0]))
                       (= piece (get-in board [0 1]))
                       (= piece (get-in board [0 2])))
                  (and (= piece (get-in board [1 0]))
                       (= piece (get-in board [1 1]))
                       (= piece (get-in board [1 2])))
                  (and (= piece (get-in board [2 0]))
                       (= piece (get-in board [2 1]))
                       (= piece (get-in board [2 2])))))
            (vert-win? [board row col]
              (or (and (= piece (get-in board [0 0]))
                       (= piece (get-in board [1 0]))
                       (= piece (get-in board [2 0])))
                  (and (= piece (get-in board [0 1]))
                       (= piece (get-in board [1 1]))
                       (= piece (get-in board [2 1])))
                  (and (= piece (get-in board [0 2]))
                       (= piece (get-in board [1 2]))
                       (= piece (get-in board [2 2])))))
            (diag-win? [board row col]
              (or (and (= piece (get-in board [0 0]))
                       (= piece (get-in board [1 1]))
                       (= piece (get-in board [2 2])))
                  (and (= piece (get-in board [0 2]))
                       (= piece (get-in board [1 1]))
                       (= piece (get-in board [2 0])))))
            (won? [row col]
              (let [board (replace-in-board piece row col)]
                (or (horiz-win? board row col)
                    (vert-win? board row col)
                    (diag-win? board row col))))]
                       
      (into #{} (for [row (range 3) col (range 3)
                      :when (and (= :e (get-in board [row col]))
                                 (won? row col))]
                  [row col]))))
  )

(and
 ;; As in <a href="/problem/73">Problem 73</a>, a tic-tac-toe board is
 ;; represented by a two dimensional vector. X is represented by :x, O is
 ;; represented by :o, and empty is represented by :e. Create a function
 ;; that accepts a game piece and board as arguments, and returns a set
 ;; (possibly empty) of all valid board placements of the game piece which
 ;; would result in an immediate win.
 ;;
 ;; Board coordinates should be as in calls to <code>get-in</code>. For
 ;; example, <code>[0 1]</code> is the topmost row, center position.
 (= (__ :x [[:o :e :e]
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
   #{[2 2] [1 1]})
 )

;;; ****************************************************************
;;; http://www.4clojure.com/problem/111

(def __
  (fn [word board]
    (let [word-len (count word)
          height (count board)
          board (into [] (map #(apply str (filter (fn [ch] (not= \space ch)) %)) board)) ; remove spaces
          width (count (nth board 0))]
      (letfn [(in-bounds? [row col] (and (>= row 0) (>= col 0) (< row height) (< col width)))
              (cell [row col]           ; return wall if out of bounds
                (if (in-bounds? row col)
                  (get-in board [row col] \#)
                  \#))
              (fits-across? [row col]
                (and (= \# (cell row (dec col)))
                     (= word-len (count (for [i (range word-len)
                                              :let [ch (cell row (+ col i))]
                                              :when (or (= ch (nth word i))
                                                        (= ch \_))]
                                          1)))
                     (= \# (cell row (+ col word-len)))))
              (fits-down? [row col]
                (and (= \# (cell (dec row) col))
                     (= word-len (count (for [i (range word-len)
                                              :let [ch (cell (+ row i) col)]
                                              :when (or (= ch (nth word i))
                                                        (= ch \_))]
                                          1)))
                     (= \# (cell (+ row word-len) col))))]
        (if (or
             ;; across
             (first (filter true? (for [row (range height)
                                        col (range (inc (- width word-len)))
                                        :when (fits-across? row col)]
                                    true)))
             ;; down
             (first (filter true? (for [row (range (inc (- height word-len)))
                                        col (range width)
                                        :when (fits-down? row col)]
                                    true))))
          true
          false))))
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
    ;; Find target subtree
    ;; Find parent
    ;; Append parent, minus target subtree, to end of target subtree
    ;; Wash, rinse, repeat
    (letfn [(subtree-map [tree]
              (let [ts (tree-seq coll? next tree)]
                (zipmap (map first ts) ts)))
            (parent-tree [node subtrees]
              (first
               (for [k (keys subtrees)
                     :let [subtree (get subtrees k)]
                     :when (some #{node} (map first (next subtree)))]
                 subtree)))
            (make-node-new-root [node subtrees child-subtree i]
              (let [root-subtree (get subtrees node)
                    parent (parent-tree node subtrees)
                    parent-without-root-subtree (remove #(= root-subtree %) parent)]
                (if (empty? parent)
                  child-subtree
                  (concat
                   (list (first root-subtree))
                   (remove #(= new-root (first %)) (next root-subtree))
                   (list (make-node-new-root (first parent) subtrees parent-without-root-subtree (inc i)))))))]
      (let [subtrees (subtree-map tree)]
        (make-node-new-root new-root subtrees (get subtrees new-root) 0))))
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
  (fn [board player]
    (let [width (count (nth board 0))
          height (count board)
          dirs [[1 0] [-1 0] [0 1] [0 -1]
                [1 1] [1 -1] [-1 -1] [-1 1]]
          other-player (if (= player 'w) 'b 'w)]
      (letfn [(next-loc-in-dir [loc dir]
                [(+ (first loc) (first dir))
                 (+ (second loc) (second dir))])
              (legal-move [start-loc dir]
                (loop [loc (next-loc-in-dir start-loc dir)
                       captured #{}]
                  (let [cell (get-in board loc nil)]
                    (cond (= player cell) (if (empty? captured) [nil nil] [start-loc captured])
                          (= other-player cell) (recur (next-loc-in-dir loc dir) (conj captured loc))
                          :else [nil nil]))))]
        (let [moves (for [row (range height)
                          col (range width)
                          dir dirs
                          :when (= 'e (get-in board [row col]))
                          :let [[move captured] (legal-move [row col] dir)]
                          :when move]
                      [move captured])]
          (zipmap (map first moves) (map second moves))))))
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
;;; http://www.4clojure.com/problem/157

(def __
     (fn [coll]
       (map #(list %1 %2) coll (range))))

(and
 (= (__ [:a :b :c]) [[:a 0] [:b 1] [:c 2]])
 (= (__ [0 1 3]) '((0 0) (1 1) (3 2)))
 (= (__ [[:foo] {:bar :baz}]) [[[:foo] 0] [{:bar :baz} 1]]))

;;; ****************************************************************
;;; http://www.4clojure.com/problem/162

(def __ 1)

(and
 (= __ (if-not false 1 0))
 (= __ (if-not nil 1 0))
 (= __ (if true 1 0))
 (= __ (if [] 1 0))
 (= __ (if [0] 1 0))
 (= __ (if 0 1 0))
 (= __ (if 1 1 0))
 )

;;; ****************************************************************
;;; http://www.4clojure.com/problem/161

;; Set A is a subset of set B, or equivalently B is a superset of A, if A is
;; "contained" inside B. A and B may coincide.

(def __ #{1 2})

(and
 (clojure.set/superset? __ #{2})
 (clojure.set/subset? #{1} __)
 (clojure.set/superset? __ #{1 2})
 (clojure.set/subset? #{1 2} __)
 )

;;; ****************************************************************
;;; http://www.4clojure.com/problem/156

;; When retrieving values from a map, you can specify default values in case
;; the key is not found:
;;
;; (= 2 (:foo {:bar 0, :baz 1} 2))
;;
;; However, what if you want the map itself to contain the default values?
;; Write a function which takes a default value and a sequence of keys and
;; constructs a map.

(def __
  (fn [default keys]
    (apply hash-map (mapcat #(list %1 %2) keys (repeat default)))
    )
  )

(and
 (= (__ 0 [:a :b :c]) {:a 0 :b 0 :c 0})
 (= (__ "x" [1 2 3]) {1 "x" 2 "x" 3 "x"})
 (= (__ [:a :b] [:foo :bar]) {:foo [:a :b] :bar [:a :b]})
 )

;;; ****************************************************************
;;; http://www.4clojure.com/problem/166

;; For any orderable data type it's possible to derive all of the basic
;; comparison operations (<, ≤, =, ≠, ≥, and >) from a single operation (any
;; operator but = or ≠ will work). Write a function that takes three
;; arguments, a less than operator for the data and two items to compare.
;; The function should return a keyword describing the relationship between
;; the two items. The keywords for the relationship between x and y are as
;; follows:
;;
;; x = y -> :eq
;; x > y -> :gt
;; x < y -> :lt


(def __
     (fn [lt_op x y]
       (cond (lt_op x y) :lt
             (lt_op y x) :gt
             :else :eq)
    )
  )

(and
 (= :gt (__ < 5 1))
 (= :eq (__ (fn [x y] (< (count x) (count y))) "pear" "plum"))
 (= :lt (__ (fn [x y] (< (mod x 5) (mod y 5))) 21 3))
 (= :gt (__ > 0 2))
 )

;;; ****************************************************************
;;; http://www.4clojure.com/problem/153

;; Given a set of sets, create a function which returns true if no two of
;; those sets have any elements in common(1) and false otherwise. Some of
;; the test cases are a bit tricky, so pay a little more attention to them.
;;
;; (1) Such sets are usually called pairwise disjoint or mutually disjoint.

(def __
  (fn [coll]
    (zero? (count (filter #(> % 1) (vals (frequencies (reduce concat coll))))))
    )
  )

(and
 (= (__ #{#{\U} #{\s} #{\e \R \E} #{\P \L} #{\.}})
    true)
 (= (__ #{#{:a :b :c :d :e}
          #{:a :b :c :d}
          #{:a :b :c}
          #{:a :b}
          #{:a}})
    false)
 (= (__ #{#{[1 2 3] [4 5]}
          #{[1 2] [3 4 5]}
          #{[1] [2] 3 4 5}
          #{1 2 [3 4] [5]}})
    true)
 (= (__ #{#{'a 'b}
          #{'c 'd 'e}
          #{'f 'g 'h 'i}
          #{''a ''c ''f}})
    true)
 (= (__ #{#{'(:x :y :z) '(:x :y) '(:z) '()}
          #{#{:x :y :z} #{:x :y} #{:z} #{}}
          #{'[:x :y :z] [:x :y] [:z] [] {}}})
    false)
 (= (__ #{#{(= "true") false}
          #{:yes :no}
          #{(class 1) 0}
          #{(symbol "true") 'false}
          #{(keyword "yes") ::no}
          #{(class '1) (int \0)}})
    false)
 (= (__ #{#{distinct?}
          #{#(-> %) #(-> %)}
          #{#(-> %) #(-> %) #(-> %)}
          #{#(-> %) #(-> %) #(-> %)}})
    true)
 (= (__ #{#{(#(-> *)) + (quote mapcat) #_ nil}
          #{'+ '* mapcat (comment mapcat)}
          #{(do) set contains? nil?}
          #{, , , #_, , empty?}})
    false)
 )

;;; ****************************************************************
;;; http://www.4clojure.com/problem/158

;; Write a function that accepts a curried function of unknown arity n.
;; Return an equivalent function of n arguments.
;; 
;; You may wish to read http://en.wikipedia.org/wiki/Currying

(def __
  (fn [f]
    (fn [& args] (reduce #(%1 %2) f args))
    )
  )

(and
 (= 10 ((__ (fn [a]
              (fn [b]
                (fn [c]
                  (fn [d]
                    (+ a b c d))))))
        1 2 3 4))
 (= 24 ((__ (fn [a]
              (fn [b]
                (fn [c]
                  (fn [d]
                    (* a b c d))))))
        1 2 3 4))
 (= 25 ((__ (fn [a]
              (fn [b]
                (* a b))))
        5 5))
 )

;;; ****************************************************************
;;; http://www.4clojure.com/problem/168

;; In mathematics, the function f can be interpreted as an infinite matrix
;; with infinitely many rows and columns that, when written, looks like an
;; ordinary matrix but its rows and columns cannot be written down
;; completely, so are terminated with ellipses. In Clojure, such infinite
;; matrix can be represented as an infinite lazy sequence of infinite lazy
;; sequences, where the inner sequences represent rows.
;;
;; Write a function that accepts 1, 3 and 5 arguments
;;
;; * with the argument f, it returns the infinite matrix A that has the
;;   entry in the i-th row and the j-th column equal to f(i,j) for i,j =
;;   0,1,2,...;
;;
;; * with the arguments f, m, n, it returns the infinite matrix B that
;;   equals the remainder of the matrix A after the removal of the first m
;;   rows and the first n columns;
;;
;; * with the arguments f, m, n, s, t, it returns the finite s-by-t matrix
;;   that consists of the first t entries of each of the first s rows of the
;;   matrix B or, equivalently, that consists of the first s entries of each
;;   of the first t columns of the matrix B.
;;
;; NOTE: may not use for, range, iterate, repeat, cycle, drop

;; TODO iterate is bad

(def __
  (fn inf-matrix
    ([f] (inf-matrix f 0 0))
    ([f m n] (inf-matrix f m n nil nil)) ; i,j == f(i+m, j+n)
    ([f m n s t]                    ; i,j == f(i+m, j+n), finite size (s, t)
       ;; This is probably cheating...
       (letfn [(my-iterate [f x] (cons x (lazy-seq (my-iterate f (f x)))))]
         (let [matrix (map
                       (fn [row-index]
                         (let [row (map #(f (+ row-index m) (+ % n)) (my-iterate inc 0))]
                           (if t
                             (take t row)
                             row)))
                       (my-iterate inc 0))]
           (if s
             (take s matrix)
             matrix)))))
  )

(and
 (= (take 5 (map #(take 6 %) (__ str)))
    [["00" "01" "02" "03" "04" "05"]
     ["10" "11" "12" "13" "14" "15"]
     ["20" "21" "22" "23" "24" "25"]
     ["30" "31" "32" "33" "34" "35"]
     ["40" "41" "42" "43" "44" "45"]])
 (= (take 6 (map #(take 5 %) (__ str 3 2)))
    [["32" "33" "34" "35" "36"]
     ["42" "43" "44" "45" "46"]
     ["52" "53" "54" "55" "56"]
     ["62" "63" "64" "65" "66"]
     ["72" "73" "74" "75" "76"]
     ["82" "83" "84" "85" "86"]])
 (= (__ * 3 5 5 7)
    [[15 18 21 24 27 30 33]
     [20 24 28 32 36 40 44]
     [25 30 35 40 45 50 55]
     [30 36 42 48 54 60 66]
     [35 42 49 56 63 70 77]])
 (= (__ #(/ % (inc %2)) 1 0 6 4)
    [[1/1 1/2 1/3 1/4]
     [2/1 2/2 2/3 1/2]
     [3/1 3/2 3/3 3/4]
     [4/1 4/2 4/3 4/4]
     [5/1 5/2 5/3 5/4]
     [6/1 6/2 6/3 6/4]])
 (= (class (__ (juxt bit-or bit-xor)))
    (class (__ (juxt quot mod) 13 21))
    (class (lazy-seq)))
 (= (class (nth (__ (constantly 10946)) 34))
    (class (nth (__ (constantly 0) 5 8) 55))
    (class (lazy-seq)))
 (= (let [m 377 n 610 w 987
          check (fn [f s] (every? true? (map-indexed f s)))
          row (take w (nth (__ vector) m))
          column (take w (map first (__ vector m n)))
          diagonal (map-indexed #(nth %2 %) (__ vector m n w w))]
      (and (check #(= %2 [m %]) row)
           (check #(= %2 [(+ m %) n]) column)
           (check #(= %2 [(+ m %) (+ n %)]) diagonal)))
    true)
 )

;;; ****************************************************************
;;; Solved, not yet submitted
;;; ****************************************************************

;;; ****************************************************************
;;; Unsolved
;;; ****************************************************************

;;; ****************************************************************
;;; http://www.4clojure.com/problem/127

(def __
  (fn [matrix]
    ;; We start with any mineral piece and call it the point of a triangle.
    ;; From there, try to form cardinal and 45deg triangles (defined below).
    ;; When the largest possible one is found, it is removed and we try
    ;; again until there are no remaining triangles to be found. Note: we
    ;; don't attempt to try to solve the packing problem; we just find the
    ;; largest, then the next largest, etc. Given the sample data this seems
    ;; adequate.
    ;;
    ;; A "cardinal" triangle is one that radiates from a point in a N, S, E,
    ;; or W direction. Line lengths from the tip to the opposite side grows
    ;; in length by two.
    ;;
    ;; A "45deg" triangle is one that radiates from a point in a NE, NW, SE,
    ;; or SW direction in one of two orientations. One side runs N/S or E/W.
    ;; Line lengths from the tip to the opposite side growsn in length by
    ;; one. There are eight growth possibilities: left and up, left and
    ;; down, right and up, right and down, up and left, up and right, down
    ;; and left, down and right.
    ;;
    ;; Note: all row, col, and line numbers are 0-based.
    (let [matrix-chars (into [] (map #(apply str ; convert number to binary string
                                             (loop [n %, s ()]
                                               (if (zero? n)
                                                 s
                                                 (recur (bit-shift-right n 1) (conj s (if (even? n) \0 \1))))))
                                     matrix))
          width (apply max (map count matrix-chars))
          height (count matrix)]
      (letfn [(in-bounds? [row col] (and (>= row 0) (>= col 0) (< row height) (< col width)))
              (cell [matrix-chars row col]
                ;; Return char at [row col], or nil if out of bounds
                (and (in-bounds? row col)
                     (nth (nth matrix-chars row) col)))
              (all-mineral? [matrix-chars line-start line-length row-cell-func]
                ;; Return true if line-length cells starting at [row col]
                ;; are all minerals. Coordinates in the line are calculated
                ;; by row-cell-func, which takes the previous coord and
                ;; returns the next one.
                (loop [i line-length, cell-loc line-start]
                  (let [[row col] cell-loc]
                    (cond (zero? i) true
                          (not= \1 (cell matrix-chars row col)) false
                          :else (recur (dec i) (row-cell-func cell-loc))))))
              (area-from-num-lines [n line-length-func]
                ;; Given line number 0-(n-1), line-length-func should return
                ;; length of line.
                (apply + (map line-length-func (range n))))
              (biggest-triangle [matrix-chars row col row-start-func row-cell-func line-length-func]
                ;; row-start-func takes [row col] pair and line number (0-based) and returns row start coord.
                ;; row-cell-func takes prev cell in line and returns next row in line
                ;; line-length-func returns length of nth row (0-based)
                ;; Returns map with keys :start, :gen-funcs, :area, and :num-lines
                (let [num-lines (loop [num-lines 1]
                                  (cond (all-mineral? matrix-chars
                                                      (row-start-func [row col] (dec num-lines))
                                                      (line-length-func (dec num-lines))
                                                      row-cell-func)
                                          (recur (inc num-lines))
                                        :else num-lines))]
                {:start [row col]
                 :gen-funcs [row-start-func row-cell-func line-length-func]
                 :num-lines num-lines
                 :area (area-from-num-lines num-lines line-length-func)}))
              (remove-from-matrix [matrix-chars triangle]
                ;; Remove triangle from matrix, replacing it with \0 chars
                (println "remove-from-matrix") ; DEBUG
                (println "  matrix-chars" matrix-chars) ; DEBUG
                (println "  triangle" (merge triangle {:gen-funcs nil})) ; DEBUG
;; FIXME
                ;; Triangle is a map with keys :start, :gen-funcs, :num-lines, and :area
                (let [[row col] (:start triangle)
                      [row-start-func row-cell-func line-length-func] (:gen-funcs triangle)
                      num-lines (:num-lines triangle)
                      r-matrix-chars (ref matrix-chars)
                      replace-cell (fn [m-chars row col]
                                     (println "replace-cell m-chars" m-chars "row" row "col" col) ; DEBUG
                                     ;; Replace char at [row col] with \0
                                     (let [row-chars (nth m-chars row)
                                           new-row (str (subs row-chars 0 col) \0 (subs row-chars (inc col)))]
                                       (assoc m-chars row new-row)))]
                  (loop [line 1]
                    ;; loop over each line
                    (println "  line" line "num-lines" num-lines) ; DEBUG
                    (if (> line num-lines)
                      @r-matrix-chars
                      (recur
                       (let [[row col] (row-start-func [row col] (dec line))]
                         (loop [i (line-length-func (dec line))
                                cell-loc [row col]]
                           ;; loop over each char in the line
                           (cond (zero? i) (inc line)
                                 :else (let [[row col] cell-loc]
                                         (dosync
                                          (ref-set r-matrix-chars (replace-cell @r-matrix-chars row col)))
                                         (recur (dec i) (row-cell-func cell-loc)))))))))))]
        (let [ ;; Pairs of functions. First returns starting coord of row
              ;; n (0-based). Second returns next coord in row given
              ;; previous coord.
              cardinal-funcs [[(fn [[r c] n] [(+ r n) (- c n)]) (fn [[r c]] [r (inc c)])]
                              [(fn [[r c] n] [(- r n) (- c n)]) (fn [[r c]] [r (inc c)])]
                              [(fn [[r c] n] [(+ r n) (+ c n)]) (fn [[r c]] [(inc r) c])]
                              [(fn [[r c] n] [(+ r n) (- c n)]) (fn [[r c]] [(inc r) c])]]
              t45deg-funcs [[(fn [[r c] n] [(+ r n)       c]) (fn [[r c]] [r (dec c)])]  ; up left
                            [(fn [[r c] n] [(+ r n)       c]) (fn [[r c]] [r (inc c)])]  ; up right
                            [(fn [[r c] n] [      r (+ c n)]) (fn [[r c]] [(inc r) c])]  ; right up
                            [(fn [[r c] n] [      r (+ c n)]) (fn [[r c]] [(dec r) c])]  ; right down
                            [(fn [[r c] n] [(- r n)       c]) (fn [[r c]] [r (inc c)])]  ; down right
                            [(fn [[r c] n] [(- r n)       c]) (fn [[r c]] [r (dec c)])]  ; down left
                            [(fn [[r c] n] [      r (- c n)]) (fn [[r c]] [(dec r) c])]  ; left down
                            [(fn [[r c] n] [      r (- c n)]) (fn [[r c]] [(inc r) c])]]] ; left up
          (loop [matrix-chars matrix-chars, area-so-far 0]
            (println "matrix-chars" matrix-chars) ; DEBUG
            (let [all-biggest-triangles (flatten (concat
                                                  (for [row (range height), col (range width)] ; cardinal triangles
                                                    (map (fn [func-pair] (biggest-triangle matrix-chars row col (first func-pair) (second func-pair) #(inc (* (dec %) 2))))
                                                         cardinal-funcs))
                                                  (for [row (range height), col (range width)] ; 45deg triangles
                                                    (map (fn [func-pair] (biggest-triangle matrix-chars row col (first func-pair) (second func-pair) inc))
                                                         t45deg-funcs))))
                  biggest-tri (apply max-key :area all-biggest-triangles)
                  biggest-area (:area biggest-tri)]
              (if (< biggest-area 3)
                area-so-far
                (recur (remove-from-matrix matrix-chars biggest-tri) (+ area-so-far biggest-area)))))))))
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
;;  (= 15 (__ [1 3 7 15 31]))
;; ; 00001      0000*
;; ; 00011      000**
;; ; 00111  ->  00***
;; ; 01111      0****
;; ; 11111      *****
;;  (= 3 (__ [3 3]))
;; ; 11      *1
;; ; 11  ->  **
;;  (= 4 (__ [7 3]))
;; ; 111      ***
;; ; 011  ->  0*1
;;  (= 6 (__ [17 22 6 14 22]))
;; ; 10001      10001
;; ; 10110      101*0
;; ; 00110  ->  00**0
;; ; 01110      0***0
;; ; 10110      10110
;;  (= 9 (__ [18 7 14 14 6 3]))
;; ; 10010      10010
;; ; 00111      001*0
;; ; 01110      01**0
;; ; 01110  ->  0***0
;; ; 00110      00**0
;; ; 00011      000*1
;;  (= nil (__ [21 10 21 10]))
;; ; 10101      10101
;; ; 01010      01010
;; ; 10101  ->  10101
;; ; 01010      01010
;;  (= nil (__ [0 31 0 31 0]))
;; ; 00000      00000
;; ; 11111      11111
;; ; 00000  ->  00000
;; ; 11111      11111
;; ; 00000      00000
 )

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
symbols are in the alphabet #{'a, 'A, 'b, 'B, ...}."
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

;;; ****************************************************************
;;; http://www.4clojure.com/problem/164

;; A deterministic finite automaton (DFA)
;; (http://en.wikipedia.org/wiki/Deterministic_finite_automaton) is an
;; abstract machine that recognizes a regular language
;; (http://en.wikipedia.org/wiki/Regular_language). Usually a DFA is defined
;; by a 5-tuple, but instead we'll use a map with 5 keys:
;; 
;; :states is the set of states for the DFA.
;; :alphabet is the set of symbols included in the language recognized by the DFA.
;; :start is the start state of the DFA.
;; :accepts is the set of accept states in the DFA.
;; :transitions is the transition function for the DFA, mapping :states X :alphabet onto :states.
;;
;; Write a function that takes as input a DFA definition (as described
;; above) and returns a sequence enumerating all strings in the language
;; recognized by the DFA. Note: Although the DFA itself is finite and only
;; recognizes finite-length strings it can still recognize an infinite set
;; of finite-length strings. And because stack space is finite, make sure
;; you don't get stuck in an infinite loop that's not producing results
;; every so often!

;;; Correct, but blows stack.
(def __
  (fn [{:keys [states alphabet start accepts transitions]}]
    (letfn [(next-state [sym curr-state] (get-in transitions (list curr-state sym)))
            (build-accepted
             [prefix state]
             (for [s (shuffle alphabet) ; randomness avoids infinite depth-first searches
                   :let [new-s (str prefix s)
                         new-state (next-state s state)]]
               (cond (accepts new-state) (lazy-seq (cons new-s (build-accepted new-s new-state)))
                     new-state (lazy-seq (build-accepted new-s new-state)))))]
      (filter identity (flatten (build-accepted "" start)))))
)

;;; Correct, faster, and uses less stack, but still blows stack with dfa6.
;;;
;;; queue contains {:state state :prefix vector-of-chars-that-got-us-here}
(def __
  (fn [{:keys [states alphabet start accepts transitions]}]
    (letfn [(next-state [char curr-state]
                        (get-in transitions (list curr-state char)))
            (build-queue [state prefix-chars] {:state state :prefix prefix-chars})
            (word [chars char] (str (apply str chars) char))
            (build-accepted
             [{:keys [state prefix]}]
             (for [ch alphabet
                   :let [new-state (next-state ch state)]]
               (cond (accepts new-state) (let [s (conj prefix ch)]
                                           (lazy-seq (cons (apply str s) (build-accepted (build-queue new-state s)))))
                     new-state (lazy-seq (build-accepted (build-queue new-state (conj prefix ch)))))))]
      (filter identity (flatten (build-accepted (build-queue start []))))))
)


(def dfa1 '{:states #{q0 q1 q2 q3}
            :alphabet #{a b c}
            :start q0
            :accepts #{q1 q2 q3}
            :transitions {q0 {a q1}
                          q1 {b q2}
                          q2 {c q3}}})

(def dfa2 '{:states #{q0 q1 q2 q3 q4 q5 q6 q7}
            :alphabet #{e h i l o y}
            :start q0
            :accepts #{q2 q4 q7}
            :transitions {q0 {h q1}
                             q1 {i q2, e q3}
                             q3 {l q5, y q4}
                             q5 {l q6}
                             q6 {o q7}}})

(def dfa3 '{:states #{q0 q1 q2 q3 q4}
            :alphabet #{v w x y z}
            :start q0
            :accepts #{q4}
            :transitions {q0 {v q1, w q1, x q1, y q1, z q1}
                             q1 {v q2, w q2, x q2, y q2, z q2}
                             q2 {v q3, w q3, x q3, y q3, z q3}
                             q3 {v q4, w q4, x q4, y q4, z q4}}})

(def dfa4 '{:states #{q0 q1}
            :alphabet #{0 1}
            :start q0
            :accepts #{q0}
            :transitions {q0 {0 q0, 1 q1}
                          q1 {0 q1, 1 q0}}})

(def dfa5 '{:states #{q0 q1}
            :alphabet #{n m}
            :start q0
            :accepts #{q1}
            :transitions {q0 {n q0, m q1}}})

(def dfa6 '{:states #{q0 q1 q2 q3 q4 q5 q6 q7 q8 q9}
            :alphabet #{i l o m p t}
            :start q0
            :accepts #{q5 q8}
            :transitions {q0 {l q1}
                          q1 {i q2, o q6}
                          q2 {m q3}
                          q3 {i q4}
                          q4 {t q5}
                          q6 {o q7}
                          q7 {p q8}
                          q8 {l q9}
                          q9 {o q6}}})

(defn t1 []
  (= #{"a" "ab" "abc"}
    (set (__ dfa1))))

(defn t2 []
  (= #{"hi" "hey" "hello"}
    (set (__ dfa2))))

(defn t3 []
  (= (set (let [ss "vwxyz"] (for [i ss, j ss, k ss, l ss] (str i j k l))))
    (set (__ dfa3))))

(defn t4 []
  (let [res (take 2000 (__ dfa4))]
   (and (every? (partial re-matches #"0*(?:10*10*)*") res)
        (= res (distinct res)))))

(defn t5 []
  (let [res (take 2000 (__ dfa5))]
   (and (every? (partial re-matches #"n*m") res)
        (= res (distinct res)))))

(defn t6 []
  (let [res (take 2000 (__ dfa6))]
   (and (every? (partial re-matches #"limit|(?:loop)+") res)
        (= res (distinct res)))))

(and
 (t1)
 (t2)
 (t3)
 (t4)
 (t5)
 (t6)
)

;;; ****************************************************************
;;; http://www.4clojure.com/problem/152

;; A Latin square of order n is an n x n array that contains n different
;; elements, each occurring exactly once in each row, and exactly once in
;; each column. For example, among the following arrays only the first one
;; forms a Latin square:
;;
;; A B C    A B C    A B C
;; B C A    B C A    B D A
;; C A B    C A C    C A B
;;
;; Let V be a vector of such vectors(1) that they may differ in length(2).
;; We will say that an arrangement of vectors of V in consecutive rows is an
;; alignment (of vectors) of V if the following conditions are satisfied:

;; All vectors of V are used.
;; Each row contains just one vector.
;; The order of V is preserved.
;; All vectors of maximal length are horizontally aligned each other.
;; If a vector is not of maximal length then all its elements are aligned
;;   with elements of some subvector of a vector of maximal length.
;;
;; Let L denote a Latin square of order 2 or greater. We will say that L is
;; included in V or that V includes L iff there exists an alignment of V
;; such that contains a subsquare that is equal to L. For example, if V
;; equals [[1 2 3][2 3 1 2 1][3 1 2]] then there are nine alignments of V
;; (brackets omitted):
;;
;; 
;;        1              2              3
;;
;;      1 2 3          1 2 3          1 2 3
;;  A   2 3 1 2 1    2 3 1 2 1    2 3 1 2 1
;;      3 1 2        3 1 2        3 1 2
;;
;;      1 2 3          1 2 3          1 2 3
;;  B   2 3 1 2 1    2 3 1 2 1    2 3 1 2 1
;;        3 1 2        3 1 2        3 1 2
;;
;;      1 2 3          1 2 3          1 2 3
;;  C   2 3 1 2 1    2 3 1 2 1    2 3 1 2 1
;;          3 1 2        3 1 2        3 1 2
;;
;; Alignment A1 contains Latin square [[1 2 3][2 3 1][3 1 2]], alignments
;; A2, A3, B1, B2, B3 contain no Latin squares, and alignments C1, C2, C3
;; contain [[2 1][1 2]]. Thus in this case V includes one Latin square of
;; order 3 and one of order 2 which is included three times.
;;
;; Our aim is to implement a function which accepts a vector of vectors V as
;; an argument, and returns a map which keys and values are integers. Each
;; key should be the order of a Latin square included in V, and its value a
;; count of different Latin squares of that order included in V. If V does
;; not include any Latin squares an empty map should be returned. In the
;; previous example the correct output of such a function is {3 1, 2 1} and
;; not {3 1, 2 3}.
;;
;; (1) Of course, we can consider sequences instead of vectors. 
;; (2) Length of a vector is the number of elements in the vector.


(def __
     (fn [v]
       (let [minlen (apply min (map count v))
             maxlen (apply max (map count v))]
         )
    )
  )

(and
 (= (__ '[[A B C D]
          [A C D B]
          [B A D C]
          [D C A B]])
    {})
 (= (__ '[[A B C D E F]
          [B C D E F A]
          [C D E F A B]
          [D E F A B C]
          [E F A B C D]
          [F A B C D E]])
    {6 1})
 (= (__ '[[A B C D]
          [B A D C]
          [D C B A]
          [C D A B]])
    {4 1, 2 4})
 (= (__ '[[B D A C B]
          [D A B C A]
          [A B C A B]
          [B C A B C]
          [A D B C A]])
    {3 3})
 (= (__ [  [2 4 6 3]
           [3 4 6 2]
           [6 2 4]  ])
    {})
 (= (__ [[1]
         [1 2 1 2]
         [2 1 2 1]
         [1 2 1 2]
         []       ])
    {2 2})
 (= (__ [[3 1 2]
         [1 2 3 1 3 4]
         [2 3 1 3]    ])
    {3 1, 2 2})
 (= (__ [[8 6 7 3 2 5 1 4]
         [6 8 3 7]
         [7 3 8 6]
         [3 7 6 8 1 4 5 2]
         [1 8 5 2 4]
         [8 1 2 4 5]])
    {4 1, 3 1, 2 7})
 )
