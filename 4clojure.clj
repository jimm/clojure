;; http://www.4clojure.com/problem/82#prob-title
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

;; http://www.4clojure.com/problem/83#prob-title
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

;;; **************** done ****************

;; http://www.4clojure.com/problem/84#prob-title
;; Http://en.wikipedia.org/wiki/Transitive_closure
;; http://en.wikipedia.org/wiki/Binary_relation
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
  (fn [s]
    (let [r (range (count s))]
      

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
