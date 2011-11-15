(defn foo ([i] (+ 3 i)))

(foo 34)

; Keywords are also functions that take a map as arg and return the
; associated value.
(:b {:a 1 :b 2}) ; => 2

; List
(conj '(2 3) 1) ; => (1 2 3)

; Vector
(conj [2 3] 1) ; => [2 3 1]

; Vectors are also functions that take an index as arg.
([2 3] 0) ; => 2

; Maps are also functions that take an keys as arg.
({:a 1 :b 2} :b) ; => 2
(def m {:a 1 :b 2 :c 3})
(assoc m :d 3 :c 42) ; => {:d 4, :a 1, :b 2, :c 42}
(merge-with + m {:a 2 :b 3}) ; => #{:a 3, :b 5, :c 3}

; Sets
 #{:a :b :c}
(set [1 2 3 2 1 2 3])

; Strings
(str "abc" "def")    ; => "abcdef"
(concat "abc" "def") ; => (\a \b \c \d \e \f)

(let [avec [1 2 3 4]
      amap {:fred "ethel"}
      alist (list 2 3 4 5)]             ; bracket is part of let syntax
  [(conj avec 5)
   (conj amap [:ricky "lucy"])
   (conj alist 1)

   ; the originals are intact
   avec amap alist])
; => [[1 2 3 4 5] {:ricky "lucy", :fred "ethel"} ...]

; recursive loops
(defn zipm [keys vals]
  (loop [map {}
         [k & ks :as keys] keys
         [v & vs :as vals] vals]
    (if (and keys vals)
      (recur (assoc map k v) ks vs)
      map)))

(zip [:a :b :c] [1 2 3]) ; => {: a 1, :b 2, :c 3}

(apply hash-map (interleave [:a :b :c] [1 2 3]))
(into {} (map vector [:a :b :c] [1 2 3]))

; now use Math/PI instead of (. Math PI)
; (. Math PI)

(defn swap
  [coll i j]
  ;; If coll is a vector this is more simply written as
  ;; (assoc v i (v j) j (v i))
  (assoc coll i (nth coll j) j (nth coll i)))
