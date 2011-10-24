(def *small* (ref 1))
(def *big* (ref 100))

(defn guess-my-number []
  (bit-shift-right (+ @*small* @*big*) 1))

(defn smaller []
  (dosync (ref-set *big* (dec (guess-my-number))))
  (guess-my-number))

(defn bigger []
  (dosync (ref-set *small* (inc (guess-my-number))))
  (guess-my-number))

(defn start-over []
  (dosync
   (ref-set *small* 1)
   (ref-set *big* 100))
  (guess-my-number))
