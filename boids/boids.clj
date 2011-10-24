(def max-speed 10.0)
(def origin [0 0 0])

(def dim 1000)                          ; world goes from [-dim, -dim, -dim] to [dim, dim, dim]

(def anmation-sleep-ms 100)
(def boid-sleep-ms 40)

(defstruct camera :pos :vec)

(defstruct boid :pos :vec :max-speed)

(defn add-position-to-vector
  [thing]
  (assoc thing :pos (map + (thing :pos) (thing :vec))))

;; ================ points ================

(defn square-of-distance [p0 p1]
  (let [deltas (map - p0 p1)]
    (reduce + (map * deltas deltas))))

(defn distance [p0 p1] (. Math (sqrt (square-of-distance p0 p1))))

(defn magnitude [p] (distance origin p))

;; ================ boid behavior ================

(defn move-towards-flock-center
  [boid flock]
  boid
  )

(defn avoid-others
  [boid flock]
  boid
  )

(defn match-others-velocities
  [boid flock]
  boid
  (let [others (filter #(not (= boid %)) flock)]
;;     (reduce #() others
  ))

(defn bound-position
  [boid]
  (let [v (into [] (map #(cond (< % (- dim)) 30
                               (> % dim) (- 30)
                               true 0)
                        (boid :pos)))]
    (assoc boid :vec (into [] (map + (boid :vec) v)))))

(defn limit-speed
  [boid]
  (let [speed (magnitude (boid :vec))]
    (if (> speed max-speed)
      (assoc boid :vec (map * (map / (boid :vec) (repeat speed)) (repeat max-speed))))
    boid))

(defn move
  "Boid movement."
  [boid flock]
  (dosync
   (when running
     (send *agent* move))
   (. Thread (sleep boid-sleep-ms))
   (-> boid
       (move-towards-flock-center flock)
       (avoid-others flock)
       (match-others-velocities flock)
       bound-position
       limit-speed
       add-position-to-vector)))

;; ================ GUI ================

;; ================ startup ================

; Create flock
; An agent for each boid
