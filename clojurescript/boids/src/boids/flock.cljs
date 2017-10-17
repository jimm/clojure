(ns boids.flock)

(def num-boids 20)
(def too-close-squared 64)
(def too-fast-squared 400)

(defn make-boid-graphic [context]
  (let [geometry (THREE.BoxGeometry. 5 5 5)
        material (THREE.MeshNormalMaterial. #js {:color 0x000000
                                                 :wireframe false
                                                 :wireframeLinewidth 2})
        mesh (THREE.Mesh. geometry material)]

    (.add (:scene context) mesh)
    mesh))


(defn make-boid [context]
  {:graphic (make-boid-graphic context)
   :loc [(rand 100) (rand 100) (rand 100)]
   :velocity [(rand 10) (rand 10) (rand 10)]})

;; (defn rotate [obj d]
;;   (let [rot (.-rotation obj)]
;;     (set! (.-x rot) (* d 0.0005))
;;     (set! (.-y rot) (* d 0.001))))

(defn center-of [flock key]
  (->> flock
       (map key)
       (reduce #(map + %1 %2))
       (map #(/ % (count flock)))
       vec))

(defn center-of-velocity [flock]
  (center-of flock :velocity))

(defn center-of-mass [flock]
  (center-of flock :loc))

(defn too-close [b1 b2]
  (let [diffs (map - (:loc b1) (:loc b2))
        squared (map * diffs diffs)]
    (>= (apply + squared) too-close-squared)))

;;; fly towards center of mass
(defn stay-together [boid flock]
  (let [com (center-of-mass flock)
        new-vel (map #(/ % 100)
                     (map - com (:loc boid)))]
    (assoc boid :velocity (vec (map + new-vel (:velocity boid))))))

;;; keep distance between boids
(defn avoid-other-boids [boid flock]
  (let [avoid-vel (apply map -
                         (for [other flock]
                           (if (too-close boid other)
                             (map - (:loc other) (:loc boid))
                             [0 0 0])))]
    (assoc boid :velocity (map + (:velocity boid) avoid-vel))))

;;; match velocity with other boids
(defn keep-up [boid flock]
  (let [cov (center-of-velocity flock)
        new-vel (map #(/ % 8)
                     (map - cov (:velocity boid)))]
    (assoc boid :velocity (vec (map + new-vel (:velocity boid))))))

(defn add-velocity-to-position [boid]
  (assoc boid :loc (vec (map + (:velocity boid) (:loc boid)))))

(defn limit-velocity [boid]
  (let [squared (apply + (map * (:velocity boid) (:velocity boid)))]
    (if (>= squared too-fast-squared)
      (let [scale (/ too-fast-squared (Math/sqrt squared))]
        (assoc boid :velocity
               (vec (map #(* scale %) (:velocity boid))))))))

(defn move-boids [context flock]
  (for [b flock]
    (let [others (remove #(= b %) flock)]
      (-> b
          (stay-together others)
          (avoid-other-boids others)
          (keep-up others)
          add-velocity-to-position
          limit-velocity))))

(defn make [context]
  (move-boids context
              (for [_ (range num-boids)] (make-boid context))))
