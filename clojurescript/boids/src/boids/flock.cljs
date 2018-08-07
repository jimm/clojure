(ns boids.flock)

(def num-boids 20)
(def too-close-squared 64)
(def too-fast 20)
(def too-fast-squared 400)

(defn make-boid-graphic [context]
  (let [geometry (THREE.CubeGeometry. 5 5 5)
        material (THREE.MeshNormalMaterial. #js {:color 0x000000
                                                 :wireframe false
                                                 :wireframeLinewidth 2})
        mesh (THREE.Mesh. geometry material)]

    (.add (:scene context) mesh)
    mesh))


(defn make-boid [context]
  (ref
   {:graphic (make-boid-graphic context)
    :loc [(rand 100) (rand 100) (rand 100)]
    :velocity [(rand 10) (rand 10) (rand 10)]}))

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
      (let [scale (/ too-fast (Math/sqrt squared))]
        (assoc boid :velocity
               (vec (map #(* scale %) (:velocity boid))))))))

(defn update-graphic-loc [boid]
  (let [pos (.-position (:graphic boid))
        [x y z] (:loc boid)]
    (set! (.-x x))
    (set! (.-y y))
    (set! (.-z z))))

(defn move [context flock]
  (dosync
   (for [b flock]
     (let [others (remove #(= b %) flock)]
       (alter b #(-> %1
                     (stay-together %2)
                     (avoid-other-boids %2)
                     (keep-up %2)
                     limit-velocity
                     add-velocity-to-position
                     update-graphic-loc
                     identity)
              others))))
  flock)

(defn make [context]
  (let [flock (for [_ (range num-boids)] (make-boid context))]
    (move context flock)
    flock))
