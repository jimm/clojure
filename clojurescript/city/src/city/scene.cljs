(ns city.scene
  (:use [city.camera :only (place) :rename {place place-camera}]))

(def num-buildings 50)
(def ground-width 200)
(def ground-depth 200)
(def ground-height 10)
(def max-building-height 100)
(def max-building-width 100)
(def max-building-depth 100)

(defn build-ground [scene]
  (let [shape (THREE.CubeGeometry. ground-width ground-depth ground-height)
        cover (THREE.MeshNormalMaterial.)
        box (THREE.Mesh. shape cover)]
    (.add scene box)
    box))

(defn building-color []
  (let [base (rand 256)
        fuzz (fn [n] (bit-and 0xff (- (+ n (rand 10) 5))))]
    (+ (bit-shift-left 16 (fuzz base))
       (bit-shift-left  8 (fuzz base))
                          (fuzz base))))

(defn build-building [scene ground _i]
  (let [height (rand max-building-height)
        shape (THREE.CubeGeometry. (rand max-building-width) (rand max-building-depth) height)
        cover (THREE.MeshBasicMaterial. #js {:color (building-color)})
        box (THREE.Mesh. shape cover)]
    (.add scene box)
    (set! (.-castShadow box) true)
    (set! (.-position box) (THREE.Vector3. (- (rand 200) 100)
                                           (- (rand 200) 100)
                                           (- (/ height 2))))
    box))

(defn build-buildings [scene ground]
  (map #(build-building scene ground %) (range num-buildings)))

(defn build-scene [scene]
  (let [ground (build-ground scene)
        buildings (build-buildings scene ground)]
    (conj buildings ground)))

(defn make-scene-objs [context]
  (place-camera context)
  (build-scene (:scene context)))
