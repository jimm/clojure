;;; From example code at http://gamingjs.com/ice/

(ns graphics.core)

;; (enable-console-print!)

;;; ================ context ================

(defn make-scene [] (THREE.Scene.))

(defn make-camera []
  (THREE.PerspectiveCamera. 75 (/ (.-innerWidth js/window) (.-innerHeight js/window)) 1 1000))

(defn make-renderer [] (THREE.CanvasRenderer.))

(defn make-context []
  {:scene (make-scene) :camera (make-camera) :renderer (make-renderer)})

;;; ================ camera ================

(defn place-camera [context]
  (set! (-> context :camera .-position .-z) 500)
  (.add (:scene context) (:camera context)))

;;; ================ scene building ================

(defn build-scene [scene]
  (let [geometry (THREE.IcosahedronGeometry. 200 1)
        material (THREE.MeshBasicMaterial. #js {:color 0x000000
                                                :wireframe true
                                                :wireframeLinewidth 2})
        mesh (THREE.Mesh. geometry material)]

    (.add scene mesh)
    [mesh]))

(defn make-objs [context]
  (place-camera context)
  (let [renderer (:renderer context)]
    (.setClearColorHex renderer 0xffffff)
    (.setSize renderer window.innerWidth window.innerHeight)
    (let [body (.-body js/document)
          style (.-style body)]
      (set! (.-margin style) 0)
      (set! (.-overflow style) "hidden")
      (.appendChild body (.-domElement renderer))))
  (build-scene (:scene context)))

;;; ================ animation ================

(defn rotate [obj d]
  (let [rot (.-rotation obj)]
    (set! (.-x rot) (* d 0.0005))
    (set! (.-y rot) (* d 0.001))))

(defn draw-frame [context scene-objs]
  (let [d (.now js/Date)]               ; use same rotation seed for all objs
    (doseq [obj scene-objs] (rotate obj d))
  (.render (:renderer context) (:scene context) (:camera context)))

;;; ================ main ================

(defn main []
  (let [context (make-context)
        scene-objs (make-objs context)
        animation (fn [f]
                    (draw-frame context scene-objs)
                    (js/requestAnimationFrame #(f f)))]
    (animation animation)))

(main)
