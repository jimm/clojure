;;; From example code at http://gamingjs.com/ice/

(ns graphics.core)

;; (enable-console-print!)

(defn make-scene [] (THREE.Scene.))

(defn make-camera []
  (THREE.PerspectiveCamera. 75 (/ (.-innerWidth js/window) (.-innerHeight js/window)) 1 1000))

(defn make-renderer [] (THREE.CanvasRenderer.))

(defn place-camera [context]
  (set! (-> context :camera .-position .-z) 500)
  (.add (:scene context) (:camera context)))

(defn build-scene [scene]
  (let [geometry (THREE.IcosahedronGeometry. 200 1)
        material (THREE.MeshBasicMaterial. #js {:color 0x000000
                                                :wireframe true
                                                :wireframeLinewidth 2})
        mesh (THREE.Mesh. geometry material)]

    (.add scene mesh)
    mesh))

(defn make-objs [context]
  (place-camera context)
  (let [scene-objs (build-scene (:scene context))
        renderer (:renderer context)]
    (.setClearColorHex renderer 0xffffff)
    (.setSize renderer window.innerWidth window.innerHeight)
    (let [body (.-body js/document)
          style (.-style body)]
      (set! (.-margin style) 0)
      (set! (.-overflow style) "hidden")
      (.appendChild body (.-domElement renderer)))
    scene-objs))

(defn animate [context scene-objs]
  ;; scene-objs is a single mesh in this example
  (let [rot (.-rotation scene-objs)
        d (.now js/Date)]
    (set! (.-x rot) (* d 0.0005))
    (set! (.-y rot) (* d 0.001))
    (.render (:renderer context) (:scene context) (:camera context))))

(defn main []
  (let [context {:scene (make-scene)
                 :camera (make-camera)
                 :renderer (make-renderer)}
        scene-objs (make-objs context)
        animation (fn [f]
                    (animate context scene-objs)
                    (js/requestAnimationFrame #(f f)))]
    (animation animation)))

(main)
