;;; From example code at http://gamingjs.com/ice/

(ns graphics.core)

(enable-console-print!)

(defn init [scene camera]
  (set! (-> camera .-position .-z) 500)
  (.add scene camera)

  (let [geometry (THREE.IcosahedronGeometry. 200 1)
        material (THREE.MeshBasicMaterial. #js {:color 0x000000
                                                :wireframe true
                                                :wireframeLinewidth 2})
        mesh (THREE.Mesh. geometry material)
        renderer (THREE.CanvasRenderer.)]

    (.add scene mesh)
    (.setClearColorHex renderer 0xffffff)
    (.setSize renderer window.innerWidth window.innerHeight)

    (let [body (.-body js/document)
          style (.-style body)]
      (set! (.-margin style) 0)
      (set! (.-overflow style) "hidden")
      (.appendChild body (.-domElement renderer)))

    [mesh renderer]))

(defn animate [scene camera mesh renderer]
  (let [rot (.-rotation mesh)
        d (.now js/Date)]
    (set! (.-x rot) (* d 0.0005))
    (set! (.-y rot) (* d 0.001))
    (.render renderer scene camera)))

(defn main []
  (let [scene (THREE.Scene.)
        camera (THREE.PerspectiveCamera. 75 (/ (.-innerWidth js/window) (.-innerHeight js/window)) 1 1000)
        [mesh renderer] (init scene camera)
        animation (fn [f]
                    (animate scene camera mesh renderer)
                    (js/requestAnimationFrame #(f f)))]
    (animation animation)))

(main)
