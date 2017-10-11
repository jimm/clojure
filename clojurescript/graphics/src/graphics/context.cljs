(ns graphics.context)

(defn- make-scene [] (THREE.Scene.))

(defn- make-camera []
  (THREE.PerspectiveCamera. 75 (/ (.-innerWidth js/window) (.-innerHeight js/window)) 1 1000))

(defn- make-renderer []
  (let [renderer (THREE.WebGLRenderer. #js {:alias true})
        body (.-body js/document)]
    (.setClearColorHex renderer 0xffffff)
    (.setSize renderer window.innerWidth window.innerHeight)
    (.appendChild body (.-domElement renderer))
    renderer))

(defn make []
    (let [style (-> js/document .-body .-style)]
      (set! (.-margin style) 0)
      (set! (.-overflow style) "hidden"))
  {:scene (make-scene) :camera (make-camera) :renderer (make-renderer)})
