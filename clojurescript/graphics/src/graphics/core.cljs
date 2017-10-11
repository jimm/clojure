;;; From example code at http://gamingjs.com/ice/

(ns graphics.core
  (:use [graphics.context :only (make) :rename {make make-context}]
        [graphics.camera :only (place) :rename {place place-camera}]
        [graphics.animation :only (animate)]))

;; (enable-console-print!)

;;; ================ scene building ================

(defn build-scene [scene]
  (let [geometry (THREE.IcosahedronGeometry. 200 1)
        material (THREE.MeshNormalMaterial. #js {:color 0x000000
                                                 :wireframe false
                                                 :wireframeLinewidth 2})
        mesh (THREE.Mesh. geometry material)]

    (.add scene mesh)
    [mesh]))

(defn make-objs [context]
  (place-camera context)
  (build-scene (:scene context)))

;;; ================ frame drawing ================

(defn rotate [obj d]
  (let [rot (.-rotation obj)]
    (set! (.-x rot) (* d 0.0005))
    (set! (.-y rot) (* d 0.001))))

(defn draw-frame [context scene-objs]
  (let [d (.now js/Date)]               ; use same rotation seed for all objs
    (doseq [obj scene-objs] (rotate obj d)))
  (.render (:renderer context) (:scene context) (:camera context)))

;;; ================ main ================

(defn main []
  (let [context (make-context)
        scene-objs (make-objs context)]
    (animate draw-frame context scene-objs)))

(main)
