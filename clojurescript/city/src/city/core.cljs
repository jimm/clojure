(ns city.core
  (:use [city.context :only (make) :rename {make make-context}]
        [city.camera :only (place) :rename {place place-camera}]
        [city.animation :only (animate)]
        [city.scene :only (make-scene-objs)]))

(enable-console-print!)


;;; ================ animation ================

(defn rotate [obj d]
  (let [rot (.-rotation obj)]
    (set! (.-x rot) (* d 0.0005))
    (set! (.-y rot) (* d 0.001))))

(defn draw-frame [context scene-objs]
  #_(let [d (.now js/Date)]               ; use same rotation seed for all objs
    (doseq [obj scene-objs] (rotate obj d)))
  (let [rot (.-rotation (first scene-objs))]
    (set! (.-x rot) 90)
    (set! (.-y rot) 0.05))
  (.render (:renderer context) (:scene context) (:camera context)))

;;; ================ main ================

(defn main []
  (let [context (make-context)
        scene-objs (make-scene-objs context)]
    #_(animate draw-frame context scene-objs)
    (draw-frame context scene-objs)))

(main)
