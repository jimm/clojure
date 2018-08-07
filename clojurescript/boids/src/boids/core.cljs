;;; From example code at http://gamingjs.com/ice/

(ns boids.core
  (:use [boids.context :only (make) :rename {make make-context}]
        [boids.camera :only (place) :rename {place place-camera}]
        [boids.animation :only (animate)]
        [boids.flock :only (make move) :rename {make make-flock
                                                move move-flock}]))

(enable-console-print!)                 ; DEBUG

;;; ================ scene building ================

(defn build-scene [context]
  (make-flock context))

(defn make-objs [context]
  (place-camera context)
  (build-scene context))

;;; ================ frame drawing ================

(defn draw-frame [context flock]
  (let [new-flock (move-flock context flock)]
    (.render (:renderer context) (:scene context) (:camera context))
    new-flock))

;;; ================ main ================

(defn main []
  (let [context (make-context)
        flock (make-objs context)]
    (animate draw-frame context flock)))

(main)
