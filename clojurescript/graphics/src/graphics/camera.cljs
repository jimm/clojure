(ns graphics.camera)

(defn place [context]
  (set! (-> context :camera .-position .-z) 500)
  (.add (:scene context) (:camera context)))
