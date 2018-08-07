(ns boids.animation)

(defn animate [frame-func & args]
  (let [args-ref (ref args)
        animate-func (fn [self-ref]
                       (dosync
                        (ref-set args-ref (apply frame-func @args-ref)))
                       (js/requestAnimationFrame #(self-ref self-ref)))]
    (animate-func animate-func)))
