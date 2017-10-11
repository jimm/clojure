(ns city.animation)

(defn animate [frame-func & args]
  (let [animate-func (fn [self-ref]
                       (apply frame-func args)
                       (js/requestAnimationFrame #(self-ref self-ref)))]
    (animate-func animate-func)))
