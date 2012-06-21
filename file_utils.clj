(ns file-utils
  (:use clojure.java.io))

(defn rm-rf [f]
  (when (.isDirectory f)
    (doseq [child (.listFiles f)] (rm-rf child)))
  (delete-file f))

(defn cp-r [f dest]
  (cond (not (.isDirectory f))
        (copy f (file dest (.getName f)))

        (not (.exists dest))
        (do
          (.mkdir dest)
          (doseq [child (.listFiles f)] (cp-r child dest)))

        :else
        (let [sub-dest (file dest (.getName f))]
          (.mkdir sub-dest)
          (doseq [child (.listFiles f)] (cp-r child sub-dest)))))
