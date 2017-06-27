(ns debug)

(def ^:dynamic *debug* false)

(defn debug
  [& args]
  (when *debug*
    (apply println args)))

(defmacro debug-dump
  "Generate a call to debug with each form becoming a string and a value,
  all optionally preceded by a string. For example:

      (debug-dump x (reverse s)) ; (debug \"x\" x \"(reverse s)\" (reverse s))
      (debug-dump \"hello\" x y) ; (debug \"hello\" \"x\" x \"y\" y)"
  [init-str-or-form & forms]
  (let [prefix (if (string? init-str-or-form)
                 (list 'debug init-str-or-form)
                 (list 'debug))
        all-forms (if (string? init-str-or-form)
                   forms
                   (conj forms init-str-or-form))]
    (concat prefix
            (mapcat #(list (list 'str (list 'quote %)) %)
                    all-forms))))
