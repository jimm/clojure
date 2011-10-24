; to do
; - aliases ("l" for look, "go" or "g" for walk, etc.)

(require '[clojure.string :as str])

; ================ data ================

(load-file "wizard-data.clj")

(defn edge-direction [edge] (nth edge 1))
(defn edge-appearance [edge] (nth edge 2))

; ================ game engine ================

(defn describe-location [location nodes]
  (location nodes))

(defn describe-path [edge]
  (str "There is a " (edge-appearance edge) " going " (edge-direction edge) " from here."))

(defn describe-paths [location edges]
  (str/join " " (map describe-path (location edges))))

(defn objects-at [loc obj-locs]
  (let [at-loc? #(if (= loc (%1 obj-locs)) %1 nil)]
    (keep at-loc? (keys obj-locs))))

(defn describe-objects [loc obj-loc]
  (str/join " " (map #(str "You see a " (name %1) " on the floor.") (objects-at loc obj-loc))))

(defn object-at [object loc obj-locs]
  ; Is this the best way to test if object is in a collection?
  (some (hash-set object) (objects-at loc obj-locs)))

; ================ game commands ================

(defn look []
  (str (describe-location @*wizard-location* *wizard-nodes*) " "
       (describe-paths @*wizard-location* *wizard-edges*) " "
       (describe-objects @*wizard-location* @*wizard-object-locations*)))

(defn walk [direction]
  (let [exits (*wizard-location* *wizard-edges*)
        next-loc (first (filter #(= (nth %1 1) direction) exits))]
    (if next-loc
      (dosync
       (ref-set *wizard-location* (nth next-loc 0))
       (look))
      "You cannot go that way.")))

(defn pickup [object-name]
  (let [object (keyword object-name)
        here? (object-at object @*wizard-location* @*wizard-object-locations*)
        i-haz? (object-at object :body @*wizard-object-locations*)]
    (cond here? (dosync
                 (alter *wizard-object-locations* assoc object :body)
                 (str "You are now carrying the " (name object) "."))
          i-haz? (str "You already have the " (name object) ".")
          :else "You cannot get that.")))

(defn letgo [object-name]
  (let [object (keyword object-name)
        i-haz? (object-at object :body @*wizard-object-locations*)]
    (cond i-haz? (dosync
                 (alter *wizard-object-locations* assoc object @*wizard-location*)
                 (str "You dropped the " (name object) "."))
          :else (str "You are not carrying the " (name object) "."))))

(defn inventory []
  (let [names (map name (objects-at :body @*wizard-object-locations*))]
    (cond (empty? names) "You are not carrying anything."
          :else (str "You have:\n  " (str/join "\n  " names)))))

(defn reset []
  (dosync
   (ref-set *wizard-object-locations* {:whiskey :living-room
                                       :bucket :living-room
                                       :chain :garden
                                       :frog :garden})
   (ref-set *wizard-location* :living-room)))

; ================ repl ================

(defn game-read []
  (let [words (.split (read-line) " ")]
    (conj (rest words) (symbol (first words)))))

(defn game-eval [sexp]
  (if (some (hash-set (keyword (first sexp))) *wizard-allowed-commands*) (eval sexp)
      "I do not know that command."))

(defn game-print [lst]
  (println lst))

(defn game-repl []
  (let [cmd (game-read)]
    (when-not (= (first cmd) 'quit)
      (game-print (game-eval cmd))
      (game-repl))))

; ================ experimentation ================

(comment
(reset)

(println (inventory))

(letgo "whiskey")
(pickup "whiskey")

(walk "east")
(walk "west")
(look)

(describe-objects :living-room @*wizard-object-locations*)
(objects-at :living-room @*wizard-object-locations*)

(describe-paths :living-room *wizard-edges*)
(describe-path [:garden "west" "door"])

(describe-location :garden *wizard-nodes*)

) ; end comment
