(load-file "graph-util.clj")

(require '[clojure.set :as set])

(def *congestion-city-nodes* (ref nil))
(def *congestion-city-edges* (ref nil))
(def *player-pos* (ref nil))
(def *visited-nodes* (ref nil))
(def *node-num* 30)
(def *edge-num* 45)
(def *worm-num* 3)
(def *cop-odds* 15)

;; ================ utils ================

(defn random-node []
  (inc (rand-int *node-num*)))

(defn edge-pair [a b]
  (when-not (= a b)
    (list [a b] [b a])))

(defn make-edge-list []
  (loop [n *edge-num*
         l ()]
    (let [[e0 e1] (edge-pair (random-node) (random-node))]
      (if (> n 0) (recur (dec n) (conj l e0 e1))
          (filter identity l)))))       ; filter to get rid of nils

;; ================ join islands ================

(defn direct-edges [node edge-list]
  (filter #(= (first %) node) edge-list))

(defn get-connected
  ([node edge-list]
     (get-connected node edge-list (ref #{})))
  ([node edge-list visited]
     (letfn [(traverse [n] (when-not (contains? @visited n)
                             (dosync (alter visited conj n))
                             (doseq [e (direct-edges n edge-list)]
                               (traverse (nth e 1)))))]
       (traverse node)
       @visited)))

(defn find-islands
  ([nodes edge-list]
     (find-islands nodes edge-list (ref ())))
  ([nodes edge-list islands]
     (letfn [(find-island [ns]
                          (let [connected (set (get-connected (first ns) edge-list))
                                unconnected (set/difference (set ns) connected)]
                            (dosync (alter islands conj connected))
                            (when (not-empty unconnected)
                              (find-island unconnected))))]
       (find-island nodes)
       @islands)))

(defn connect-with-bridges [islands]
  (when (next islands)
    (concat (edge-pair (ffirst islands) (ffirst (rest islands)))
            (connect-with-bridges (rest islands)))))

(defn connect-all-islands [nodes edge-list]
  (concat (connect-with-bridges (find-islands nodes edge-list)) edge-list))

;; ================ make city streets ================

(defn add-cops [edges-map edges-with-cops]
  "Returns a map whose keys are nodes and values are lists of single edges,
possibly with :cops. For example, {22 ((23)), 24 ((15) (4
:cops)), 4 ((24 :cops))}."
  (into {}
        (map (fn [edge-map-entry]
               (let [node1 (key edge-map-entry)
                     node1-edges (val edge-map-entry)]
                 {node1 (map (fn [node-edge]
                               (let [node2 (first node-edge)]
                                 (if (not-empty (set/intersection (set (edge-pair node1 node2))
                                                                  (set edges-with-cops)))
                                   (list node2 :cops)
                                   node-edge)))
                             node1-edges)
                       }))
             edges-map)))

(defn edges-to-map [edge-list]
  "Returns a map whose keys are nodes and values are lists of single edges.
For example, {22 ((23)), 24 ((15) (4)), 4 ((24))}."
  (into {}
        (map (fn [node]
               {node (map (fn [edge] (list (nth edge 1)))
                          (set (direct-edges node edge-list)))})
             (set (map first edge-list)))))

(defn make-city-edges []
  (let [nodes (map inc (range *node-num*))
        edge-list (connect-all-islands nodes (make-edge-list))
        edges-with-cops (filter (fn [e] (zero? (rand-int *cop-odds*))) edge-list)]
    (add-cops (edges-to-map edge-list) edges-with-cops)))
     

;; ================ node creation and neighbor tests ================

(defn neighbors [node edge-map]
  (set (map first (get edge-map node))))

(defn within-one [a b edge-map]
  (contains? (neighbors a edge-map) b))

(defn within-two [a b edge-map]
  (or (within-one a b edge-map)
      (some #(within-one % b edge-map)
            (neighbors a edge-map))))

(defn make-city-nodes [edge-map]
  (let [wumpus (random-node)
        glow-worms (into #{} (map (fn [_] (random-node)) (range *worm-num*)))]
    (into {}
          (map (fn [edge-map-entry]
                 (let [n (key edge-map-entry)
                       edges (val edge-map-entry)]
                   {n
                    (filter identity   ; get rid of nils
                            (list
                             (cond (= n wumpus) :wumpus
                                   (within-two n wumpus edge-map) :blood)
                             (cond (contains? glow-worms n) :glow-worm
                                   (some #(within-one n % edge-map) glow-worms) :lights!)
                             ; Relies on the fact that :cops is cdr
                             (when (some next edges)
                               :sirens!)))}))
               edge-map))))

(defn visited-and-nearby-nodes []
  (distinct
   (concat @*visited-nodes*
           (mapcat (fn [node]
                     (map first
                          (get @*congestion-city-edges* node)))
                   @*visited-nodes*))))

(defn known-city-nodes []
  (let [kn (ref {})]
    (dosync
     (doseq [node (visited-and-nearby-nodes)]
       (if (contains? @*visited-nodes* node)
         (let [contents (get @*congestion-city-nodes* node)]
           (if (= node @*player-pos*)
             (alter kn conj {node (conj contents :you-are-here)})
             (alter kn conj {node contents})))
         (alter kn conj {node (list "unknown")}))))
    @kn))

(defn known-city-edges []
  "Return edges attached to visited nodes, stripped of cop sirens if we have
not yet heard them (visited the node at the other end of the edge)."
  (into {}
        (map (fn [node]
               (hash-map node
                         (map (fn [x]
                            (if (contains? @*visited-nodes* (first x)) ; seen other end
                              x
                              (list (first x))))
                          (get @*congestion-city-edges* node))))
             @*visited-nodes*)))

;; ================ game ================

(defn find-empty-node []
  (let [n (random-node)]
    (if (not-empty (get @*congestion-city-nodes* n)) ;; try again
      (find-empty-node)
      n)))

(defn draw-city []
  (ugraph->graphviz "/tmp/city.gv" @*congestion-city-nodes* @*congestion-city-edges*))

(defn draw-known-city []
  (ugraph->graphviz "/tmp/known-city.gv" (known-city-nodes) (known-city-edges)))

(defn init-game []
  (dosync
   (ref-set *congestion-city-edges* (make-city-edges))
   (ref-set *congestion-city-nodes* (make-city-nodes @*congestion-city-edges*))
   (ref-set *player-pos* (find-empty-node))
   (ref-set *visited-nodes* #{@*player-pos*})))

(defn new-game []
  (init-game)
  (draw-city)
  (draw-known-city))

;; ================ movement ================

;; TODO expects node vals to be assoc lists, so I need to change those to maps
;; where key = node and val = (possibly nil) contents.
(defn handle-direction [pos charging]
  (let [edge (
)]))

(defn walk [pos]
  (handle-direction pos nil))

(defn charge [pos]
  (handle-direction pos true))

;; ================ test code ================

(comment

(let [el '([1 3] [3 1] [3 4] [4 3] [3 2] [2 3] [5 6] [6 5])
      nodes (map inc (range 6))]
  (connect-all-islands nodes el))

(let [el '([1 3] [3 1] [3 4] [4 3] [3 2] [2 3] [5 6] [6 5])
      nodes (map inc (range 6))]
  (connect-with-bridges (find-islands nodes el)))

(let [el '([1 3] [3 1] [3 4] [4 3] [3 2] [2 3] [5 6] [6 5])
      nodes (map inc (range 6))]
  (find-islands nodes el))

(let [el '([1 3] [3 1] [3 4] [4 3] [3 2] [2 3] [5 6] [6 5])]
  (println "edge list:" el)
  (for [i (map inc (range 6))]
    (println "connected to" i ":" (get-connected i el))))

(let [el '([1 3] [3 1] [3 4] [4 3] [3 2] [2 3] [5 6] [6 5])]
  (println "edge list:" el)
  (for [i (map inc (range 6))]
    (println "direct edges of" i ":" (direct-edges i el))))

(def el (make-edge-list))
(edge-pair (random-node) (random-node))
(random-node)

) ; end comment
