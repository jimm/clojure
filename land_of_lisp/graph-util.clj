(require '[clojure.java.shell :as sh])
(require '[clojure.string :as str])

; ================ data ================

(def *dot-max-label-length* 30)

; ================ dot functions ================

(defn dot-name [exp]
  (let [s (cond (keyword? exp) (name exp)
                (number? exp) (str "n" exp)
                :else (str exp))]
    (.replaceAll s "\\W" "_")))

(defn dot-label [exp]
  (if exp
    (let [str-exp (cond (keyword? exp) (name exp)
                        (seq? exp) (str/join ", " (map dot-name exp))
                        :else (str exp))
          s (.replaceAll str-exp "\"" "\\\\\"")]
        (if (> (.length s) *dot-max-label-length*)
          (str (.substring exp 0 (- *dot-max-label-length* 3)) "...")
          s))
    ""))

(defn nodes->dot [nodes]
  (doseq [node nodes]
    (println (dot-name (key node)) "[label=\"" (dot-name (key node)) (dot-label (val node)) "\"];")))

(defn edge-defs [room-edges]
  (doseq [e (val room-edges)]
    (println (dot-name (key room-edges))
             "->"
             (dot-name (nth e 0))
             "[label=\""
             (dot-label (rest e))
             "\"];")))

(defn edges->dot [edges]
  (doseq [room-edges edges]
    (edge-defs room-edges)))

(defn graph->dot [nodes edges]
  (println "digraph {")
  (nodes->dot nodes)
  (edges->dot edges)
  (println "}"))

(defn dot->graphviz [fname thunk]
  (binding [*out* (java.io.FileWriter. fname)]
    (thunk)
    (.close *out*))
  (sh/sh "open" fname)) ; Mac OS X

(defn dot->png [fname thunk]
  (binding [*out* (java.io.FileWriter. fname)]
    (thunk)
    (.close *out*))
  (sh/sh "dot" "-Tpng" "-O" fname))

(defn graph->graphviz [fname nodes edges]
  (dot->graphviz fname #(graph->dot nodes edges)))

(defn graph->png [fname nodes edges]
  (dot->png fname #(graph->dot nodes edges)))

; ================ extensions ================

(defn uedges->dot [edges]
  (let [seen (ref #{})]
    (doseq [node-edges edges]
      (let [node (key node-edges)
            es (val node-edges)]
        (doseq [nlist es]
          (let [other-node (first nlist)
                pair [(min node other-node) (max node other-node)]]
            (when-not (contains? @seen pair)
              (dosync (alter seen conj pair))
              (println (dot-name node)
                  "--"
                  (dot-name other-node)
                  "[label=\""
                  (dot-label (rest nlist))
                  "\"];"))))))))

(defn ugraph->dot [nodes edges]
  (println "graph {")
  (nodes->dot nodes)
  (uedges->dot edges)
  (println "}"))

(defn ugraph->graphviz [fname nodes edges]
  (dot->graphviz fname #(ugraph->dot nodes edges)))

(defn ugraph->png [fname nodes edges]
  (dot->png fname #(ugraph->dot nodes edges)))

; ================ workspace ================

(comment

(load-file "wizard-data.clj")
  
(ugraph->graphviz "/tmp/dot.gv" *wizard-nodes* *wizard-edges*)
(graph->graphviz "/tmp/dot.gv" *wizard-nodes* *wizard-edges*)

(graph->dot *wizard-nodes* *wizard-edges*)
(edges->dot *wizard-edges*)
(nodes->dot *wizard-nodes*)

(dot-name 24)

) ; end comment
