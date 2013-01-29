(ns prosediff.directed-graph)

(defn vertex
  "Returns graph with a vertex added to it. If optional data is given, this object is attached to the vertex; otherwise, the vertex's data is nil. If the vertex already exists, overwrites data with new given data, unless no data is given, in which case no change is made."
  ([graph vertex-name data]
    (if (graph vertex-name) 
      (assoc-in graph [vertex-name :data] data)
      (assoc graph vertex-name {:out {} :in {} :data data})))
  ([graph vertex-name]
    (if (graph vertex-name)
        graph
        (assoc graph vertex-name {:out {} :in {} :data nil}))))

(defn edge
  "Returns graph with an edge from vertex-1 to vertex-2 added to it. Creates vertices if they do not already exist. If no edge-type is supplied, defaults to nil. If no edge-label is supplied, defaults to nil. If the edge here of this type already exists, changes label (that is, there cannot be two edges of same origin, same destination, same type)."
  ([graph vertex-1 vertex-2 edge-type edge-label] 
   (-> graph 
       (vertex ,, vertex-1)
       (vertex ,, vertex-2)
       (update-in ,, [vertex-1 :out edge-type] (fnil assoc {}) vertex-2 edge-label) 
       (update-in ,, [vertex-2 :in edge-type]  (fnil assoc {}) vertex-1 edge-label)))
  ([graph vertex-1 vertex-2 edge-type]
    (edge graph vertex-1 vertex-2 edge-type nil))
  ([graph vertex-1 vertex-2]
    (edge graph vertex-1 vertex-2 nil nil)))

(defn out-edge-types
  "Returns a sequence of all the types of edges pointing from the vertex."
  ([graph vertex-name]
   (-> vertex-name graph :out keys)))

(defn in-edge-types
  "Returns a sequence of all the types of edges pointing at the vertex."
  ([graph vertex-name]
   (-> vertex-name graph :in keys)))

(defn out-edges
  "Return all edges outward-directed from the vertex in a sequence of vectors of form [<from> <to> <type> <label>]. If an edge-type is supplied, returns only edges of that type."
  ([graph vertex-name]
    (let [o-v (get-in graph [vertex-name :out])]
      (for [edge-type (keys o-v)
            [other-vertex edge-label] (o-v edge-type)]
        [vertex-name other-vertex edge-type edge-label])))
  ([graph vertex-name edge-type]
    (let [o-v (get-in graph [vertex-name :out])]
      (for [[other-vertex edge-label] (o-v edge-type)]
        [vertex-name other-vertex edge-type edge-label]))))

(defn in-edges
  "Return all edges inward-directed to the vertex in a sequence of vectors of form [<from> <to> <type> <label>].If an edge-type is supplied, returns only edges of that type."
  ([graph vertex-name]
    (let [i-v (get-in graph [vertex-name :in])]
      (for [edge-type (keys i-v)
            [other-vertex edge-label] (i-v edge-type)]
        [other-vertex vertex-name edge-type edge-label])))
  ([graph vertex-name edge-type]
    (let [i-v (get-in graph [vertex-name :in])]
      (for [[other-vertex edge-label] (i-v edge-type)]
        [other-vertex vertex-name edge-type edge-label]))))

(defn contains-vertex?
  "Tests if graph contains the vertex. Sugar for (graph vertex-name)."
  ([graph vertex-name]
    (graph vertex-name)))

(defn vertex-names
  "Returns the names of all vertices in graph."
  ([graph] (keys graph)))

(defn vertices
  "Returns a map with vertices as keys and the data attached as values."
  ([graph] (into {} (for [[v {d :data}] graph] [v d]))))

(defn edges
  "Returns all edges in graph, in a sequence of vectors of form [<from> <to> <type>]. If an edge-type is specified, returns only edges of that type."
  ([graph]
    (apply concat
           (for [v (vertices graph)]
                (out-edges graph v))))
  ([graph edge-type]
    (apply concat
           (for [v (vertices graph)]
                (out-edges graph v edge-type)))))

(defn vertex-data
  "Returns the data attached to a vertex in the graph."
  ([graph vertex-name] (get-in graph [vertex-name :data])))

(defn edge-types
  "Returns a sequence of all edge types present in the graph."
  ([graph]
    (apply (comp seq set concat)
           (for [v (vertices graph)]
                (out-edge-types graph v)))))

(defn edge-start
  "Returns the vertex name at which the edge begins."
  ([edge] (nth edge 0)))

(defn edge-end
  "Returns the vertex name at which the edge ends."
  ([edge] (nth edge 1)))

(defn edge-type
  "Returns the type of the edge."
  ([edge] (nth edge 2)))

(defn edge-label
  "Returns the label of the edge."
  ([edge] (nth edge 3)))

(defn contains-edge?
  "Returns true if graph contains an edge from vertex-1 to vertex-2, false otherwise. If an edge-type is specified, only returns true if there is an edge of edge-type in graph."
  ([graph vertex-1 vertex-2 edge-type]
   (if (and (get ((graph vertex-1) :out) edge-type)
            (get ((graph vertex-2) :in) edge-type))
       true
       false))
  ([graph vertex-1 vertex-2]
    (if (some identity
          (for [edge-type (out-edge-types graph vertex-1)]
            (contains-edge? graph vertex-1 vertex-2 edge-type)))
        true
        false)))

(defn remove-edge
  "Return a graph with the edge from vertex-1 to vertex-2 removed. Defaults to edge-type of nil if none supplied. Does nothing if the edge does not exist."
  ([graph vertex-1 vertex-2 edge-type]
   (if (contains-edge? graph vertex-1 vertex-2 edge-type)
       (-> graph
           ; remove vertices from in and out lists
           (update-in ,,
                      [vertex-1 :out edge-type]
                      (fnil dissoc {}) vertex-2)
           (update-in ,,
                      [vertex-2 :in edge-type]
                      (fnil dissoc {}) vertex-1)
           ; remove newly empty dictionary entries if they exist
           (update-in ,,
                      [vertex-1 :out]
                      (fn [d] (if (empty? (d edge-type))
                                  (dissoc d edge-type)
                                  d)))
           (update-in ,,
                      [vertex-2 :in]
                      (fn [d] (if (empty? (d edge-type))
                                  (dissoc d edge-type)
                                  d))))
       graph))
  ([graph vertex-1 vertex-2]
    (remove-edge graph vertex-1 vertex-2 nil)))

(defn remove-vertex
  "Return a graph with the vertex and all edges to and from it removed."
  ([graph vertex-name]
    (dissoc
      (reduce (fn [g v] (apply remove-edge (cons g v)))
              graph
              (concat (out-edges graph vertex-name)
                      (in-edges graph vertex-name)))
      vertex-name)))

(defn make-graph
  "Succinctly construct a graph from a sequence of vectors of form [<from> <to> <optional edge-type>], with an optional additional map of vertices to allow for unconnected vertices and data attachments."
  ([edges vertices]
   (reduce (fn [graph e] (apply edge
                                graph
                                (if (= 2 (count e))
                                    (conj e nil)
                                    e)))
           (reduce (fn [graph vertex-data-pair] 
                       (apply vertex graph vertex-data-pair))
                   (make-graph)
                   (for [vertex-data-pair vertices] vertex-data-pair))
           edges))
  ([edges]
    (make-graph edges nil))
  ([] {}))

