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
  "Returns graph with an edge from vertex-1 to vertex-2 added to it. Creates vertices if they do not already exist. If no edge-type is supplied, defaults to nil. If no edge-label is supplied, defaults to nil. If the edge here of this type already exists, changes label (that is, there cannot be two edges of same origin, same destination, same type). Idiomatic usage is to give a vector of [vertex-1 vertex-2 edge-type edge-label], but can also take these as arguments rather than encapsulated as a vector."
  ([graph edge-vector]
    (apply edge graph edge-vector))
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

(defn- out-edge-types
  "Returns a sequence of all the types of edges pointing from the vertex."
  ([graph vertex-name]
   (-> vertex-name graph :out keys)))

(defn- in-edge-types
  "Returns a sequence of all the types of edges pointing at the vertex."
  ([graph vertex-name]
   (-> vertex-name graph :in keys)))

(defn- out-edges
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

(defn- in-edges
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

(defn all-vertices
  "Returns a map with vertices as keys and the data attached as values."
  ([graph] (into {} (for [[v {d :data}] graph] [v d]))))

(defn- all-edges
  "Returns all edges in graph, in a sequence of vectors of form [<from> <to> <type> <label>]. If an edge-type is specified, returns only edges of that type."
  ([graph]
    (apply concat
           (for [v (keys (all-vertices graph))]
                (out-edges graph v))))
  ([graph edge-type]
    (apply concat
           (for [v (keys (all-vertices graph))]
                (out-edges graph v edge-type)))))

(defn- edges-between
  "Returns a list of edges between two vertices in the format [<from> <to> <type> <label>]. If given an edge-type, returns only edges of that type (which is to say, a singleton list, as there can only be one edge of a type between two vertices)."
  ([graph vertex-1 vertex-2 edge-type]
    (list [vertex-1
           vertex-2
           edge-type
           (get-in graph
                   [vertex-1 :out edge-type vertex-2])]))
  ([graph vertex-1 vertex-2]
    (apply concat
           (for [edge-type (out-edge-types graph vertex-1)]
                (edges-between graph vertex-1 vertex-2 edge-type)))))

(defn edges
  "Returns a list of edges in the graph matching the pattern specified, thus combining into a succint interface all the explicit, specific types of edge accessor functions. Takes an (up to) 4-vector of form [<from> <to> <type> <label>] with the :_ keyword (or the vector being too short to have a particular item) representing a wildcard. If the :_ keyword conflicts with the graph, another arbitrary wildcard value may be specified.
  This is a much more efficient way to retrieve edges maching various specifications than filtering all the edges, as it takes advantage of the graph's structure to dramatically reduce the necessary work."
  ([graph wildcard [vertex-1 vertex-2 edge-type edge-label]]
    (let [query-form [(if (or (= vertex-1  wildcard) (= vertex-1  nil)) 0 1)
                      (if (or (= vertex-2  wildcard) (= vertex-2  nil)) 0 1)
                      (if (or (= edge-type wildcard) (= edge-type nil)) 0 1)]
          no-label-filter (if (or (= edge-label wildcard) (= edge-label nil)) true false)]
         (filter (fn [[v-1 v-2 e-t e-l]]
                     (or no-label-filter
                         (= e-l edge-label)))
                 (condp = query-form
                        [0 0 0] (all-edges     graph                            )
                        [0 0 1] (all-edges     graph                   edge-type)
                        [0 1 0] (in-edges      graph          vertex-2          )
                        [0 1 1] (in-edges      graph          vertex-2 edge-type)
                        [1 0 0] (out-edges     graph vertex-1                   )
                        [1 0 1] (out-edges     graph vertex-1          edge-type)
                        [1 1 0] (edges-between graph vertex-1 vertex-2          )
                        [1 1 1] (edges-between graph vertex-1 vertex-2 edge-type)))))
  ([graph [vertex-1 vertex-2 edge-type edge-label]]
    (edges graph :_ [vertex-1 vertex-2 edge-type edge-label]))
  ([graph]
    (edges graph [])))

(defn remove-edge
  "Return a graph with the edge from vertex-1 to vertex-2 removed. Defaults to edge-type of nil if none supplied. Does nothing if the edge does not exist."
  ([graph vertex-1 vertex-2 edge-type]
   (if (edges graph [vertex-1 vertex-2 edge-type])
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

(defn all-edge-types
  "Returns a set of all edge types present in the graph."
  ([graph]
    (->> graph edges (map edge-type) set)))

(defn make-graph
  "Succinctly construct a graph from a sequence of vectors of form [<from> <to> <edge-type> <edge-label>], with an optional additional list of vertices of form [<name> <data>]. When specifying edges, type and label are optional; when specifying vertices, data is optional."
  ([edges vertices]
   (reduce #(edge %1 %2)
           (reduce #(apply vertex %1 %2)
                   (make-graph)
                   vertices)
           edges))
  ([edges]
    (make-graph edges nil))
  ([] {}))

