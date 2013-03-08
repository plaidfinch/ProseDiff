(ns prosediff.ukkonen-suffix-tree
  (:require [prosediff.directed-graph :as dg]))

;;  Dereferencing functions for various pointers into the text...

(defn clip
  "Clips x to an interval: if x < low, returns low; if x > high, returns high; else returns x."
  ([low high x]
    (cond (< x low)  low
          (> x high) high
          :else x)))

(defn safe-subvec
  "Takes the subvec of the vector given, but allowing out-of-range specifications and backwards indices."
  ([v start end]
    (subvec v (clip 0 (count v) (min start end))
              (clip 0 (count v) (max start end)))))

(defn reify-interval
  "Takes the current end and an interval 2-tuple, and substitutes the current end into the interval wherever :# is present."
  ([current-end interval]
    [(if (= :# (first interval))
        current-end
        (first interval))
     (if (= :# (second interval))
         current-end
         (second interval))]))

(defn inclusive-to-exclusive-interval
  "Converts inclusive interval specifications (used internally to refer to intervals of text) into exclusive specifications, like those used by subvec."
  ([interval]
    ((juxt (comp dec first) second) interval)))

(defn interval-deref
  "Takes the source text as a vector, the current ending index, and an interval 2-tuple where the keyword :# is a reference to whatever the current value of current-end is. Returns the corresponding chunk of text, as a vector of characters."
  ([text-vec current-end interval]
    (apply safe-subvec
           text-vec
           (inclusive-to-exclusive-interval
             (reify-interval current-end interval))))
  ([text-vec interval]
    (interval-deref text-vec (count text-vec) interval)))

(defn index-deref
  "Takes the source text and an index and returns the character at that index."
  ([text-vec i]
    (nth text-vec (dec i))))

(defn active-point-deref
  "Takes an active point, a tree, and a text-vec, and returns the symbol at the active point."
  ([text-vec tree active-point]
    (if (and (:active-edge active-point)
             (> (:active-length active-point) 0))
        (nth text-vec
             (+ (dec (:active-length active-point))
                (some identity
                      (map (comp first dg/edge-label)
                           (dg/edges tree [(:active-node active-point)
                                           (:active-edge active-point)
                                           :normal]))))))))

(defn edge-deref
  "Takes the text-vec and an edge vector and returns the subvector to which the edge corresponds."
  ([text-vec current-end edge]
    (if (not (nil? edge))
        (->> edge dg/edge-label (interval-deref text-vec current-end ,,)))))

;;  Starting values for suffix tree and active point...

(defn empty-suffix-tree
  "Returns an empty suffix tree."
  ([] (dg/make-graph [] [[:root]])))

(defn starting-active-point
  "Returns the starting value of the active point."
  ([] {:active-node :root
       :active-edge nil
       :active-length 0}))

;;  Create and manipulate terminating symbols...

(defn terminator
  "Takes a number and returns a terminating symbol of that number."
  ([n] (with-meta (symbol (str \$ n))
                  {:terminator true :number n})))

(def terminators
  "A lazy sequence of terminating symbols with metadata identifying them as such."
  (map terminator (iterate inc 0)))

(defn terminator?
  "Determines if a symbol is a terminating symbol based on its metadata."
  ([s] (if (-> s meta :terminator) true false)))

(defn terminator-number
  "Returns the number of a terminating symbol given."
  ([s] (-> s meta :number)))

(defn combine-with-terminators
  "Takes multiple sequences and combines them with special terminator symbols to delineate the end of each."
  ([& strings]
    (apply (comp vec concat)
           (interleave strings (map vector terminators)))))

;;  The bulk of the algorithm proper...

(defn new-node-name
  "Returns a new node name that will not conflict with the current nodes in the tree. Assumes that all nodes save :root are integer-type and sequential, which is a valid assumption if this is the only way used to generate new nodes."
  ([tree] (inc (reduce max 0 (filter number? (keys tree))))))

(defn add-child-at
  "Adds a child of the tree at the current active point and labeled to start at the current end. If the active point is inside an edge, splits the edge and creates a new node in order to insert the child."
  ([tree current-end active-point]
    (let [new-node (new-node-name tree)
          old-edge (first (dg/edges tree [(:active-node active-point)
                                          (:active-edge active-point)
                                          :normal]))]
      (if (active-point-deref (repeat true) tree active-point)
          (-> tree
              (dg/remove-edge ,, old-edge)
              (dg/edge ,, [(dg/edge-start old-edge)
                           new-node
                           :normal
                           [(first (dg/edge-label old-edge))
                            (:active-length active-point)]])
              (dg/edge ,, [new-node
                           (dg/edge-end old-edge)
                           :normal
                           [(inc (:active-length active-point))
                            (second (dg/edge-label old-edge))]])
              (add-child-at ,, current-end {:active-node new-node
                                            :active-edge nil
                                            :active-length 0}))
          (dg/edge tree [(:active-node active-point)
                         new-node
                         :normal
                         [current-end :#]])))))

(defn matching-edge
  "Finds the outgoing edge which begins with the symbol specified, if any. Returns nil if there is no such edge."
  ([text-vec tree active-node current-end s]
    (first (filter #(= s (first (edge-deref text-vec current-end %)))
                   (dg/edges tree [active-node])))))

(defn test-and-split
  "Takes a text-vec, tree, active point, and current-symbol, and adds a child node at the active point if this is necessary. Annotates the returned tree with boolean meta-data key :changed to reflect whether an insertion was necessary. This is essentially equivalent to the test-and-split procedure from the original Ukkonen paper."
  ([text-vec tree active-point current-end current-symbol]
    (if (or (active-point-deref text-vec         ; Dereference the active point -- if the result
                                tree             ; is nil, this means it is not on an edge. If
                                active-point)    ; it's not on an edge ...
            (matching-edge text-vec                       ; ... then check to see if there exists
                           tree                           ; a matching outgoing (child) node from
                           (:active-node active-point)    ; the current node. If there is such a
                           current-end                    ; node,  or if there is a matching symbol 
                           current-symbol))               ; found at the active point above, then
        (with-meta tree                                   ; we don't have to change anything about
                   {:changed false})                      ; the tree. If we do have to change
        (with-meta (add-child-at tree current-end active-point) ; something, then split an edge
                   {:changed true}))))                          ; at the current active-point.

(defn ukkonen-construct
  "Constructs a suffix tree to represent text-vec. Uses Ukkonen's algorithm."
  ([text-vec tree active-point remainder current-end]
    ; (println "STEP" current-end)
    ; (println "active point:" active-point)
    ; (println "remainder:" remainder)
    ; (println "current end:" current-end)
    ; (println (tree-to-dot text-vec tree active-point current-end))
    (let [current-symbol (first (interval-deref text-vec current-end [:# :#]))]
         (if (> current-end (count text-vec))
             tree
             (recur text-vec
                    (test-and-split text-vec tree active-point current-end current-symbol)
                    active-point
                    remainder
                    (inc current-end))))))

(defn build-suffix-tree
  "Constructs a suffix tree to represent the string(s). Uses Ukkonen's algorithm."
  ([& strings]
    (ukkonen-construct (apply combine-with-terminators strings)
                       (empty-suffix-tree)
                       (starting-active-point)
                       1
                       1)))

;;  Printing functions...

(defn- dot-edge-str
  "Takes a text, active point, current end, and edge vector and returns a string representing that edge in DOT format. Not a general procedure; tailored specifically for displaying suffix trees in the representation this program uses."
  ([text-vec tree active-point current-end edge]
    (str "\t"
         (if (keyword? (dg/edge-start edge))
             (name (dg/edge-start edge))
             (dg/edge-start edge))
         " -> "
         (if (keyword? (dg/edge-end edge))
             (name (dg/edge-end edge))
             (dg/edge-end edge))
         (if (= :suffix (dg/edge-type edge))
             " [style=dotted]"
             (let [label (interval-deref text-vec
                                         current-end
                                         (dg/edge-label edge))
                   is-active-edge (and (= (first label)
                                          (active-point-deref text-vec tree active-point))
                                       (= (:active-node active-point) (dg/edge-start edge))
                                       (:active-edge active-point))]
                  (str " [label=\""
                       (apply str (subvec label 0 (:active-length active-point)))
                       (if is-active-edge " | ")
                       (apply str (subvec label (:active-length active-point)))
                       "\""
                       (if is-active-edge ", color=blue")
                       "]")))
         ";\n"))
  ([tree text-vec edge]
    (dot-edge-str text-vec tree (starting-active-point) (count text-vec) edge)))

(defn tree-to-dot
  "Generates a GraphViz DOT format representation of the tree, with the active point displayed on it. Takes a tree, an active point, and the text."
  ([text-vec tree active-point current-end]
    (str "digraph SuffixTree {\n"
         "\tnode [shape=point];\n"
         "\tnode [label=""];\n"
         "\troot [width=0.1];\n"
         "\t" (if (keyword? (active-point :active-node))
                  (name (active-point :active-node))
                  (active-point :active-node))
         " [color=red, width=0.1];\n"
         (apply str
                (map (partial dot-edge-str text-vec tree active-point current-end)
                     (dg/edges tree)))
         "}"))
  ([text-vec tree]
    (str "digraph SuffixTree {\n"
         "\tnode [shape=point];\n"
         "\tnode [label=""];\n"
         "\troot [width=0.1];\n"
         (apply str
                (map (partial dot-edge-str tree text-vec)
                     (dg/edges tree)))
         "}")))

(defn make-dot-tree
  "Runs the algorithm and directly outputs a DOT format tree."
  ([& strings]
    (tree-to-dot (apply combine-with-terminators strings)
                 (apply build-suffix-tree strings))))
