(ns prosediff.ukkonen-suffix-tree
    (:require [prosediff.directed-graph :as dg]))

;;  Some helpful utility functions...

(def debug
  true)

(defn update-many-in
  "Takes a map and any number of vectors of format [[k & ks] f & args] and uses update-in to update all these values in the map."
  ([m & key-function-args-vecs]
   (reduce #(apply update-in %1 %2) m key-function-args-vecs)))

(defn only-item
  "Returns first of a list, but if the list is not 1 long, fails an assertion."
  ([L]
   (assert (= 1 (count L)) (str "List passed to only-item was of length " (count L) " and was equal to the following: " L))
   (first L)))

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
             (clip 0 (count v) (max start end))))
  ([v start]
   (safe-subvec v start (count v))))

;;  Dereferencing functions for various pointers into the text...

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
  "Returns the symbol at an index in the text."
  ([text-vec i]
   (nth text-vec (dec i))))

(defn active-point-deref
  "Returns the symbol at the active point."
  ([text-vec tree {:keys [active-node active-edge active-length] :as active-point}]
   (if (and active-edge (> active-length 0))
       (nth text-vec
            (+ (dec active-length)
               (only-item
                 (map (comp first dg/edge-label)
                      (dg/edges tree [active-node active-edge :normal]))))))))

(defn edge-deref
  "Returns the subvector of the text to which an edge corresponds."
  ([text-vec current-end edge]
   (if (not (nil? edge))
       (->> edge dg/edge-label (interval-deref text-vec current-end ,,)))))

;;  Starting values for suffix tree and active point...

(defn starting-active-point
  "Returns the starting value of the active point."
  ([] {:active-node :root
       :active-edge nil
       :active-length 0}))

(defn empty-suffix-tree
  "Returns an empty suffix tree."
  ([] (with-meta
        (dg/make-graph [] [[:root]])
        {:changed true
         :finished false})))

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
  ([tree] (count tree)))

(defn add-child-at
  "Adds a child of the tree at the current active point and labeled to start at the current end. If the active point is inside an edge, splits the edge and creates a new node in order to insert the child."
  ([tree text-vec current-end {:keys [active-node active-edge active-length] :as active-point}]
   (let [new-node (new-node-name tree)
         old-edge (if active-edge
                      (only-item (dg/edges tree [active-node active-edge :normal])))
         start-node  active-node
         end-node    (dg/edge-end old-edge)
         start-index (first (dg/edge-label old-edge))
         end-index  (second (dg/edge-label old-edge))
         split-index (if-let [s-i start-index]
                             (+ start-index -1 active-length))]
        (if (active-point-deref text-vec tree active-point)
            (-> tree
                ; Split edge...
                (dg/remove-edge ,, old-edge)
                (dg/edge  ,, [start-node new-node :normal [start-index split-index]])
                (dg/edge  ,, [new-node   end-node :normal [(inc split-index) end-index]])
                ; Notate new node with its child...
                (assoc-in ,, [new-node :children (index-deref text-vec (inc split-index))] end-node)
                ; Actually add the child...
                (add-child-at ,, text-vec current-end
                              {:active-node new-node
                               :active-edge nil
                               :active-length 0}))
            (-> tree
                ; Notate what symbol child starts with...
                (assoc-in ,, [start-node :children (index-deref text-vec current-end)] new-node)
                ; Create it here...
                (dg/edge  ,, [start-node new-node :normal [current-end :#]]))))))

(defn matching-edge
  "Finds the outgoing edge which begins with the symbol specified, if any. Returns nil if there is no such edge."
  ([text-vec tree {:keys [active-node active-edge active-length] :as active-point} s]
   (if (> active-length 0)
       (if (= s (active-point-deref text-vec tree active-point))
           active-edge)
       (get-in tree [active-node :children s]))))

(defn test-and-split
  "Adds a child node at the active point if this is necessary. Annotates the returned tree with boolean meta-data key :changed to reflect whether an insertion was necessary. This is essentially equivalent to the test-and-split procedure from the original Ukkonen paper."
  ([text-vec tree {:keys [active-node active-edge active-length] :as active-point} current-end current-symbol] 
   (if (or (= current-symbol (active-point-deref text-vec tree active-point))
           (and (matching-edge text-vec tree active-point current-symbol)
                (zero? active-length)))
       (vary-meta tree
                  assoc :changed false)
       (vary-meta (add-child-at tree text-vec current-end active-point)
                  assoc :changed true))))

; TODO! Go through entire remainder, inserting as needed, and keep track of edge-split inserts during this in a list. Then (reduce (partial dg/edge tree) (map #(concat % [:suffix]) (partition 2 1 list-of-new-nodes))).

; TODO! Move active-point around tree using proper rules: if root, decrease length; else, move by suffix link at active-node; else, move to root.

; TODO! Allow for "freezing" all open leaves when a terminator is reached. This will require changes to the interval dereferencing procedure, as well as a few other things. Idea is to have different :# symbols (change to symbols ala terminators) to represent the "here" for the different strings. The one matching the terminator that is reached stops changing.

(defn ukkonen-construct
  "Constructs a suffix tree to represent text-vec. Uses Ukkonen's algorithm."
  ([text-vec tree {:keys [active-node active-edge active-length] :as active-point} remainder current-end]
   (if debug (do (println "STEP" current-end)
                 (println "active point:" active-point)
                 (println "remainder:" remainder)
                 (println "current end:" current-end)
                 (println (tree-to-dot text-vec tree active-point current-end))))
   (if (> current-end (count text-vec))
       (vary-meta tree assoc :finished true)
       (if (zero? remainder)
           (recur text-vec tree active-point 1 current-end)
           (let [current-symbol (index-deref text-vec current-end)
                 new-tree (test-and-split text-vec tree active-point current-end current-symbol)
                 tree-changed (:changed (meta new-tree))]
                (recur text-vec
                       new-tree
                       {:active-length ((if tree-changed identity inc) active-length)
                        :active-edge (matching-edge text-vec tree active-point current-symbol)
                        :active-node active-node}
                       ((if tree-changed dec inc) remainder)
                       (inc current-end)))))))

(defn make-suffix-tree
  "Constructs a suffix tree to represent the string(s). Uses Ukkonen's algorithm."
  ([& strings]
   (ukkonen-construct (apply combine-with-terminators strings)
                      (empty-suffix-tree)
                      (starting-active-point)
                      0
                      1)))

;;  Printing functions...

(defn- dot-edge-str
  "Takes a text, active point, current end, and edge vector and returns a string representing that edge in DOT format. Not a general procedure; tailored specifically for displaying suffix trees in the representation this program uses."
  ([text-vec tree {:keys [active-node active-edge active-length] :as active-point} current-end edge]
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
                                        (dec current-end)
                                        (dg/edge-label edge))
                  is-active-edge (and (= (dg/edge-end edge) active-edge)
                                      (= (dg/edge-start edge) active-node))]
                 (str " [label=\""
                         (apply str (safe-subvec label 0 active-length))
                         (if is-active-edge "|")
                         (apply str (safe-subvec label active-length))
                         "\""
                         (if is-active-edge ", color=blue")
                         "]")))
        ";\n"))
  ([tree text-vec edge]
   (dot-edge-str text-vec tree (starting-active-point) (inc (count text-vec)) edge)))

(defn tree-to-dot
  "Generates a GraphViz DOT format representation of the tree, with the active point displayed on it. Takes a tree, an active point, and the text."
  ([text-vec tree {:keys [active-node active-edge active-length] :as active-point} current-end]
   (str "digraph SuffixTree {\n"
        "\tnode [shape=point];\n"
        "\tnode [label=""];\n"
        "\troot [width=0.1];\n"
        "\t" (if (keyword? active-node)
                 (name active-node)
                 active-node)
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
                (apply make-suffix-tree strings))))

(defn print-dot-tree
  "Runs the algorithm and directly prints a DOT format tree to the console."
  ([& strings]
   (println (apply make-dot-tree strings))))
