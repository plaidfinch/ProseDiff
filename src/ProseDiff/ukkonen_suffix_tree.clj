(ns prosediff.ukkonen-suffix-tree
  (:require [prosediff.directed-graph :as dg]))

(defn clip-to-interval
  ([low high x]
    (cond (< x low)  low
          (> x high) high
          :else x)))

(defn safe-subvec
  "Takes the subvec of the vector given, but allowing out-of-range specifications."
  ([v start end]
    (subvec v (clip-to-interval 0 (dec (count v)) (min start end))
              (clip-to-interval 0 (dec (count v)) (max start end)))))


(defn interval-deref
  "Takes the source text as a vector, the current ending index, and an interval 2-tuple where the keyword :# is a reference to whatever the current value of current-end is. Returns the corresponding chunk of text, as a vector of characters."
  ([text-vec current-end interval]
    (safe-subvec text-vec
                 (if (= :# (first interval))
                     current-end
                     (dec (first interval)))
                 (if (= :# (second interval))
                     current-end
                     (second interval))))
  ([text-vec interval]
    (interval-deref text-vec (count text-vec) interval)))

(defn index-deref
  "Takes the source text and an index and returns the character at that index."
  ([text-vec i]
    (nth text-vec (dec i))))

(defn active-point-deref
  "Takes an active point, a tree, and a text-vec, and returns the symbol at the active point."
  ([text-vec tree active-point]
    (if (:active-edge active-point)
        (nth text-vec
             (+ (dec (:active-length active-point))
                (some identity
                      (map (comp first dg/edge-label)
                           (dg/edges tree [(:active-node active-point)
                                           (:active-edge active-point)
                                           :normal]))))))))

(defn empty-suffix-tree
  "Returns an empty suffix tree."
  ([] (dg/make-graph [] [[:root]])))

(defn starting-active-point
  "Returns the starting value of the active point."
  ([] {:active-node :root
       :active-edge nil
       :active-length 0}))

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

(defn new-node-name
  "Returns a new node name that will not conflict with the current nodes in the tree. Assumes that all nodes save :root are integer-type and sequential, which is a valid assumption if this is the only way used to generate new nodes."
  ([tree] (inc (reduce max 0 (filter number? (keys tree))))))

(defn add-child-at
  "Adds a child of the tree at the current active point and labeled to start at the current end."
  ([tree current-end active-point]
    (let [new-node (new-node-name tree)
          old-edge (first (dg/edges tree [(:active-node active-point)
                                          (:active-edge active-point)
                                          :normal]))]
      (if (and (:active-edge active-point)
               (> (:active-length active-point) 0))
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

(defn ukkonen-construct
  "Constructs a suffix tree to represent text-vec. Uses Ukkonen's algorithm."
  ([text-vec tree active-point remainder current-end]
    (let [current-symbol (first (interval-deref text-vec current-end [:# :#]))
          active-symbol  (active-point-deref text-vec tree active-point)]
         (if (= current-end (dec (count text-vec)))
             tree
             (recur text-vec
                    (add-child-at tree current-end active-point)
                    active-point
                    remainder
                    (inc current-end))))))


(defn build-suffix-tree
  "Constructs a suffix tree to represent the string(s). Uses Ukkonen's algorithm."
  ([& strings]
    (ukkonen-construct (apply (comp vec concat)
                              (interleave strings (map vector terminators)))
                       (empty-suffix-tree)
                       (starting-active-point)
                       1
                       1)))

;; NOTE: This function is rather ugly; it's not that important that it be elegant, but it could certainly be vastly more so.
(defn- dot-edge-str
  "Takes a text, active point, current end, and edge vector and returns a string representing that edge in DOT format. Not a general procedure; tailored specifically for displaying suffix trees in the representation this program uses."
  ([text-vec active-point current-end edge]
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
             (str " [label=\""
                  (let [label (interval-deref text-vec
                                              current-end
                                              (dg/edge-label edge))]
                       (str
                         (apply str (subvec label 0 (active-point :active-length)))
                         (if (and (= (first (interval-deref text-vec
                                                            current-end
                                                            (dg/edge-label edge)))
                                      (active-point :active-edge))
                                  (= (active-point :active-node) (dg/edge-start edge)))
                             " | ")
                         (apply str (subvec label (active-point :active-length)))))
                  "\""
                  (if (and (= (first (interval-deref text-vec
                                                     current-end
                                                     (dg/edge-label edge)))
                              (active-point :active-edge))
                           (= (active-point :active-node) (dg/edge-start edge)))
                      ", color=blue")
                  "]"))
         ";\n"))
  ([text-vec edge]
    (dot-edge-str text-vec (starting-active-point) (dec (count text-vec)) edge)))

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
                (map (partial dot-edge-str text-vec active-point current-end)
                     (dg/edges tree)))
         "}"))
  ([text-vec tree]
    (str "digraph SuffixTree {\n"
         "\tnode [shape=point];\n"
         "\tnode [label=""];\n"
         "\troot [width=0.1];\n"
         (apply str
                (map (partial dot-edge-str text-vec)
                     (dg/edges tree)))
         "}")))

