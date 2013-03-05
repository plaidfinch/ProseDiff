(ns prosediff.ukkonen-suffix-tree
  (:require [prosediff.directed-graph :as dg]))

(defn text-deref
  "Takes the source text as a vector, the current ending character, and an interval 2-tuple where the keyword :# is a reference to whatever the current value of current-end is. Returns the corresponding chunk of text, as a vector of characters."
  ([text-vec current-end interval]
    (subvec text-vec
            (if (= :# (first interval))
                current-end
                (dec (first interval)))
            (if (= :# (second interval))
                current-end
                (second interval))))
  ([text-vec interval]
    (text-deref text-vec (count text-vec) interval)))

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
  ([n] (with-meta (symbol (str "$" n))
                  {:terminator true :number n})))

(defn terminators
  "Returns a lazy sequence of terminating symbols with metadata identifying them as such."
  ([] (map terminator (iterate inc 0))))

(defn terminator?
  "Determines if a symbol is a terminating symbol based on its metadata."
  ([s] (if (-> s meta :terminator) true false)))

(defn terminator-number
  "Returns the number of a terminating symbol given."
  ([s] (-> s meta :number)))

(defn ukkonen-construct
  "Constructs a suffix tree to represent text-vec. Uses Ukkonen's algorithm."
  ([text-vec tree active-point remainder current-end]
    ; (let [current-symbol (text-deref text-vec current-end [:# :#])]
    ;      (filter #(= current-symbol
    ;                  (first (text-deref text-vec current-end (dg/edge-label %))))
    ;              (dg/edges tree [(:active-node active-point) :_ nil])))
    text-vec))

(defn build-suffix-tree
  "Constructs a suffix tree to represent the string(s). Uses Ukkonen's algorithm."
  ([& strings]
    (ukkonen-construct (apply (comp vec concat)
                              (interleave strings (map vector (terminators))))
                       (empty-suffix-tree)
                       (starting-active-point)
                       1
                       0)))

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
                  (let [label (text-deref text-vec
                                          current-end
                                          (dg/edge-label edge))]
                       (str
                         (apply str (subvec label 0 (active-point :active-length)))
                         (if (and (= (first (text-deref text-vec
                                                        current-end
                                                        (dg/edge-label edge)))
                                      (active-point :active-edge))
                                  (= (active-point :active-node) (dg/edge-start edge)))
                             " | ")
                         (apply str (subvec label (active-point :active-length)))))
                  "\""
                  (if (and (= (first (text-deref text-vec
                                                 current-end
                                                 (dg/edge-label edge)))
                              (active-point :active-edge))
                           (= (active-point :active-node) (dg/edge-start edge)))
                      ", color=blue")
                  "]"))
         ";\n")))

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
         "}")))

