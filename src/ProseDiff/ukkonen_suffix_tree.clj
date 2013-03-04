(ns prosediff.ukkonen-suffix-tree
  (:require [prosediff.directed-graph :as dg]))

(defn text-deref
  "Takes the source text as a vector, the current ending character, and an interval 2-tuple where :# is a reference to whatever the current value of current-end is. Returns the corresponding chunk of text, as a vector of characters."
  ([text-vec current-end interval]
    (subvec text-vec
            (dec (first interval))
            (if (= :# (second interval))
                current-end
                (second interval)))))

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
         "\tnode [label=""];\n\n"
         "\troot [width=0.1];\n"
         "\t" (if (keyword? (active-point :active-node))
                  (name (active-point :active-node))
                  (active-point :active-node))
         " [color=red, width=0.1];\n\n"
         (apply str
                (map (partial dot-edge-str text-vec active-point current-end)
                     (dg/edges tree)))
         "}")))

