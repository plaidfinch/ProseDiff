(ns prosediff.ukkonen-suffix-tree
    (:require [prosediff.directed-graph :as dg]
              [clojure.java.shell :as shell]))

(declare
  matching-edge tree-to-dot view-dot)

; Print additional logging during program run.
(def debug
  false)

; By default, print a DOT tree including formatting that makes it render nicely. For logging output, this may be disabled for ease of scrolling.
(def final-dot-formatting
  true)

;;  Some helpful utility functions...

; Wrap in dbg to log.
(defmacro dbg [x] `(let [x# ~x] (println '~x "=" x#) x#))

(defn take-until-same
  "Returns the part of a sequence up until it has a consecutive repeated element."
  ([s]
   (cons (first s)
         (map second (take-while #((complement =) (first %) (second %))
                                 (partition 2 1 s))))))

(defn take-while-unique
  "Returns a lazy subsequence of a sequence up until it repeats any element (not just a consecutive element like take-until-same does)."
  ([coll]
   (take-while-unique coll #{}))
  ([coll seen]
   (lazy-seq
     (when ((complement seen) (first coll))
           (when-let [coll (seq coll)]
                     (cons (first coll)
                           (take-while-unique
                             (rest coll)
                             (conj seen (first coll)))))))))

; This one is from clojure.contrib
(defn dissoc-in
  "Dissociates an entry from a nested associative structure returning a new
  nested structure. keys is a sequence of keys. Any empty maps that result
  will not be present in the new structure."
  [m [k & ks :as keys]]
  (if ks
    (if-let [nextmap (get m k)]
      (let [newmap (dissoc-in nextmap ks)]
        (if (seq newmap)
          (assoc m k newmap)
          (dissoc m k)))
      m)
    (dissoc m k)))

(defn update-many-in
  "Takes a map and any number of vectors of format [[k & ks] f & args] and uses update-in to update all these values in the map."
  ([m & key-function-args-vecs]
   (reduce #(apply update-in %1 %2) m key-function-args-vecs)))

(defn assoc-many-in
  "Takes a map and any number of vectors of format [[k & ks] f & args] and uses assoc-in to assoc all these values in the map."
  ([m & key-value-vecs]
   (reduce #(apply assoc-in %1 %2) m key-value-vecs)))

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

;;  Create and manipulate terminating symbols and end symbols...

(defn terminator
  "Takes a number and returns a terminating symbol of that number."
  ([n] (with-meta (symbol (str \$ n))
                  {::terminator true ::number n})))

(def terminators
  "A lazy sequence of terminating symbols with metadata identifying them as such."
  (map terminator (iterate inc 0)))

(defn terminator?
  "Determines if a symbol is a terminating symbol based on its metadata."
  ([s] (if (-> s meta ::terminator) true false)))

(defn terminator-number
  "Returns the number of a terminating symbol given."
  ([s] (if (terminator? s) (-> s meta ::number))))

(defn combine-with-terminators
  "Takes multiple sequences and combines them with special terminator symbols to delineate the end of each."
  ([& strings]
   (apply (comp vec concat)
          (interleave strings (map vector terminators)))))

(defn end-symbol
  "Takes a number and returns an end-symbol of that number."
  ([n] (with-meta (symbol (str \# n))
                  {::end-symbol true ::number n})))

(def end-symbols
  "A lazy sequence of end-symbols with metadata identifying them as such."
  (map end-symbol (iterate inc 0)))

(defn end-symbol?
  "Determines if a symbol is an end-symbol based on its metadata."
  ([s] (if (-> s meta ::end-symbol) true false)))

(defn end-symbol-number
  "Returns the number of an end-symbol given."
  ([s] (if (end-symbol? s) (-> s meta ::number))))

(defn find-ends
  "Takes any number of strings and returns an ends map mapping end-symbols to where those ends would be in the terminator-combined text."
  ([& strings]
   (let [end-numbers (drop 1 (reductions #(inc (+ %1 %2)) 0 (map count strings)))]
        (into
          (sorted-map-by
            (fn [a b]
                (assert (and (or (end-symbol? a)
                                 (and (keyword? a)
                                      (= ":current-end" (str a))))
                             (or (end-symbol? b)
                                 (and (keyword? b)
                                      (= ":current-end" (str b)))))
                        "The keys of an ends map may not be anything other than end-symbols and the keyword :current-end.")
                (cond (and (end-symbol? a) (end-symbol? b))
                        (apply < (map end-symbol-number [a b]))
                      (and (end-symbol? a) (keyword? b))
                        true
                      (and (keyword? a) (end-symbol? b))
                        false)))
          (map #(vector (end-symbol %2) %1)
               end-numbers
               (iterate inc 0))))))

;;  Dereferencing functions for various pointers into the text...

(defn reify-interval
  "Takes the current end and an interval 2-tuple, and substitutes the current end into the interval wherever an end-symbol is present."
  ([{:keys [current-end] :as ends} [interval-start interval-end]]
   [(if (end-symbol? interval-start)
        (min (get ends interval-start current-end) current-end)
        interval-start)
    (if (end-symbol? interval-end)
        (min (get ends interval-end current-end) current-end)
        interval-end)]))

(defn inclusive-to-exclusive-interval
  "Converts inclusive interval specifications (used internally to refer to intervals of text) into exclusive specifications, like those used by subvec."
  ([[start end]]
   [(dec start) end]))

(defn interval-deref
  "Takes the source text as a vector, the current ending index, and an interval 2-tuple where the keyword :# is a reference to whatever the current value of current-end is. Returns the corresponding chunk of text, as a vector of characters."
  ([text-vec {:keys [current-end] :as ends} interval]
   (apply safe-subvec
          text-vec
          (inclusive-to-exclusive-interval
            (reify-interval ends interval))))
  ([text-vec interval]
   (interval-deref text-vec (count text-vec) interval)))

(defn index-deref
  "Returns the symbol at an index in the text."
  ([text-vec i]
   (nth text-vec (dec i))))

(defn active-point-deref
  "Returns the symbol at the active point."
  ([text-vec tree {:keys [active-node active-edge active-length] :as active-point}]
   (if (and active-edge
            (matching-edge tree active-node active-edge))
       (index-deref
         text-vec
         (+ active-length
            ((comp first dg/edge-label)
             (only-item
               (dg/edges
                 tree [active-node
                       (matching-edge tree active-node active-edge)
                       :normal]))))))))

(defn edge-deref
  "Returns the subvector of the text to which an edge corresponds."
  ([text-vec {:keys [current-end] :as ends} edge]
   (if (not (nil? edge))
       (->> edge dg/edge-label (interval-deref text-vec ends ,,)))))

(defn edge-length
  "Returns the length of an edge, given the current ends."
  ([ends edge]
   (apply #(- (inc %2) %1) (reify-interval ends (dg/edge-label edge)))))

;;  Starting values for suffix tree and active point...

(defn starting-active-point
  "Returns the starting value of the active point."
  ([] {:active-node :root
       :active-edge nil
       :active-length 0}))

(defn starting-remainder
  "Returns the starting value of the remainder."
  ([] 1))

(defn starting-suffix-tree
  "Returns an empty suffix tree."
  ([] (dg/make-graph [] [[:root]])))

;;  The bulk of the algorithm proper...

(defn new-node-name
  "Returns a new node name that will not conflict with the current nodes in the tree. Assumes that all nodes save :root are integer-type and sequential, which is a valid assumption if this is the only way used to generate new nodes."
  ([tree] (count tree)))

(defn add-child-at
  "Adds a child of the tree at the current active point and labeled to start at the current end. If the active point is inside an edge, splits the edge and creates a new node in order to insert the child."
  ([tree text-vec {:keys [current-end] :as ends} {:keys [active-node active-edge active-length] :as active-point}]
   (assert (= clojure.lang.PersistentTreeMap (type ends)))
   (let [new-node (new-node-name tree)
         old-edge (if active-edge
                      (only-item
                        (dg/edges tree [active-node
                                        (matching-edge tree active-node active-edge)
                                        :normal])))
         start-node  active-node
         end-node    (dg/edge-end old-edge)
         start-index (first (dg/edge-label old-edge))
         end-index  (second (dg/edge-label old-edge))
         split-index (if-let [s-i start-index]
                             (+ start-index -1 active-length))]
        (if (and (> active-length 0)
                 (active-point-deref text-vec tree active-point))
            (-> tree
                ; Split edge...
                (dg/remove-edge ,, old-edge)
                (dg/edge ,, [start-node new-node :normal [start-index split-index]])
                (dg/edge ,, [new-node end-node :normal [(inc split-index) end-index]])
                ; Notate new node with its child...
                (assoc-in ,,
                          [new-node :children (index-deref text-vec (inc split-index))]
                          end-node)
                ; Notate the old node with its new child...
                (assoc-in ,, [start-node :children active-edge] new-node)
                ; Actually add the child...
                (add-child-at ,, text-vec ends
                              {:active-node new-node
                               :active-edge nil
                               :active-length 0}))
            (-> tree
                ; Notate what symbol child starts with...
                (assoc-in ,,
                          [start-node :children (index-deref text-vec current-end)]
                          new-node)
                ; Create it here...
                (dg/edge ,,
                         [start-node new-node :normal 
                          [current-end
                           (->> ends
                                (drop-while #(< (second %) current-end ,,))
                                ffirst)]]))))))

(defn matching-edge
  "If the active point is not on an edge, finds the outgoing edge from the active node which begins with the symbol specified. If the active point is on an edge, returns that edge iff the symbol matches on that edge at the current point. Otherwise, returns nil."
  ([text-vec tree {:keys [active-node active-edge active-length] :as active-point} s]
   (if (or (= active-length 0)
           (= s (active-point-deref text-vec tree active-point)))
       (get-in tree [active-node :children s])))
  ([tree active-node s]
   (get-in tree [active-node :children s])))

(defn test-and-split
  "Adds a child node at the active point if this is necessary. This is essentially equivalent to the test-and-split procedure from the original Ukkonen paper."
  ([text-vec tree {:keys [active-node active-edge active-length] :as active-point} {:keys [current-end] :as ends}]
   (let [current-symbol (index-deref text-vec current-end)]
        (if (matching-edge text-vec tree active-point current-symbol)
            tree
            (add-child-at tree text-vec ends active-point)))))

; TODO! Go through entire remainder, inserting as needed, and keep track of edge-split inserts during this in a list. Then (reduce (partial dg/edge tree) (map #(concat % [:suffix]) (partition 2 1 list-of-new-nodes))).

(defn raise-active-point
  "Moves the active point towards the root by one character, using the rules for the algorithm -- if active node is root, decrease length and switch active edge to the next-shortest edge; if not, follow a suffix link if there is one, and if there is not one, go to root."
  ([text-vec tree remainder {:keys [current-end] :as ends} {:keys [active-node active-edge active-length] :as active-point}]
   (let [next-active-edge (if (> (- current-end remainder 1) 0)
                              (index-deref text-vec (- current-end remainder 1)))]
        (if (= active-node :root)
            (if (> active-length 0)
                (assoc-many-in active-point
                               [[:active-length] (dec active-length)]
                               [[:active-edge]   next-active-edge])
                active-point)
            (if-let [out-suffix-link
                     (do (assert (<= (dg/edges tree [active-node :_ :suffix]) 1)
                                 "Something is wrong if there is more than one suffix link out of any node in the suffix tree.")
                         (first (dg/edges tree [active-node :_ :suffix])))]
                    (assoc active-point :active-node
                           (dg/edge-end out-suffix-link))
                    (assoc active-point :active-node :root))))))

(defn lower-active-point
  "Moves the active point away from the root by one character. If the active edge is nil, finds the correct active edge to pick based on the current symbol."
  ([text-vec tree remainder {:keys [current-end] :as ends} {:keys [active-node active-edge active-length] :as active-point}]
   (let [this-active-edge (index-deref text-vec (- current-end active-length))]
        (assoc-many-in active-point
                       [[:active-length] (inc active-length)]
                       [[:active-edge]   this-active-edge]))))
        
(defn advance-active-point
  "Raises or lowers the active point based on whether the tree has changed (yes -> raise, no -> lower)."
  ([tree-changed text-vec tree remainder ends active-point]
   (if tree-changed
       (raise-active-point text-vec tree remainder ends active-point)
       (lower-active-point text-vec tree remainder ends active-point))))

(defn test-and-split-all
  "Iterates through the remainder, splitting off new children using test-and-split until the graph remains unchanged -- indicating that all remaining suffixes are present in the graph."
  ([text-vec tree remainder {:keys [current-end] :as ends} active-point]
   (loop [tree         tree
          remainder    remainder
          active-point active-point
          new-nodes    []]
         (if debug
             (do (view-dot (tree-to-dot text-vec tree active-point ends)
                           (str "preview" current-end "-" (count new-nodes)))
                 (dbg active-point) (dbg remainder) (dbg ends)
                 (with-redefs [final-dot-formatting false]
                              (println (tree-to-dot text-vec tree active-point ends))
                              (println))))
         (let [new-tree     (test-and-split text-vec tree active-point ends)
               tree-changed (not (= tree new-tree))]
              (if tree-changed
                  (recur new-tree
                         (dec remainder)
                         (raise-active-point text-vec tree remainder ends active-point)
                         (if (= 2 (- (count new-tree) (count tree))) ; <- if edge-split
                             (conj new-nodes (new-node-name tree))
                             new-nodes))
                  (reduce dg/edge
                          new-tree
                          (map #(concat % [:suffix])
                               (partition 2 1 new-nodes))))))))

(defn canonize
  "Makes the active point canonical. Same as in original paper. NOT YET IMPLEMENTED!!!"
  ([text-vec tree remainder {:keys [current-end] :as ends} {:keys [active-node active-edge active-length] :as active-point}]
   active-point))

(defn ukkonen-construct
  "Constructs a suffix tree to represent text-vec. Uses Ukkonen's algorithm."
  ([text-vec tree {:keys [active-node active-edge active-length] :as active-point} remainder {:keys [current-end] :as ends}]
   (if (> current-end (count text-vec))
       (do (if debug (do (println "CONSTRUCTION COMPLETE.\n")
                         (dbg active-point)
                         (dbg remainder)
                         (dbg ends)))
           (vary-meta tree assoc ::finished true))
       (let [new-tree (test-and-split-all text-vec tree remainder ends active-point)
             tree-changed (not (= tree new-tree))]
            (recur text-vec
                   new-tree
                   (if tree-changed
                       (starting-active-point)
                       (lower-active-point text-vec new-tree remainder ends active-point))
                   (if tree-changed 1 (inc remainder))
                   (update-in ends [:current-end] inc))))))

(defn make-suffix-tree
  "Constructs a suffix tree to represent the string(s). Uses Ukkonen's algorithm."
  ([& strings]
   (ukkonen-construct (apply combine-with-terminators strings)
                      (starting-suffix-tree)
                      (starting-active-point)
                      (starting-remainder)
                      (into (apply find-ends strings) {:current-end 1}))))

;;  Printing functions...

(defn- dot-edge-str
  "Takes a text, active point, current end, and edge vector and returns a string representing that edge in DOT format. Not a general procedure; tailored specifically for displaying suffix trees in the representation this program uses."
  ([text-vec tree {:keys [active-node active-edge active-length] :as active-point} {:keys [current-end] :as ends} edge]
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
                                        (update-in ends [:current-end] dec)
                                        (dg/edge-label edge))
                  is-active-edge (and (= (dg/edge-end edge)
                                         (matching-edge tree active-node active-edge))
                                      (= (dg/edge-start edge) active-node))]
                 (str " [label=\""
                         (apply str (safe-subvec label 0 active-length))
                         (if is-active-edge "|")
                         (apply str (safe-subvec label active-length))
                         "\""
                         (if is-active-edge ", color=blue")
                         "]")))
        ";\n"))
  ([tree text-vec ends edge]
   (dot-edge-str text-vec
                 tree
                 (starting-active-point)
                 (into ends {:current-end (inc (count text-vec))})
                 edge)))

(defn tree-to-dot
  "Generates a GraphViz DOT format representation of the tree, with the active point displayed on it. Takes a tree, an active point, and the text."
  ([text-vec tree {:keys [active-node active-edge active-length] :as active-point} {:keys [current-end] :as ends}]
   (str "digraph {\n"
        (if final-dot-formatting
            (str "\tnode [shape=point];\n\tnode [label=""];\n\troot [width=0.1];\n"
                  "\t" (if (keyword? active-node)
                           (name active-node)
                           active-node)
                  " [color=red, width=0.1];\n"))
        (apply str
               (map (partial dot-edge-str text-vec tree active-point ends)
                    (dg/edges tree)))
        "}"))
  ([text-vec ends tree]
   (str "digraph {\n"
        (if final-dot-formatting
            (str "\tnode [shape=point];\n\tnode [label=""];\n\troot [width=0.1];\n"))
        (apply str
               (map (partial dot-edge-str tree text-vec ends)
                    (dg/edges tree)))
        "}")))

(defn make-dot-tree
  "Runs the algorithm and directly outputs a DOT format tree."
  ([& strings]
   (tree-to-dot (apply combine-with-terminators strings)
                (apply find-ends strings)
                (apply make-suffix-tree strings))))

(defn print-dot-tree
  "Runs the algorithm and directly prints a DOT format tree to the console."
  ([& strings]
   (println (apply make-dot-tree strings))))

(defn view-dot
  "Runs the algorithm and opens a PDF window viewing the graph. Only works on Mac OS X; for debugging purposes only."
  ([string filename]
   (do (shell/sh "dot" "-Tpdf" (str "-o" filename ".pdf") :in string)
       (shell/sh "open" "preview.pdf")
       nil))
  ([string]
   (view-dot string "preview")))

(defn debug-dot-tree
  "Runs the algorithm and directly prints a DOT format tree to the console, along with intermediate debugging information as it is building the tree. Equivalent to running with the var debug set to true."
  ([& strings]
   (with-redefs [debug true]
                (let [d-t (apply make-dot-tree strings)]
                     (println d-t)
                     (view-dot d-t)))))
