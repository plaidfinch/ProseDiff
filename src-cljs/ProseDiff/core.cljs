(ns prosediff.core
    (:require [clojure.string]))

;; The list of characters considered to be part of a word.
(def word-chars
  #{\A \B \C \D \E \F \G \H \I \J \K \L \M \N \O \P \Q \R \S \T \U \V \W \X \Y \Z
    \a \b \c \d \e \f \g \h \i \j \k \l \m \n \o \p \q \r \s \t \u \v \w \x \y \z
    \0 \1 \2 \3 \4 \5 \6 \7 \8 \9 \' \-})

(defn log [item]
  (.log js/console (pr-str item)))

;; Splits a string to component words, preserving non-word characters as their own words.
; Spaces are removed by default; they may be kept in the output by specifying :strip-spaces false. However, note that running the core matching algorithms on input containing spaces has been shown to have an EXTREMELY LARGE real-life performance cost. This is why spaces are stripped and re-inserted after the matching has completed on the words.
(defn split-to-words [string & {:keys [strip-spaces] :or {strip-spaces true}}]
  (remove #(and strip-spaces (= % " "))
    (map (partial apply str)
         (partition-by #((complement contains?) word-chars %) string))))

;; Makes a list of vectors [index word] from a list of words
(defn indexed-word-list [word-list]
    (map vector (iterate inc 0) word-list))

;; Takes a list of objects and an unary function, and generates a hash-table where the keys are the result of the application of that function to each object, and the values are vectors containing the val-func of every object which was mapped to that key.
(defn pigeonhole [L key-func val-func]
  (apply merge-with
         (comp vec flatten vector)
         (map (partial apply hash-map)
              (map #(vector (key-func %) (vector (val-func %)))
                   L))))

;; Maps a function over the values of a dict.
(defn map-over-values [f m]
  (into (empty m) (for [[k v] m] [k (f v)])))

;; Makes a hash map from a list of words with key = word, val = list of indices of that word.
(defn word-indices-map [word-list]
  (pigeonhole (indexed-word-list word-list) second first))

;; Takes two word-lists and returns a list of dicts of [:word <string> :left-index <int> :right-indices (<int>...)] which correspond to all the matching words in those word-lists.
(defn match-graph [left-word-list right-word-list]
  (let [right-word-indices-map (word-indices-map right-word-list)]
    (map (fn [v] {:word          (second v)
                  :left-index    (first v)
                  :right-indices (get right-word-indices-map (second v))})
         (indexed-word-list left-word-list))))

;; Given a node in the graph, returns its first edge.
(defn first-edge [graph-node]
  {:word        (:word graph-node)
   :left-index  (:left-index graph-node)
   :right-index (first (:right-indices graph-node))})

;; Given a node in the graph, returns the a node with the rest of its edges.
(defn rest-edges [graph-node]
  (if (empty? (rest (:right-indices graph-node)))
      {}
      {:word          (:word graph-node)
       :left-index    (:left-index graph-node)
       :right-indices (rest (:right-indices graph-node))}))

;; Returns all edges in a graph-node
(defn graph-node-edges [graph-node]
  (if (empty? (:right-indices graph-node))
      nil
      (cons (first-edge graph-node)
            (graph-node-edges (rest-edges graph-node)))))

;; Converts a sequence of edges into a dict {:left-start <int> :right-start <int> :length <int> :words (<string>...)}
(defn sequence-to-range [s]
  {:left-start  (:left-index  (first s))
   :right-start (:right-index (first s))
   :length      (+ 1 (- (:left-index (last s))
                        (:left-index (first s))))
   :words       (map :word s)})

;; Takes an edge and a sequence; returns a dict of the sequence with the edge appended if it can "stick"; otherwise returns the sequence. Either way, return sequence is tagged as to whether it matches by use of a dict.
(defn match-with-single-sequence [sequence graph-edge]
  (if (and (= (:left-index graph-edge)
              (+ 1 (:left-index (first sequence))))
           (= (:right-index graph-edge)
              (+ 1 (:right-index (first sequence)))))
      {:match (cons graph-edge sequence)}
      {:non-match sequence}))

;; Takes an edge and a list of sequences; returns a a dict which has a list of sequences (:matches) where the edge can "stick" (with the correspondig edge in each sequence), and a list of sequences where the edge could not stick (:non-matches).
(defn match-with-sequences [sequence-list graph-edge]
  (let [results (map #(match-with-single-sequence % graph-edge) sequence-list)]
    (let [matches     (map :match     (filter :match     results))
          non-matches (map :non-match (filter :non-match results))]
         (if (empty? matches)
             ; new sequences are started when an edge doesn't match any live sequences
             {:new         (list (list graph-edge))
              :non-matches non-matches}
             {:matches     matches
              :non-matches non-matches}))))

;; Given a graph node and a list of possibly live sequences, returns a dict --- :matches - only the sequences which can be continued using an edge from the node, with the new edges attached to them, and :non-matches - the set of sequences which could not be extended from the node.
(defn extend-sequences-from-node [sequence-list graph-node]
  (loop [edges (graph-node-edges graph-node)
         results {:matches     nil
                  :non-matches sequence-list}]
        (if (empty? edges)
            results
            (let [new-results (match-with-sequences (:non-matches results)
                                                    (first edges))]
              ;(println new-results)
              (recur (rest edges)
                     {:matches (concat (:matches results)
                                       (:matches new-results)
                                       (:new     new-results))
                      :non-matches (:non-matches new-results)})))))

;; Returns a list of longest-common-subsequences in the two strings, which may possibly contain overlapping sequences.
(defn find-common-subsequences [left-string right-string]
  (log (str "Matching strings of lengths " (count left-string) " (left) and " (count right-string) " (right)."))
  (map reverse
    (let [m-g (apply match-graph
                     (map (comp split-to-words #(.toLowerCase %))
                          [left-string right-string]))]
      (log (str "Edges in graph: " (reduce + 0 (map (comp count :right-indices) m-g))))
      (loop [graph m-g
             live     nil
             complete nil]
            (if (empty? graph)
                (concat live complete)
                (let [new-sequences (extend-sequences-from-node live (first graph))]
                  (recur (rest graph)
                         (:matches new-sequences)
                         (concat complete
                                 (:non-matches new-sequences)))))))))

;; Takes two ranges (A, B) and returns true iff A's nodes are a subset of B's nodes.
(defn both-sides-range-subset? [A-range B-range]
  (and (>= (:left-start A-range)
           (:left-start B-range))
       (>= (:right-start A-range)
           (:right-start B-range))
       (<= (+ (:left-start A-range) (:length A-range))
           (+ (:left-start B-range) (:length B-range)))))

;; Filters a list of ranges to find only the ranges which map to the largest possible set of words.
(defn remove-redundant-ranges [ranges-list]
  (log (str "Unfiltered matches: " (count ranges-list)))
  (let [r-map (pigeonhole ranges-list :left-start identity)]
    (remove empty?
      (map (comp (fn [pair]
                   ; For a pair of [range (range, range...)], returns nil if the first range is a subset of any of the others, so long as the found superset is not the test range itself. If none of the possible supersets is verified, returns the initial range.
                    (if (empty?
                          (filter #(and (both-sides-range-subset? (first pair) %)
                                        (not (= % (first pair))))
                                  (second pair)))
                        (first pair)
                        nil))
                   ; Possible supersets of any range *r* can only begin on indices which are between (r.start - r.length) to (r.start + r.length), inclusive. We can thus narrow our set of comparisons to those inside this range. Thus, this improved function operates not in O(n^2) time, but in O(mn) time - where m is the average  length of a range, and n is the number of ranges.
                    (juxt identity
                          (comp (partial apply concat)
                                (partial map #(get r-map %))
                                (partial apply #(range (- %1 %2) (+ 1 %1 %2)))
                                (juxt :left-start :length))))
                (apply concat (vals r-map))))))

;; Takes a list of dicts and gives each one a unique numeric ID.
(defn attach-IDs [dict-list]
  (log (str "Filtered matches: " (count dict-list)))
  (let [id-list (range (count dict-list))]
       (loop [list-no-ids   dict-list
              ids           id-list
              list-with-ids nil]
             (if (empty? list-no-ids)
                 list-with-ids
                 (recur (rest list-no-ids)
                        (rest ids)
                        (cons (conj (first list-no-ids) {:block-id (first ids)})
                              list-with-ids))))))

;; Filters out singleton common words from being matched.
(def common-words-threshold
  2) ; <- Threshold for "common-ness." Any singleton words which appear in the left or right text more than this number of times (inclusive) are filtered.
(defn remove-spurious [left-string right-string ranges-list]
  (let [[left-counts right-counts]
          (map (fn [string]
                  ((comp (partial map-over-values count)
                         word-indices-map
                         split-to-words
                         #(.toLowerCase %))
                 string))
          [left-string right-string])]
       (filter (fn [r] (or (<= (or (get left-counts  (comp str :words r)) 0)
                               common-words-threshold)
                           (<= (or (get right-counts (comp str :words r)) 0)
                               common-words-threshold)))
               ranges-list)))

;; Returns a list of ranges which have been properly filtered and conflict-resolved so that they can be formatted as HTML without issue.
(defn filtered-match-ranges [left-string right-string]
  ((comp attach-IDs
         remove-redundant-ranges
         (partial remove-spurious left-string right-string)
         (partial map sequence-to-range)
         find-common-subsequences)
  left-string right-string))

;; Generates a pair of sorted lists of words, such that each word is tagged with an ID corresponding to the match-block to which it belongs.
(defn tagged-word-lists [left-string right-string]
  ; Function which is used to *reduce* across the ranges-with-IDs list which adds each range's ID to all the words in the range.
  (defn id-labeling-fold-func [side words-index-map block-range]
         (let [indices (range (side block-range)
                              (+ (side block-range) (:length block-range)))]
              (reduce (fn [w-d indx]
                        (update-in w-d
                                   [indx :block-ids]
                                   (fn [L] (cons (:block-id block-range) L))))
                       words-index-map
                       indices)))
  ; This big let block defines ranges-with-IDs by calling filtered-match-ranges, and the left and right words' "index maps." The latter is a sorted map where the key is the index of the word in its respective string, and the value is a dict of form {:word <string> :block-ids <seq>}.
  (let [ranges-with-IDs
          (filtered-match-ranges left-string right-string)
        [left-words-index-map right-words-index-map]
          (map (fn [string]
                 (into (sorted-map)
                   (map (fn [pair] [(first pair) {:word (second pair) :block-ids nil}])
                        (indexed-word-list (split-to-words string)))))
               [left-string right-string])]
       ; The anonymous function here is to reduce repetition of code. Simply takes a "side" (which is just the selector to get that side's index out of a range object) and a word-index-map, and does the whole reduce-thing with each of those parameter pairs, thus generating a vector of two elements.
       (map (fn [side words-index-map]
              (vals
                (reduce (partial id-labeling-fold-func side)
                        words-index-map
                        ranges-with-IDs)))
            [:left-start          :right-start]
            [left-words-index-map right-words-index-map])))

;; Reinserts spaces into words based on spaces in original string.
(defn reinsert-spaces [t-w-list string]
  (map (fn [tagged spacified]
         (conj tagged spacified))
       t-w-list
       (map (fn [w] {:word w})
         (loop [w-s (split-to-words string :strip-spaces false)
                n-s (split-to-words string)
                result nil]
           (if (empty? w-s)
               (reverse result)
               (if (= (first w-s) (first n-s))
                   (recur (rest w-s)
                          (rest n-s)
                          (cons (first w-s) result))
                   (recur (rest w-s)
                          n-s
                          (cons (str (first result) (first w-s))
                                (rest result)))))))))

;; Takes a single fully processed tagged word and transforms it into HTML.
(defn tagged-word-to-HTML [element move-class nil-class word]
  (str "<" element " class=\""
    (apply str (cons (if (empty? (:block-ids word))
                         nil-class
                         move-class)
                     (map #(str " pd-block-" %)
                          (:block-ids word))))
    "\">"
    (clojure.string/replace (:word word) "\n" "<br/>")
    "</" element ">"))

;; Transforms a fully processed tagged word list into an HTML string.
(defn generate-HTML [element move-class nil-class string tagged-words]
  (apply str (map (partial tagged-word-to-HTML element move-class nil-class)
                  (reinsert-spaces tagged-words string))))

;; From https://gist.github.com/1658431
(defn clj-js
  "Recursively transforms ClojureScript maps into Javascript objects,
   other ClojureScript colls into JavaScript arrays, and ClojureScript
   keywords into JavaScript strings."
  [x]
  (cond
    (string? x) x
    (keyword? x) (name x)
    (map? x) (.-strobj (reduce (fn [m [k v]]
               (assoc m (clj-js k) (clj-js v))) {} x))
    (coll? x) (apply array (map clj-js x))
    :else x))

;; Takes two strings and returns a vector of two (HTML) strings.
(defn ^:export process [left-string right-string]
  (clj-js
    (map (partial generate-HTML "span" "pd-move")
         ["pd-delete" "pd-insert"]
         [left-string right-string]
         (tagged-word-lists left-string right-string))))
