(ns prosediff.utilities)

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

(defn bool-to-int
  "Maps true -> 1, false -> 0."
  ([b] (if b 1 0)))

(defn int-to-bool
  "Maps 0 -> false, anything else to true."
  ([i]
   {:pre [(number? i)]}
   (if (= i 0) false true)))

(defn xor
  "Takes the exclusive or of its two arguments."
  ([a b]
   (int-to-bool (apply bit-xor (map bool-to-int [a b])))))

(defn safe-subvec
  "Takes the subvec of the vector given, but allowing out-of-range specifications and backwards indices."
  ([v start end]
   (subvec v (clip 0 (count v) (min start end))
             (clip 0 (count v) (max start end))))
  ([v start]
   (safe-subvec v start (count v))))
