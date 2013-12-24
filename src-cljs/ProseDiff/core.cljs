(ns prosediff.core
    (:use [prosediff.directed-graph :only [make-graph]]
          [prosediff.ukkonen-suffix-tree :only [make-graph]]))

(defn log [item]
  (.log js/console (pr-str item)))

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

(defn ^:export test-1 []
  (log (make-graph [[1 2] [2 3]])))
