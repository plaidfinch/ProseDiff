(ns prosediff.nfa
   (:require [prosediff.directed-graph :as dg]
             [clojure.set :as set]))

(defn next-states [graph transition state]
   (set (map dg/edge-end (dg/out-edges graph state transition))))

(defn setify [x]
  (if (set? x) x #{x}))

;; ADD EPSILON TRANSITIONS VIA GRAPH TRAVERSAL

(defn make-nfa [transition-rules start-states]
   (let [transition-graph (dg/make-graph transition-rules)]
      (fn [string]
         (cons (setify start-states)
            (if (empty? string)
                nil
                (lazy-seq
                  ((make-nfa transition-rules
                             (apply set/union
                                    (map (partial next-states transition-graph (first string))
                                         (setify start-states))))
                   (rest string))))))))

(defn string-accepted? [nfa accept-states string]
   (if (some identity
         (for [state accept-states]
            (contains? (last (nfa string)) state)))
       true
       false))
