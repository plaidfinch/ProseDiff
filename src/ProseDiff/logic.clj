(ns prosediff.logic)

; Some logic...

(defn xor
  "Takes the exclusive or of its two arguments."
  ([a b]
   (or (and (not a) b)
       (and a (not b)))))

(defn iff
  "Logical biconditional. Same as (not (xor a b))."
  ([a b] (not (xor a b))))

(defn implies
  "Material implication. True unless a = false and b = true."
  ([a b] (or a (not b))))

(defn nand
  "Same as (not (and ...))."
  ([& xs] (not (reduce #(and %1 %2) true xs))))

(defn nor
  "Same as (not (or ...))."
  ([& xs] (not (reduce #(or %1 %2) false xs))))
