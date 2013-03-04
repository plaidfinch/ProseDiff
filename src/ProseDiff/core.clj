(ns prosediff.core
    (:require [prosediff.directed-graph      :as dg
               prosediff.ukkonen-suffix-tree :as ust]]))

;; The list of characters considered to be part of a word.
(def word-chars
  #{\A \B \C \D \E \F \G \H \I \J \K \L \M \N \O \P \Q \R \S \T \U \V \W \X \Y \Z
    \a \b \c \d \e \f \g \h \i \j \k \l \m \n \o \p \q \r \s \t \u \v \w \x \y \z
    \0 \1 \2 \3 \4 \5 \6 \7 \8 \9 \' \-})

;; Splits a string to component words, preserving non-word characters as their own words.
; Spaces are removed by default; they may be kept in the output by specifying :strip-spaces false. However, note that running the core matching algorithms on input containing spaces has been shown to have an EXTREMELY LARGE real-life performance cost. This is why spaces are stripped and re-inserted after the matching has completed on the words.
(defn split-to-words [string & {:keys [strip-spaces] :or {strip-spaces true}}]
  (remove #(and strip-spaces (= % " "))
    (map (partial apply str)
         (partition-by #((complement contains?) word-chars %) string))))

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

;; Takes two strings and returns a vector of two (HTML) strings.
(defn ^:export process-strings [left-string right-string]
  (map (partial generate-HTML "span" "pd-move")
       ["pd-delete" "pd-insert"]
       [left-string right-string]
       (tagged-word-lists left-string right-string)))

;; Takes two input filenames and an output filename, applies compare and outputs HTML to the output filename.
(defn ^:export process-files [left-file right-file output-file]
  (let [results (process-strings (slurp left-file)
                                 (slurp right-file))]
       (spit output-file
             (str "<html><head><title>ProseDiff</title><script type=\"text/javascript\" src=\"http://ajax.googleapis.com/ajax/libs/jquery/1.8.2/jquery.min.js\"></script><script type=\"text/javascript\" src=\"page-scripts.js\"></script><link rel=stylesheet href=\"page-style.css\" type=\"text/css\"></link></head><body>"
               "<div class=\"analysis-output\" id=\"left-output\">"
               (first results)
               "</div>"
               "<div class=\"analysis-output\" id=\"right-output\">"
               (second results)
               "</div>"
               "</body></html>"))))
