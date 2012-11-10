(defproject ProseDiff "0.5-ALPHA"
  :description "Generate a human-friendly visualization of revisions made between two drafts of a piece of (English) prose."
  :dependencies [[org.clojure/clojure "1.4.0"]]
  :plugins [[lein-cljsbuild "0.2.9"]]
  :source-path "src"
  :cljsbuild {
    :builds [{
        ; The path to the top-level ClojureScript source directory:
        :source-path "src-cljs"
        ; The standard ClojureScript compiler options:
        ; (See the ClojureScript compiler documentation for details.)
        :compiler {
          :output-to "prosediff.js"  ; default: main.js in current directory
          :optimizations :whitespace
          :pretty-print true}}]})
