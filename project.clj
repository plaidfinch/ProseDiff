(defproject ProseDiff "0.5-ALPHA"
  :description "Generate a human-friendly visualization of revisions made between two drafts of a piece of (English) prose."
  :dependencies [[org.clojure/clojure "1.4.0"]]
  :plugins [[lein-cljsbuild "0.2.9"]]
  :source-path "src"
  :cljsbuild
  {:builds
   [{:source-path "src-cljs",
     :compiler
     {:output-to "prosediff.js",
      :optimizations :whitespace,
      :pretty-print true}}]})


