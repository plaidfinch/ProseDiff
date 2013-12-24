ProseDiff
=========

Work in progress on a semantically-aware diff tool for natural-language (e.g. English) prose. The idea is to display changes to text in a way that is meaningful to human authors, operating not line by line, but on semantic units of text.

Work on this project is currently stalled, as it seems that a truly optimal solution would require a fast solution to the Minimum Common String Partition problem, which has been shown to be NP-hard and not only that, but also APX-hard. These results mean that work will need to proceed in a radically different direction to be able to meet reasonable (i.e. polynomial) efficiency goals.
