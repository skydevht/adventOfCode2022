(ns skydev.aoc.utils)

(defn get-lines [file]
  (with-open [r (clojure.java.io/reader file)]
    (doall (line-seq r))))
