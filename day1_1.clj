(ns skydev.aoc)

(def input (with-open [r (clojure.java.io/reader "day1_input.txt")]
             (map #(if (= % "") nil (Integer/parseInt %))
                  (doall (line-seq r)))))

(defn inventories [data]
  (let [p (partition-by #(if (nil? %) 1 2) data)
        f (filter #(not (nil? (first %))) p)]
    f))

(defn solution1 [input]
  (let [ivt (inventories input)
        calories (map #(apply + %) ivt)]
    (apply max calories)))

(defn solution2 [input]
  (let [ivt (inventories input)
        calories (map #(apply + %) ivt)]
    (apply + (take 3 (reverse (sort calories))))))
