(ns skydev.aoc.day6
  (:require [skydev.aoc.utils :refer :all]))

(def input (first (get-lines "day6_input.txt")))

(defn solution [input cnt]
  (loop [signal (subs input (dec cnt))
         seen (apply list (subs input 0 (dec cnt)))
         c (dec cnt)]
    (let [ch (first signal)]
      (cond (= 0 (count signal)) -1
            (= cnt
               (count (apply hash-set
                             ch
                             seen))) (inc c)
            true (recur (subs signal 1)
                        (reverse (conj (reverse (rest seen)) ch))
                          (inc c))))))

(defn solution1 [input]
  (solution input 4))

(defn solution2 [input]
  (solution input 14))
