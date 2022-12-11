(ns skydev.aoc.day4
  (:require [skydev.aoc.utils :refer :all]))

(def input
  (let [tasks (get-lines "day4_input.txt")]
    (map #(map (fn [n] (Integer/parseInt n))
               (clojure.string/split % #",|-"))
         tasks)))

(defn fully-contains? [task]
  (let [[a1 a2 b1 b2] task]
    (or (and (>= a1 b1)
             (<= a2 b2))
        (and (<= a1 b1)
             (>= a2 b2)))))

(defn solution1 [input]
  (count (filter fully-contains? input)))

(defn overlap? [task]
  (let [[a1 a2 b1 b2] task]
    (or (fully-contains? task)
        (and (>= a2 b1)
             (<= a1 b1))
        (and (<= a1 b2)
             (>= a2 b2)))))

(defn solution2 [input]
  (count (filter overlap? input)))
