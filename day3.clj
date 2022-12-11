(ns skydev.aoc.day3
  (:require [skydev.aoc.utils :refer :all]))

(def input
  "Holds a list. Each element is a list with two strings (one character)"
  (let [rs (get-lines "day3_input.txt")]
    (map #(let [sp (/ (count %) 2)]
            (list (subs % 0 sp) (subs % sp))) rs)))

(defn priority
  [char]
  (let [a (int char)
        l (int \a) ; a is 97, uppercase is lower
        u (int \A)]; A is 65
    (cond (>= a l) (inc (- a l))
          (>= a u) (+ (- a u) 27))))


(defn common [rs]
  (let [fc (list* (first rs))
        sc (list* (second rs))
        fcmap (reduce #(assoc %1 %2 (inc (get %1 %2 0))) {} fc)]
    (set (filter #(contains? fcmap %) sc))))

(defn solution1 [input]
  (let [commons (map common input)
        priorities (map #(priority (first %)) commons)]
    (reduce + priorities)))

(def input2
  (partition 3 (map clojure.string/join input)))

(defn common2 [group]
  (let [gas (map set group)]
    (first (apply clojure.set/intersection gas))))

(defn solution2 [input]
  (let [commons (map common2 input)
        priorities (map #(priority %) commons)]
    (apply + priorities)))
