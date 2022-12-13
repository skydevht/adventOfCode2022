(ns skydev.aoc.day5
  (:require [skydev.aoc.utils :refer :all]))

(def input
  (let [desc (get-lines "day5_input.txt")]
    (let [data (partition-by #(if (clojure.string/blank? %) 1 2) desc)]
      (list (first data) (nth data 2)))))

(defn parse [program]
  (letfn [(parse-line [line]
            (map #(Integer/parseInt %)
                 (clojure.string/split (clojure.string/replace line
                                                               #"move (\d+) from (\d+) to (\d+)"
                                                               "$1 $2 $3")
                                       #" ")))]
    (map parse-line program)))

(defn build [memory]
  (let [size (count (first memory))]
    (letfn [(rf [rep line]
              (loop [c 0
                     m rep]
                (if (> (inc (* c 4)) (- size 1))
                  m
                  (recur (+ c 1)
                         (let [stack (get m (inc c) '())
                               label (nth line (inc (* c 4)))]
                           (assoc m (inc c) (if (= \space label)
                                         stack
                                         (conj stack label))))))
                ))]
      (reduce rf {} memory))))

(defn model1 [cases]
  cases)

(defn model2 [cases]
  (reverse cases))

(defn evaluate
  "Returns the new memory representation after executing an instruction"
  [instruction memory model]
  (let [[amount from to] instruction
        from-stack-before (get memory from '())
        cases (take amount from-stack-before)
        from-stack-after (take-last (- (count from-stack-before) amount)
                                    from-stack-before)
        to-stack-after (apply conj (get memory to '()) (model cases))]
    (assoc memory from from-stack-after to to-stack-after)))

(defn solution [input model]
  (let [memory (build (pop (reverse (first input))))
        program (parse (second input))
        memory-after-run (reduce #(evaluate %2 %1 model) memory program)]
    (clojure.string/join (map #(str (first (get memory-after-run % '())))
          (sort (keys memory-after-run))))))

(defn solution1 [input]
  (solution input model1))

(defn solution2 [input]
  (solution input model2))
