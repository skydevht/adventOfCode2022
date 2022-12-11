(ns skydev.aoc.day2
  (:require [skydev.aoc.utils :refer :all]))

(def input
  "Holds a list. Each element is a list with two strings (one character)"
  (let [plays (get-lines "day2_input.txt")]
    (map #(clojure.string/split % #" ") plays)))

(defn strat-map
  "transform a play into keyword"
  [play]
  (cond (= play "A") :rock
        (= play "B") :paper
        (= play "C") :scissor
        (= play "X") :rock
        (= play "Y") :paper
        (= play "Z") :scissor))

(def play-point
  "Returns the point for the play"
  {:rock 1
   :paper 2
   :scissor 3})

(def round-point
  "Returns the point for the round"
  {:lose 0
   :tie 3
   :win 6})

(defn round-result
  "Returns if I win, tie, or lose"
  [other mine]
  (cond (= mine other) :tie
        (and (= mine :paper) (= other :rock)) :win
        (and (= mine :paper) (= other :scissor)) :lose
        (and (= mine :scissor) (= other :paper)) :win
        (and (= mine :scissor) (= other :rock)) :lose
        (and (= mine :rock) (= other :scissor)) :win
        (and (= mine :rock) (= other :paper)) :lose))

(defn score
  "Returns the player score for a round"
  [round]
  (+ ((round-result (first round)
                    (second round)) round-point)
     ((second round) play-point)))

(defn solution1 [input]
  (let [rounds (map #(list (strat-map (first %))
                           (strat-map (second %)))
                    input)]
    (reduce #(+ %1 (score %2)) 0 rounds)))

(defn strat-map2 [play]
  (cond (= play "X") :lose
        (= play "Y") :tie
        (= play "Z") :win))

(defn round-play
  "select play according to opponent and intended target"
  [other target]
  (cond (= target :tie) other
        (and (= target :lose) (= other :rock)) :scissor
        (and (= target :lose) (= other :scissor)) :paper
        (and (= target :lose) (= other :paper)) :rock
        (and (= target :win) (= other :rock)) :paper
        (and (= target :win) (= other :scissor)) :rock
        (and (= target :win) (= other :paper)) :scissor))

(defn score2 [round]
  (let [other (first round)
        target (second round)]
    (score (list other (round-play other target)))))

(defn solution2 [input]
  (let [rounds (map #(list (strat-map (first %))
                           (strat-map2 (second %)))
                    input)]
    (reduce #(+ %1 (score2 %2)) 0 rounds)))
