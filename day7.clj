(ns skydev.aoc.day7
  (:require [skydev.aoc.utils :refer :all]))

(def input (get-lines "day7_input.txt"))

(defn match-command [cmd]
  (if (= (first cmd) \c)
    (list :cd (subs cmd 3))
    '(:ls)))

(defn match-output [line]
  (if (= (first line) \d)
    (list :dir (subs line 4))
    (let [file-info (clojure.string/split line #" ")
          file-size (Integer/parseInt (first file-info))
          file-name (second file-info)]
      (list :file file-size file-name))))

(defn match-line [line]
  (if (= (first line) \$)
    (match-command (subs line 2))
    (match-output line)))

(defn tokenize [lines]
  (map match-line lines))

(defn consume-ls [fs output]
  (let [ls? (fn [line] (or (= :dir (first line))
                           (= :file (first line))))
        rd (fn [acc el]
             (let [md (if (= :dir (first el))
                        {:type :dir
                         :name (second el)}
                        {:type :file
                         :size (second el)
                         :name (nth el 2)})]
               (assoc acc (get md :name) md)))
        temp-fs (reduce rd fs (take-while ls? output))
        out2 (drop-while ls? output)]
    (list temp-fs out2)))

(defn build-dir [fs output]
  (loop [cd fs
         out2 output]
    (let [next (first out2)]
      (cond (nil? next) (list cd)
            (= :cd (first next)) (if (= ".." (second next))
                                   (list cd (drop 1 out2))
                                   (let [dfs (get cd (second next))
                                         ret (build-dir dfs (drop 1 out2))]
                                     (recur (assoc cd (second next) (first ret))
                                            (second ret))))
            (= :ls (first next)) (let [ret (consume-ls cd (drop 1 out2))]
                                   (recur (first ret) (second ret)))))))

(defn visit [fs]
  (reduce #(if (= :file (get %2 :type))
             (assoc %1 :size (+ (get %1 :size 0)
                              (get %2 :size)))
             (let [ret (visit %2)
                   ds (get ret :size)]
               (assoc %1
                      :size (+ (get %1 :size 0)
                               ds)
                      :tt (+ (get ret :tt 0)
                             (get %1 :tt 0)
                             (if (>= 100000 ds)
                               ds
                               0)))))
          {:size 0 :tt 0}
          (map #(get fs %) (filter string? (keys fs)))))

(defn solution1 [input]
  (let [tokens (tokenize input)
        fs (first (build-dir {} (drop 1 tokens)))]
    (get (visit fs) :tt)))
