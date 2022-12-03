(ns aoc.puzzle.day01
  (:require
   [clojure.java.io :as io]))


(defn load-data
  []
  (with-open [rdr (io/reader (io/resource "input/day01.txt"))]
    (into [] conj (line-seq rdr))))


(defn max-calories
  []
  (->> (load-data)
       (partition-by empty?)
       (remove #(some empty? %))
       (map #(map read-string %))
       (map #(reduce + %))
       (apply max)))

(comment
 (max-calories))



(defn top-three-calories
  []
  (->> (load-data)
       (partition-by empty?)
       (remove #(some empty? %))
       (map #(map read-string %))
       (map #(reduce + %))
       (sort >)
       (take 3)
       (apply +)))

(comment
 (top-three-calories))
