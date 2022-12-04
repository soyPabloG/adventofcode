(ns aoc.puzzle.day03
  (:require
   [clojure.java.io :as io]
   [clojure.set :as set]))


(defn load-data
  []
  (with-open [rdr (io/reader (io/resource "input/day03.txt"))]
    (into [] conj (line-seq rdr))))


(defn split
  [s]
  (let [mid (quot (count s) 2)]
    (split-at mid s)))


(defn priority
  [c]
  (let [c (int c)]
    (cond
      (<= (int \A) c (int \Z)) (- c 38)
      (<= (int \a) c (int \z)) (- c 96))))


(defn rep-items-priorities
  []
  (->> (load-data)
       (map split)
       (map (fn [[c1 c2]] [(set c1) (set c2)]))
       (map #(apply set/intersection %))
       (map first)
       (map priority)
       (apply +)))

(comment
 (rep-items-priorities))



(defn badges-priorities
  []
  (->> (load-data)
       (partition 3)
       (map (fn [[b1 b2 b3]] [(set b1) (set b2) (set b3)]))
       (map #(apply set/intersection %))
       (map first)
       (map priority)
       (apply +)))

(comment
 (badges-priorities))
