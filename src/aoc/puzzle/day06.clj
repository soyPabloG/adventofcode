(ns aoc.puzzle.day05
  (:require
   [clojure.java.io :as io]))


(defn load-data
  []
  (with-open [rdr (io/reader (io/resource "input/day06.txt"))]
    (into [] conj (line-seq rdr))))


(defn start-packages-report
  [s]
  (loop [from-beginning 0
         datastream     (seq s)
         count-vec      []]
    (if (empty? datastream)
      count-vec
      (if-let [_start-marker? (apply distinct? (take 4 datastream))]
        (recur 0 (drop 4 datastream) (conj count-vec (+ from-beginning 4)))
        (recur (inc from-beginning) (rest datastream) count-vec)))))

(defn first-start-package
  []
  (-> (load-data)
      first
      start-packages-report
      first))

(comment
 (first-start-package))



(defn start-messages-report
  [s]
  (loop [from-beginning 0
         datastream     (seq s)
         count-vec      []]
    (if (empty? datastream)
      count-vec
      (if-let [_start-marker? (apply distinct? (take 14 datastream))]
        (recur 0 (drop 14 datastream) (conj count-vec (+ from-beginning 14)))
        (recur (inc from-beginning) (rest datastream) count-vec)))))


(defn first-start-message
  []
  (-> (load-data)
      first
      start-messages-report
      first))

(comment
 (first-start-message))
