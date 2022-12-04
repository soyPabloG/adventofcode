(ns aoc.puzzle.day04
  (:require
   [clojure.java.io :as io]))


(defn load-data
  []
  (with-open [rdr (io/reader (io/resource "input/day04.txt"))]
    (into [] conj (line-seq rdr))))


(defn parse-ranges
  [s]
  (->> s
       (re-matches #"(\d+)-(\d+),(\d+)-(\d+)")
       rest
       (map read-string)
       (partition 2)))


(defn subrange?
  [[[l1 l2] [l3 l4]]]
  (<= l1 l3 l4 l2))


(defn fully-contains?
  [[r1 r2]]
  (or (subrange? [r1 r2])
      (subrange? [r2 r1])))


(defn fully-contained-pairs
  []
  (->> (load-data)
       (map parse-ranges)
       (map fully-contains?)
       (filter true?)
       count))

(comment
 (fully-contained-pairs))



(defn not-overlaps?
  [[[p1 p2] [p3 p4]]]
  (or (< p2 p3)
      (< p4 p1)))


#_(defn overlaps?
    [[[p1 p2] [p3 p4]]]
    (and (<= p3 p2)
         (<= p1 p4)))
;; ¿overlaps? == not(not-overlaps?)?


(defn overlapped-pairs
  []
  (->> (load-data)
       (map parse-ranges)
       (map not-overlaps?)
       (filter false?)
       count))

(comment
 (overlapped-pairs))
