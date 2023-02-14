(ns aoc.puzzle.day09
  (:require
   [clojure.java.io :as io]))


(defn load-data
  []
  (with-open [rdr (io/reader (io/resource "input/day09.txt"))]
    (into [] conj (line-seq rdr))))


(defn move-head
  [[hx hy] direction]
  (case direction
    :up    [(inc hx) hy]
    :down  [(dec hx) hy]
    :left  [hx (dec hy)]
    :right [hx (inc hy)]))


(defn move-tail
  [[hx hy :as head] [tx ty :as tail]]
  [head
   (cond
     (= [(+ hx 2) hy] tail)       [(inc tx) ty]
     (= [(- hx 2) hy] tail)       [(dec tx) ty]
     (= [hx (+ hy 2)] tail)       [tx (inc ty)]
     (= [hx (- hy 2)] tail)       [tx (dec ty)]
     (= [(+ hx 2) (+ hy 1)] tail) [(inc tx) (inc ty)]
     (= [(+ hx 2) (- hy 1)] tail) [(inc tx) (dec ty)]
     (= [(- hx 2) (+ hy 1)] tail) [(dec tx) (inc ty)]
     (= [(- hx 2) (- hy 1)] tail) [(dec tx) (dec ty)]
     (= [(+ hx 1) (+ hy 2)] tail) [(inc tx) (inc ty)]
     (= [(- hx 1) (+ hy 2)] tail) [(dec tx) (inc ty)]
     (= [(+ hx 1) (- hy 2)] tail) [(inc tx) (dec ty)]
     (= [(- hx 1) (- hy 2)] tail) [(dec tx) (dec ty)]
     :else                        tail)])


(defn move-rope
  [[head tail :as rope] direction]
  (as-> rope $
        (update $ 0 move-head direction)
        (apply move-tail $)))

(comment
 (move-rope [[1 0] [0 0]] :up))
