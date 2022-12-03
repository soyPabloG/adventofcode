(ns aoc.puzzle.day02
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))


(defn load-data
  []
  (with-open [rdr (io/reader (io/resource "input/day02.txt"))]
    (into [] conj (line-seq rdr))))


(defn decode-1
  [[p1 p2]]
  (let [code {"A" :rock
              "B" :paper
              "C" :scissors
              "X" :rock
              "Y" :paper
              "Z" :scissors}]
    [(code p1) (code p2)]))

(defn r-p-s
  [[ours theirs]]
  (cond
    (= ours theirs)             :draw
    (some #(= [ours theirs] %)
          [[:rock :scissors]
           [:paper :rock]
           [:scissors :paper]]) :won
    :else                       :loss))

(def points
  {:rock     1
   :paper    2
   :scissors 3
   :loss     0
   :draw     3
   :won      6})

(defn score
  [[ours _theirs result]]
  (+ (points ours) (points result)))


(defn total-score-1
  []
  (->> (load-data)
       (map #(str/split % #" "))
       (map decode-1)
       (map (fn [[theirs ours]] [ours theirs (r-p-s [ours theirs])]))
       (map score)
       (apply +)))

(comment
 (total-score-1))



(defn decode-2
  [[p1 p2]]
  (let [code {"A" :rock
              "B" :paper
              "C" :scissors
              "X" :loss
              "Y" :draw
              "Z" :won}]
    [(code p1) (code p2)]))

(defn guess-game
  [[theirs result]]
  (case result
    :draw theirs
    :won  (case theirs
            :rock     :paper
            :paper    :scissors
            :scissors :rock)
    :loss (case theirs
            :rock     :scissors
            :paper    :rock
            :scissors :paper)))

(defn total-score-2
  []
  (->> (load-data)
       (map #(str/split % #" "))
       (map decode-2)
       (map (fn [[theirs result]] [(guess-game [theirs result]) theirs result]))
       (map score)
       (apply +)))

(comment
 (total-score-2))
