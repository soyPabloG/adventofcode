(ns aoc.puzzle.day08
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [medley.core :as medley]))


(defn load-example
  []
  (with-open [rdr (io/reader (io/resource "input/day08.example.txt"))]
    (into [] conj (line-seq rdr))))

(defn load-data
  []
  (with-open [rdr (io/reader (io/resource "input/day08.txt"))]
    (into [] conj (line-seq rdr))))


(defn char->int
  [c]
  (Character/digit c 10))


(defn grid-size
  [data]
  (count (first data)))

(defn tree-vec
  "Returns a representation of the map as a vector of vectors"
  [data]
  (->> data
       (map seq)
       (mapv #(mapv char->int %))))


(defn coordinates
  "[x, y]"
  [grid-size idx]
  [(quot idx grid-size) (mod idx grid-size)])


(defn neighbors
  "Returns the neighbors of a tree sorted by proximity.
   [up left right down]"
  [tree-vec grid-size idx]
  (let [[x y]       (coordinates grid-size idx)
        tree-height (get-in tree-vec [x y])
        up          (->> tree-vec (take x) (map #(get % y)) reverse)
        down        (->> tree-vec (drop (inc x)) (map #(get % y)))
        left        (-> tree-vec (get x) (#(take y %)) reverse)
        right       (-> tree-vec (get x) (#(drop (inc y) %)))]
    [up left right down]))


(defn visible?
  [tree-height neighbors]
  (->> neighbors
       (map #(every? (fn [neighbor] (> tree-height neighbor)) %))
       (some true?)))


(defn visible-trees
  [data]
  (let [grid-size (grid-size data)
        tree-vec  (tree-vec data)]
    (count
     (for [idx   (range (Math/pow grid-size 2))
           :let  [coordinate (coordinates grid-size idx)]
           :when (visible? (get-in tree-vec coordinate) (neighbors tree-vec grid-size idx))]
       idx))))

(comment
 (visible-trees (load-data)))



(defn take-until
  "https://clojure.atlassian.net/browse/CLJ-1451"
  [p s]
  (transduce (halt-when p (fn [r h] (conj r h))) conj [] s))


(defn viewing-distance
  [tree-height neighbors]
  (->> neighbors
       (take-until #(>= % tree-height))
       count))


(defn scenic-score
  [tree-height neighbors]
  (->> neighbors
       (map #(viewing-distance tree-height %))
       (reduce *)))


(defn highest-scenic-score
  [data]
  (let [grid-size (grid-size data)
        tree-vec  (tree-vec data)]
    (->> (range (Math/pow grid-size 2))
         (map (fn [idx] [(get-in tree-vec (coordinates grid-size idx)) (neighbors tree-vec grid-size idx)]))
         (map #(apply scenic-score %))
         (apply max))))

(comment
 (highest-scenic-score (load-data)))
