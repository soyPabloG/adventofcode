(ns aoc.puzzle.day05
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [medley.core :as medley]))


(defn load-data
  []
  (with-open [rdr (io/reader (io/resource "input/day05.txt"))]
    (into [] conj (line-seq rdr))))


(defn parse-row-values
  [s-row]
  (map last (re-seq #"(?:^| )(?:\[(\p{Upper})\])|(?:   )(?:$| )" s-row)))

(defn seq->crates
  [seq]
  (->> seq
       (map (fn [row-values] (map-indexed #(assoc {} %1 %2) row-values)))
       (map #(reduce into {} %))
       (#(conj % {0 nil 1 nil 2 nil 3 nil 4 nil 5 nil 6 nil 7 nil 8 nil})) ;; How to init every stack?
       (apply merge-with conj)
       (medley/map-vals reverse)
       (medley/map-vals #(drop-while nil? %))))

(defn parse-crates
  [data]
  (loop [data   data
         result []]
    (let [row-values (parse-row-values (first data))]
      (if (every? nil? row-values)
        (seq->crates result)
        (recur (rest data) (conj result row-values))))))


(defn parse-move
  [s-move]
  (->> (re-seq #"move (\d+) from (\d) to (\d)" s-move)
       first
       rest
       (map read-string)))

(defn parse-moves
  [data]
  (->> data
       (drop-while #(not (str/starts-with? % "move")))
       (map parse-move)
       (map (fn [[n from to]] [n (dec from) (dec to)]))))


(defn swap-one
  [crates from to]
  (let [choosen (-> crates (get from) first)]
    (-> crates
        (update from rest)
        (update to conj choosen))))

(defn swap-n
  [crates n from to]
  (loop [crates crates
         n      n]
    (if (= 0 n)
      crates
      (recur (swap-one crates from to) (dec n)))))


(defn move-crates
  [crates moves]
  (loop [crates crates
         moves  moves]
    (if (empty? moves)
      crates
      (recur (apply swap-n crates (first moves)) (rest moves)))))


(defn top-cranes
  []
  (let [data   (load-data)
        crates (parse-crates data)
        moves  (parse-moves data)]
    (->> (move-crates crates moves)
         (medley/map-vals first)
         (sort-by first)
         (map rest)
         (map first)
         (apply str))))

(comment
 (top-cranes))



(defn swap-n-9001
  [crates n from to]
  (let [taken-cranes (take n (get crates from))]
    (-> crates
        (update from #(drop n %))
        (update to #(concat taken-cranes %)))))


(defn move-crates-9001
  [crates moves]
  (loop [crates crates
         moves  moves]
    (if (empty? moves)
      crates
      (recur (apply swap-n-9001 crates (first moves)) (rest moves)))))


(defn top-cranes-9001
  []
  (let [data   (load-data)
        crates (parse-crates data)
        moves  (parse-moves data)]
    (->> (move-crates-9001 crates moves)
         (medley/map-vals first)
         (sort-by first)
         (map rest)
         (map first)
         (apply str))))

(comment
 (top-cranes-9001))
