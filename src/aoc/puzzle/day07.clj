(ns aoc.puzzle.day07
  (:require
   [clojure.java.io :as io]
   [clojure.walk :as walk]
   [medley.core :as medley]))


(defn load-example
  []
  (with-open [rdr (io/reader (io/resource "input/day07.example.txt"))]
    (into [] conj (line-seq rdr))))

(defn load-data
  []
  (with-open [rdr (io/reader (io/resource "input/day07.txt"))]
    (into [] conj (line-seq rdr))))


(defn file-description
  [s]
  (when-let [[[_ size name]] (re-seq #"^(\d+) ([a-z.]+)$" s)]
    [(keyword name) (read-string size)]))

(defn cd-directory
  [s]
  (when-let [[[_ name]] (re-seq #"^\$ cd ([a-z./]+)$" s)]
    (keyword name)))

(def file-system (atom nil))

(defn build-file-system!
  [current-route [cmd & cmds]]
  (cond
    (nil? cmd)             :ok
    (file-description cmd) (let [[name size] (file-description cmd)]
                             (do
                               (swap! file-system assoc-in (conj current-route name) size)
                               (build-file-system! current-route cmds)))
    (cd-directory cmd)     (let [name (cd-directory cmd)]
                             (case name
                               :.. (build-file-system! (pop current-route) cmds)
                               :/  (build-file-system! [:/] cmds)
                               (build-file-system! (conj current-route name) cmds)))
    :else                  (build-file-system! current-route cmds)))


(defn add-size
  [node]
  (if (map? node)
    (let [dirs  (medley/filter-vals map? node)
          files (medley/filter-vals int? node)
          size  (+
                 (reduce (fn [acc [_name content]] (+ acc (:size content))) 0 dirs)
                 (reduce (fn [acc [_name size]] (+ acc size)) 0 files))]
      (assoc dirs :size size))
     node))


(defn size-big-directories
  []
  (reset! file-system nil)
  (build-file-system! [] (load-data))
  (->> @file-system
       (walk/postwalk add-size)
       :/
       (walk/postwalk #(if (map? %) (seq %) %))
       flatten
       (filter int?)
       (filter #(< % 100000))
       (reduce +)))

(comment
 (size-big-directories))



(defn size-victim-directory
  []
  (reset! file-system nil)
  (build-file-system! [] (load-data))
  (let [file-system-sizes (walk/postwalk add-size @file-system)
        total-size        70000000
        needed            30000000
        used              (get-in file-system-sizes [:/ :size])
        needed-to-free    (- needed (- total-size used))]
    (->> file-system-sizes
         (walk/postwalk #(if (map? %) (seq %) %))
         flatten
         (filter int?)
         sort
         (drop-while #(< % needed-to-free))
         first)))

(comment
 (size-victim-directory))
