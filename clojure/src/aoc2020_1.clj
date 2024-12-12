(ns aoc2020-1
  (:require
   [clojure.string :as str]
   [clojure.math.combinatorics :as combo]))

;; 합이 2020이 되는 숫자들 찾기

(def sample-input-numbers [1721 979 366 299 675 1456])

(def real-input-numbers
  (mapv parse-long (str/split-lines (slurp "resources/2020/day1.input.txt"))))

;; Part 1
;; 2개 숫자 합이 2020

(def product #(reduce * %))
(def sum #(reduce + %))

(defn find-sum-pairs
  "coll에서 합이 x가 되는 2개 쌍 찾기"
  [x coll]
  (let [num-map (frequencies coll)]
    (->> coll
         (map #(when (num-map (- x %)) #{% (- x %)}))
         (filter identity)
         (distinct))))

(defn find-sum-set-by-comb
  "coll에서 합이 x가 되는 n개 숫자 set 찾기"
  [n x coll]
  (filter #(= (sum %) x)
          (combo/combinations coll n)))

(defn find-sum-set-using-map
  [n x coll]
  (let [sum-group (group-by sum (combo/combinations coll (- n 1)))
        add-item-each #(map (fn [l] (conj (set l) %2)) %1)]
    (->> coll
         (map #(add-item-each (sum-group (- x %)) %))
         (remove empty?)
         (flatten)
         (distinct))))

(comment
  (let [res-pairs (find-sum-pairs 2020 real-input-numbers)]
    res-pairs
    #_(map product (seq res-pairs))))

;; Part 2
;; 3개 숫자 합이 2020
;;
(comment
  (group-by sum (combo/combinations real-input-numbers 2))

  (find-sum-set-using-map 3 2025 real-input-numbers)

  (let [res-sets (find-sum-set-by-comb 3 2025 real-input-numbers)]
    res-sets
    #_(map product res-sets)))