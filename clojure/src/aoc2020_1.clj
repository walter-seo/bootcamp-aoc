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

;; 원래 문제에서는 정확히 하나의 답만 나온다고 가정됨
;; 여기서는 여러개 답이 나올 수 있다고 생각하고 풀어보았음
;; 숫자 중복 고려 안함

(defn find-sum-pairs
  "numbers에서 합이 target-sum가 되는 2개 쌍 찾기"
  [target-sum numbers]
  (let [present-number-set (frequencies numbers)]
    (->> numbers
         (map #(when (present-number-set (- target-sum %)) #{% (- target-sum %)}))
         (filter identity)
         (distinct))))

(comment
  (let [res-pairs (find-sum-pairs 2020 real-input-numbers)]
    (prn res-pairs)
    (map product (seq res-pairs))))

;; Part 2
;; 3개 숫자 합이 2020
;;

(defn find-sum-set-by-comb
  "numbers에서 합이 target-sum가 되는 length개 숫자 set 찾기"
  [length target-sum numbers]
  (filter #(= (sum %) target-sum) (combo/combinations numbers length)))

(defn find-sum-set-using-map
  "경우의 수 탐색을 map으로 하여 최적화한 version"
  [length target-sum numbers]
  (let [make-sum-group-map (group-by sum (combo/combinations numbers (dec length)))
        ->item-map (fn [number] {:number number :make-sum-group (make-sum-group-map (- target-sum number))})
        include? (fn [rest-group item] (some #(= % item) rest-group))
        remove-duplicates-in-targets (fn [{:keys [number-one make-sum-group]}]
                                       {:number number-one :make-sum-group (remove #(include? % number-one) make-sum-group)})
        add-item-each #(map (fn [l] (conj l %2)) %1)]
    (->> numbers
         (map ->item-map) ;; LazySeq
         (map remove-duplicates-in-targets) ;; LazySeq
         (filter :make-sum-group)
         (mapcat #(add-item-each (% :make-sum-group) (% :number)))
         (map set)
         (distinct))))

(comment
  (group-by sum (combo/combinations real-input-numbers 2))

  (find-sum-set-using-map 3 2020 real-input-numbers)

  (let [res-sets (find-sum-set-by-comb 3 2020 real-input-numbers)]
    (prn res-sets)
    (map product res-sets)))
