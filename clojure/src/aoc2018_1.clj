(ns aoc2018-1
  (:require [clojure.string :as str]))

;; 파트 1
;; 주어진 입력의 모든 숫자를 더하시오.
;; 예) +10 -2 -5 +1 이 입력일 경우 4를 출력

(defn ->numbers
  "convert input string to number list"
  [input]
  (->> (str/split input #",?[\s]")
       (map parse-long)))

(defn solve-1 [input]
  (->> input
       ->numbers
       (apply +)))

(def sample-input "+3, +3, +4, -2, -4")
(def real-input (slurp "resources/day1.input.txt"))

(comment
  (solve-1 real-input))
;; 파트 2
;; 주어진 입력의 숫자를 더할 때 마다 나오는 숫자 중, 처음으로 두번 나오는 숫자를 리턴하시오.
;; 예) +3, +3, +4, -2, -4 는 10이 처음으로 두번 나오는 숫자임.
;; 0 -> 3 (+3) -> 6 (+3) -> 10(+4) -> 8(-2) -> 4(-4) -> 7(+3) -> 10(+3) -> ...
(defn find-first-dup
  "주어진 리스트에서 첫 번째로 중복되는 숫자 반환"
  [sum-iter]
  (loop [seen #{}
         [x & remain] sum-iter]
    (if (seen x) x
        (recur (conj seen x) (rest remain)))))

;; cycle로 무한반복 돌리고 reduce 대신 누적하면서 더하는 
;; reductions 로 누적 수열 만들고 그중 첫번째로 중복되는걸 재귀로 탐색
(defn solve-2
  "loop로 풀이"
  [input]
  (->> input
       (->numbers)
       (cycle)
       (reductions +)
       (find-first-dup)))

;; 다른 풀이 방법
(defn solve-2-drop-while
  "reductions & drop-while로 풀이"
  [input]
  (->> input
       (->numbers)
       (cycle)
       (reductions +)
       (reductions (fn [seen-map curr-sum]
                     (-> seen-map
                         (update :seen #(conj % (:prev seen-map)))
                         (assoc :prev (:curr seen-map))
                         (assoc :curr curr-sum)))
                   {:seen #{}, :prev nil, :curr nil})
       (drop-while #((complement contains?) (% :seen) (% :curr)))
       (first)
       (:curr)))

(comment
  (solve-2 real-input)

  (solve-2-drop-while real-input))
