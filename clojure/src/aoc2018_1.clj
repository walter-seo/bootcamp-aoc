(ns aoc2018-1
  (:require [clojure.string :as str]))

;; 파트 1
;; 주어진 입력의 모든 숫자를 더하시오.
;; 예) +10 -2 -5 +1 이 입력일 경우 4를 출력
(comment
  (def sample-input "+3, +3, +4, -2, -4")

  (defn to-numbers
    "convert input string to number list"
    [input]
    (->> (str/split input #",?[\s]")
         (map parse-long)))

  (def sample-input "+3, +3, +4, -2, -4")

  (defn solve-1 [input]
    (->> input
         to-numbers
         (reduce +)))

  (def real-input (slurp "resources/day1.input.txt"))

  (comment
    (solve-1 real-input))

;; (parse-one "-10")

  (defn parse-input [s]
    (->> (str/split s #",?[\s]")
         (map Integer/parseInt)))

  (defn solve-1 [input]
    (->> input
         (parse-input)
         (reduce +)))

  (def real-input (slurp "resources/day1.input.txt"))

  (solve-1 sample-input))
;; 파트 2
;; 주어진 입력의 숫자를 더할 때 마다 나오는 숫자 중, 처음으로 두번 나오는 숫자를 리턴하시오.
;; 예) +3, +3, +4, -2, -4 는 10이 처음으로 두번 나오는 숫자임.
;; 0 -> 3 (+3) -> 6 (+3) -> 10(+4) -> 8(-2) -> 4(-4) -> 7(+3) -> 10(+3) -> ...
(defn find-first-dup
  "주어진 리스트에서 첫 번째로 중복되는 숫자 반환"
  [iter]
  (loop [seen #{} [x & remain] iter]
    (if (seen x) x
        (recur (conj seen x) (rest remain)))))

;; cycle로 무한반복 돌리고 reduce 대신 누적하면서 더하는 
;; reductions 로 누적 수열 만들고 그중 첫번째로 중복되는걸 재귀로 탐색
(defn solve-2 [input]
  (->> input
       (to-numbers)
       (cycle)
       (reductions +)
       (find-first-dup)))

(comment
  (solve-2 real-input))
