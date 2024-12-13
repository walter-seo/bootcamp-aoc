(ns aoc2020-8
  (:require
   [clojure.string :as str]))

;; 비행기 옆자리 꼬마의 고장난 게임기를 고치는 문제
;; *Boot code(Your Input)* 에 무한 루프가 고장의 원인

;; Part 1

(def sample-input
  ["nop +0"
   "acc +1"
   "jmp +4"
   "acc +3"
   "jmp -3"
   "acc -99"
   "acc +1"
   "jmp -4"
   "acc +6"])

(def real-input (str/split-lines (slurp "resources/2020/day8.input.txt")))

(defn to-instruction
  "line 한줄 instruction pair로"
  [line]
  (let [[operation argument] (str/split line #" " 2)]
    [(keyword operation) (parse-long argument)]))

(defn run-operation
  "명령어 한줄 실행하여 accumulated value와 next index 반환"
  [acc curr-idx operation argument]
  (case operation
    :jmp [acc (+ curr-idx argument)]
    :acc [(+ acc argument) (inc curr-idx)]
    [acc (inc curr-idx)]))

(defn accumulate-until-cycle-or-terminate
  "첫번째로 두번 실행되는 instruction 나오거나 종료될때까지 simulate하고 acc 반환"
  [instruction-map]
  (loop [curr-idx 0
         acc 0
         seen #{}]
    (let [max-idx (apply max (keys instruction-map))
          [op arg] (instruction-map curr-idx)
          [next-acc next-idx] (run-operation acc curr-idx op arg)
          next-seen (conj seen curr-idx)]
      (cond
        ;; 정상 input 아닐때 예외 조건 체크
        (> next-idx max-idx) [next-acc :term]
        (next-seen next-idx) [next-acc next-idx]
        :else (recur next-idx next-acc next-seen)))))

@(def instruction-map
   (->> sample-input
        (map-indexed #(vector %1 (to-instruction %2)))
        (into {})))
(comment
  (accumulate-until-cycle-or-terminate instruction-map))

;; Part 2 
;; 이제 프로그램을 고쳐보자
;; 하나의 nop or jmp operation 만 문제가 있다.
;; jmp -> nop 나 nop -> jmp 로 바꾸면 infinite loop가 사라짐
;; 문제 해결했을 경우 terminates 시의 accumulator 값 구하기

(defn jmp-or-nop?
  [operation _argument]
  (or (= operation :nop) (= operation :jmp)))

(defn filter-jmp-or-nop-idxs
  "jmp 나 nop 명령줄만 필터링"
  [instruction-map]
  (keys (filter #(apply jmp-or-nop? (val %)) instruction-map)))

(defn switch-jmp-nop
  [[operation argument]]
  (case operation
    :jmp [:nop argument]
    :nop [:jmp argument]))

(defn fix-program
  "문제된 operation을 고쳐서 나온 simulate 결과값 출력"
  [instruction-map]
  (let [target-operations (filter-jmp-or-nop-idxs instruction-map)]
    ;; 예외 처리: 
    ;; alter: map - filter
    (loop [[target-idx & remain] target-operations]
      (let [modified-instruction-map (update instruction-map target-idx switch-jmp-nop)
            [acc last-idx] (accumulate-until-cycle-or-terminate modified-instruction-map)]
        (if (= last-idx :term)
          acc
          (recur remain))))))

(comment
  (->> instruction-map
       fix-program))

