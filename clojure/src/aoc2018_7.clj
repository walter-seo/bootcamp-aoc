(ns aoc2018-7
  (:require
   [clojure.set :as set]
   [clojure.string :as str]))

;; Part 1
;; 위상 그래프 문제
;; 각 step이 필요조건 순서대로 수행되어야함. 같은 조건 내에서는 알파벳 순서여야함
;;
;;   -->A--->B--
;;  /    \      \
;; C      -->D----->E
;;  \           /
;;   ---->F-----
;;
;; inbound 없는 C부터 시작
;; C -> A -> B -> D -> F -> E
;; 알파벳 순서대로 D에서 E로 가면 안되고, 위상 정렬에 따라 F부터 가야함
;; 순차 탐색 vs 거꾸로 탐색하는 방법 (E -> F D B -> A -> C)
;; 순차 탐색: 시작 지점부터 탐색하면서, next target 마다 available 한지(모든 previous step finished인지) 체크하면서 진행
;; 역탐색: 타겟에 연결된 지점 순차로 넣고, 그 다음 타겟들 넣을때 역시 available한지 체크하면서 진행

(def sample-input
  "Step C must be finished before step A can begin.
Step C must be finished before step F can begin.
Step A must be finished before step B can begin.
Step A must be finished before step D can begin.
Step B must be finished before step E can begin.
Step D must be finished before step E can begin.
Step F must be finished before step E can begin.")

(def real-input (slurp "resources/day7.input.txt"))

(defn parse-instruction
  "parsing one line"
  [line]
  (let [patt (re-pattern #"Step (\w) must be finished before step (\w) can begin\.")
        [_ before after] (re-find patt line)]
    [before after]))

(defn group-by-map-by
  "group-by 하면서 value mapping까지"
  [key-fn value-fn coll]
  (->> (group-by key-fn coll)
       (map (fn [[k v]]
              [k (set (map value-fn v))]))
       (into {})))

(defn find-begins-and-ends
  "시작지점 찾기"
  [instructions]
  (let [from-keys (set (map first instructions))
        end-keys (set (map second instructions))]
    [(set/difference from-keys end-keys)
     (set/difference end-keys from-keys)]))

(defn path-map
  "다음 진행 path 연결 맵"
  [instructions]
  (group-by-map-by first second instructions))

(defn prerequisite-map
  "선행 조건 검사용 맵"
  [instructions]
  (group-by-map-by second first instructions))

(defn find-first [fun coll]
  (some #(when (fun %) %) coll))

(defn find-first-available
  "주어진 가능경로 알파벳순으로 정렬했을때 available 한 첫번째 step 찾기"
  [next-paths prerequisite-map curr-order]
  (find-first
   #(set/subset? (prerequisite-map %) (set curr-order))
   (sort next-paths)))

(defn topological-ordering
  "step을 수행 가능한 순서대로 정렬"
  [path-map prerequisite-map [begins _ends]]
  (loop [[first-one & rest-begins] (sort begins)
         next-targets (set/union (path-map first-one) (set rest-begins))
         curr-order [first-one]]
    ;; next-target중에서 prerequisite 만족한 첫번째 경로 찾기
    (let [next-available (find-first-available next-targets prerequisite-map curr-order)
          next-order (conj curr-order next-available)
          next-paths (set/difference (set/union next-targets (path-map next-available)) (set next-order))]
      (if (empty? next-paths)
        next-order
        (recur next-available next-paths next-order)))))

(comment
  (as-> real-input v
    (str/split-lines v)
    (map parse-instruction v)
    (topological-ordering (path-map v) (prerequisite-map v) (find-begins-and-ends v))
    (apply str v)
       ;;
    ))

;; Part 2
