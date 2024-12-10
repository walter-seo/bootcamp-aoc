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
  ["Step C must be finished before step A can begin."
   "Step C must be finished before step F can begin."
   "Step A must be finished before step B can begin."
   "Step A must be finished before step D can begin."
   "Step B must be finished before step E can begin."
   "Step D must be finished before step E can begin."
   "Step F must be finished before step E can begin."])

(def real-input (str/split-lines (slurp "resources/day7.input.txt")))

(defn parse-instruction
  "parsing one line"
  [line]
  (let [patt (re-pattern #"Step (\w) must be finished before step (\w) can begin\.")
        [_ before after] (re-find patt line)]
    [before after]))

(defn group-by-map-by
  "Util function: group-by 하면서 value mapping하고 set으로"
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
  (let [all-steps (reduce #(assoc %1 %2 #{}) {} (flatten instructions))
        required-steps-map (group-by-map-by second first instructions)]
    (merge all-steps required-steps-map)))

(defn find-first [pred coll]
  (some #(when (pred %) %) coll))

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
      ;; 다음으로 처리할 것이 없으면 end 처리
      (if (empty? next-paths)
        next-order
        (recur next-available next-paths next-order)))))

@(def instructions
   "parsed instructions from input"
   (->> real-input
        (map parse-instruction)))

(comment
  (let [ins instructions
        path-map (path-map ins)
        prerequisite-map (prerequisite-map ins)
        begin-end (find-begins-and-ends ins)]
    (apply str
           (topological-ordering path-map prerequisite-map begin-end))))

;; Part 2
;;
;; tick 하나하나 반복문 돌면서 process 수행

(defn string-char-range
  "Util function for getting character range"
  [start end]
  (map char (range (int start) (inc (int end)))))

(defn step-required-seconds
  "each steps have their own process minute"
  [init]
  (into {}
        (map-indexed
         #(vector (str %2) (+ %1 init 1))
         (string-char-range \A \Z))))

(defn make-workers
  [count]
  (repeat count {:status :idle, :curr-step nil, :proc-time 0}))

(defn idle?
  "is worker idle?"
  [{status :status}]
  (= :idle status))

(defn- step-done?
  [worker step-finish-time-set]
  (and
   (not (idle? worker))
   (>= (:proc-time worker) (step-finish-time-set (:curr-step worker)))))

(defn find-next-available-steps
  [running-steps done-steps prerequisite-map]
  (filter #(and
            (not (contains? running-steps %))
            (set/subset? (prerequisite-map %) done-steps))
          (keys prerequisite-map)))

(defn- assign-next-step
  "assign next available step to given worker if it is :idle"
  [idle-worker available-step]
  (if available-step
    (-> idle-worker
        (assoc :status :running)
        (assoc :curr-step available-step))
    idle-worker))

(defn- assign-idle-workers
  "idle한 worker가 있다면 다음 available step을 찾아서 할당하기"
  [workers running-steps done-steps prerequisite-map]
  (let [available-steps (find-next-available-steps running-steps done-steps prerequisite-map)
        {idle-workers true running-workers false} (group-by idle? workers)
        available-steps' (concat available-steps (repeat nil))
        assigned-idle-workers (map assign-next-step idle-workers available-steps')
        merged-workers (concat assigned-idle-workers running-workers)]
    merged-workers))

(defn- acc-running-steps
  "Assigning step 완료된 worker들의 step들을 running step set에 추가"
  [{:keys [workers running-steps] :as curr-data}]
  (let [curr-steps (set (filter identity (map :curr-step workers)))
        updated-running-steps (set/union running-steps curr-steps)]
    (assoc curr-data :running-steps updated-running-steps)))

(defn- inc-workers-proc-time
  "running 워커들에 한해서 process time increase"
  [workers]
  (mapv #(if (idle? %) % (update % :proc-time inc)) workers))

(defn- acc-done-steps
  "worker들 검사하여 done step set 누적 업데이트"
  [{:keys [done-steps workers] :as curr-data} step-required-time-set]
  (let [done-workers (filter #(step-done? % step-required-time-set) workers)
        current-done-steps (set (map :curr-step done-workers))
        accumulated-done-steps (set/union done-steps current-done-steps)]
    (assoc curr-data :done-steps accumulated-done-steps)))

(defn- reset-worker
  "done 상태 worker 초기화"
  [worker]
  (-> worker
      (assoc :status :idle)
      (assoc :proc-time 0)
      (assoc :curr-step nil)))

(defn- reset-finished-workers
  "완료된 워커들 idle 상태로 초기화
  {:status :idle :proc-time 0 :curr-step nil}"
  [workers step-required-time-set]
  (mapv #(if (step-done? % step-required-time-set) (reset-worker %) %) workers))

;; keep in memory
;; - Current tick
;; - So far done step set
;; - In-progress steps and its elapsed time for each worker
;; worker e.g. {:status :idle :curr-step "A" :proc-sec 13}
(defn simulate-one-tick
  "simulate one tick for each workers"
  [{:keys [done-steps running-steps] :as curr-data} step-required-time-set prerequisite-map]
  ;; for each worker
  ;; if it has to find a next step (idle status)
  ;;    find a next step that requires all prerequisite steps (it might not find next step)
  ;;    add step to running-steps
  ;; again, for each worker
  ;; run one tick, inc processing time
  ;; when current step processing time satisfy required time, 
  ;;    then update worker status idle
  ;;    add step to done-steps
  (-> curr-data
      (update :workers assign-idle-workers running-steps done-steps prerequisite-map)
      (acc-running-steps)
      (update :tick inc)
      (update :workers inc-workers-proc-time)
      (acc-done-steps step-required-time-set)
      (update :workers reset-finished-workers step-required-time-set)))

(comment
  (let [ins instructions
        prerequisite-map (prerequisite-map ins)
        all-step-set (set (keys prerequisite-map))
        step-required-time-set (step-required-seconds 60)
        workers (make-workers 5)
        init-data {:workers workers :tick 0 :running-steps #{} :done-steps #{}}
        simulate-once-fn #(simulate-one-tick % step-required-time-set prerequisite-map)
        not-all-done? #(not= all-step-set (:done-steps %))]
    (->> init-data
         (iterate simulate-once-fn)
         (drop-while not-all-done?)
         (first))))
