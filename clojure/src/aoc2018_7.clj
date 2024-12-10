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
  [worker step-finish-time]
  (and
   (not (idle? worker))
   (>= (:proc-time worker) step-finish-time)))

(defn find-next-step
  "주어진 step set에서 전제조건 만족한, 아직 완료되지 않은 다음 step 찾기. 가능한 다음 step이 없을 수도 있음"
  [run-steps done-steps prerequisite-map]
  (let [all-steps (keys prerequisite-map)
        next-step (some #(when
                          (and (not (contains? run-steps %))
                               (set/subset? (prerequisite-map %) done-steps))
                           %)
                        all-steps)]
    next-step))

(defn- assign-next-step-if-idle
  "assign next available step to given worker if it is :idle"
  [{:keys [status curr-step proc-time] :as worker} run-steps done-steps prerequisite-map]
  (let [next-step (if (idle? worker) (find-next-step run-steps done-steps prerequisite-map) curr-step)
        next-status (if (and (idle? worker) next-step) :running status)
        updated-worker {:status next-status :curr-step next-step :proc-time proc-time}]
    updated-worker))

(defn- assign-next-step-to-idle-workers
  "idle한 worker가 있다면 다음 available step을 찾아서 할당하기"
  [{:keys [workers] :as curr-data} prerequisite-map]
  (reduce
   (fn [{:keys [run-steps done-steps] :as acc-data} worker]
     (let [updated-worker (assign-next-step-if-idle worker run-steps done-steps prerequisite-map)
           next-step (:curr-step updated-worker)
           updated-run-steps (if next-step (conj run-steps next-step) run-steps)]
       (-> acc-data
           (update :workers #(conj % updated-worker))
           (assoc :run-steps updated-run-steps))))
   (assoc curr-data :workers [])
   workers))

(defn- inc-worker-proc-time
  "running worker의 proc time 증가"
  [worker]
  (if (idle? worker)
    worker
    (update worker :proc-time inc)))

(defn- reset-worker
  "done 상태 worker 초기화"
  [worker]
  (-> worker
      (assoc :status :idle)
      (assoc :proc-time 0)
      (assoc :curr-step nil)))

(defn- update-done-steps
  [done-steps worker step-done?]
  (if step-done?
    (conj done-steps (:curr-step worker))
    done-steps))

(defn- process-one-tick-and-check-finished-worker
  "for each worker, inc process time and check if finished"
  [{:keys [workers] :as curr-data} step-proc-time-set]
  (reduce
   (fn [{:keys [done-steps] :as acc-data} worker]
     (let [updated-worker (inc-worker-proc-time worker)
           step-finish-time (step-proc-time-set (:curr-step updated-worker))
           step-done? (step-done? worker step-finish-time)
           updated-done-steps (update-done-steps done-steps updated-worker step-done?)
           done-checked-worker (if step-done? (reset-worker updated-worker) updated-worker)]
       (-> acc-data
           (update :workers #(conj % done-checked-worker))
           (assoc :done-steps updated-done-steps))))
   (assoc curr-data :workers [])
   workers))

;; keep in memory
;; - Current tick
;; - So far done step set
;; - In-progress steps and its elapsed time for each worker
;; worker e.g. {:status :idle :curr-step "A" :proc-sec 13}
(defn simulate-one-tick
  "simulate one tick for each workers"
  [curr-data step-proc-time-set prerequisite-map]
  ;; for each worker
  ;; if it has to find a next step (idle status)
  ;;    find a next step that requires all prerequisite steps (it might not find next step)
  ;;    add step to run-steps
  ;; again, for each worker
  ;; run one tick, inc processing time
  ;; when current step processing time satisfy required time, 
  ;;    then update worker status idle
  ;;    add step to done-steps
  (->
   curr-data
   (assign-next-step-to-idle-workers prerequisite-map)
   (process-one-tick-and-check-finished-worker step-proc-time-set)
   (update :tick inc)))

(comment
  (let [ins instructions
        prerequisite-map (prerequisite-map ins)
        all-step-set (set (keys prerequisite-map))
        proc-time-set (step-required-seconds 60)
        workers (make-workers 5)
        init-data {:workers workers :tick 0 :run-steps #{} :done-steps #{}}
        simulate-once-fn #(simulate-one-tick % proc-time-set prerequisite-map)
        not-all-done? #(not= all-step-set (:done-steps %))]
    (->> init-data
         (iterate simulate-once-fn)
         (take-while not-all-done?)
         (last))))
