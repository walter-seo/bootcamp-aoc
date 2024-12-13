(ns aoc2018_4
  (:require
   [clojure.string :as str]
   [java-time.api :as jt]))
;; 파트 1

;; 당신은 또다른 closet 공장에 침투함. 경비들이 보초를 서고 있음.
;; 누군가 경비들의 시간표를 벽에 메모해 놓은것을 발견함.
;; 만약 이걸 보고 특정 '시점'에 가장 높은확률로 잠들어있는 경비원을 알 수 있다면, 
;; 그 경비원만 속여서 몰래 공장에 침투할 수 있을것이다.
;; 입력:

;; [1518-11-01 00:00] Guard #10 begins shift
;; [1518-11-01 00:05] falls asleep
;; [1518-11-01 00:25] wakes up
;; [1518-11-01 00:30] falls asleep
;; [1518-11-01 00:55] wakes up
;; [1518-11-01 23:58] Guard #99 begins shift
;; [1518-11-02 00:40] falls asleep
;; [1518-11-02 00:50] wakes up
;; [1518-11-03 00:05] Guard #10 begins shift
;; [1518-11-03 00:24] falls asleep
;; [1518-11-03 00:29] wakes up
;; [1518-11-04 00:02] Guard #99 begins shift
;; [1518-11-04 00:36] falls asleep
;; [1518-11-04 00:46] wakes up
;; [1518-11-05 00:03] Guard #99 begins shift
;; [1518-11-05 00:45] falls asleep
;; [1518-11-05 00:55] wakes up

;; 키워드: 가드(Guard) 번호, 자는 시간(falls asleep), 일어나는 시간(wakes up).
;; 각 가드들은 교대 근무를 시작하고 (begins shift) 졸았다가 일어났다를 반복함.
;; 위의 예시에서 10번 가드는 0시 5분에 잤다가 25분에 일어나고, 또 0시 30분에 잠들었다가 0시 55분에 깨어남.
;; 가드들에 대해서 자고 깨는 시간 정보들이 입력으로 주어짐.
;; 단, 실제 입력값은 chronological 순서대로 나열되어있지 않다.

;; 파트 1은 “주어진 입력에 대해서, 가장 오랜시간 잠들어있었던 가드의 ID와, 그 가드가 가장 빈번하게 잠들어 있었던 분(minute)의 곱을 구하라”
;; 만약 20번 가드가 0시 10분~36분, 다음날 0시 5분~11분, 다다음날 0시 11분~13분 이렇게 잠들어 있었다면, “11분“이 가장 빈번하게 잠들어 있던 ‘분’. 그럼 답은 20 * 11 = 220.
;; 
;; 풀이
;; - 문제에서는 시간 상관없이 '분'만 중요하다.
;; - 'falls asleep' 시간부터 'wakes up' 시간까지 범위를 분으로 바꿔서 각 분 단위로 freuqency counting

(def sample-input
  ["[1518-11-01 00:00] Guard #10 begins shift"
   "[1518-11-01 00:05] falls asleep"
   "[1518-11-01 00:15] falls asleep"
   "[1518-11-01 00:25] wakes up"
   "[1518-11-01 00:30] falls asleep"
   "[1518-11-01 00:55] wakes up"
   "[1518-11-01 23:58] Guard #99 begins shift"
   "[1518-11-02 00:40] falls asleep"
   "[1518-11-02 00:50] wakes up"
   "[1518-11-03 00:05] Guard #10 begins shift"
   "[1518-11-03 00:24] falls asleep"
   "[1518-11-03 00:29] wakes up"
   "[1518-11-04 00:02] Guard #99 begins shift"
   "[1518-11-04 00:36] falls asleep"
   "[1518-11-04 00:46] wakes up"
   "[1518-11-05 00:03] Guard #99 begins shift"
   "[1518-11-05 00:45] falls asleep"
   "[1518-11-05 00:55] wakes up"])

(def real-input (str/split-lines (slurp "resources/day4.input.txt")))
;; Functions

(defn iterate-minutes
  "start ~ end time 범위 내 minutes iteration"
  [{:keys [start end]}]
  (->> (take-while #(jt/before? % end)
                   (iterate #(jt/plus % (jt/minutes 1)) start))
       (map #(.getMinute %))))

(defn parse-time
  "시간 추출"
  [line]
  (let [[_ timestamp] (re-find #"\[(.*?)\]" line)]
    (jt/local-date-time "yyyy-MM-dd HH:mm" timestamp)))

(defn parse-timestamp
  "주어진 line에서 timestamp 추출하여 description과 pair 생성"
  [line]
  (let [[_ description-part] (str/split line #"\] " 2)]
    [(parse-time line) description-part]))

(defn partition-by-guard-shifts
  "시간 순서로 나열된 schedule을 guard shift별로 partitioning"
  [schedules]
  (->> schedules
       (reduce
        (fn [[acc last-acc]
             [_timestamp description :as timeline]]
          (if (and (not-empty last-acc) (str/includes? description "Guard"))
            [(conj acc last-acc) [timeline]]
            [acc (conj last-acc timeline)]))
        ['() []])
       (apply conj)
       #_(rest)))

(defn aggregate-sleep-timelines
  "list로 나열된 event timeline을 잠든 구간을 나타내는 {:start :end} 맵 리스트로 변환"
  [schedule-lines]
  (->> schedule-lines
       (reduce
        (fn
          ([[status prev-time sections] [timestamp description]]
           (let [next-action (last (str/split description #" "))]
             (case [status next-action]
               [:asleep "asleep"] [:asleep prev-time sections]
               [:asleep "up"] [:awake nil (conj sections {:start prev-time :end timestamp})]
               [:awake "up"] [:awake nil sections]
               [:awake "asleep"] [:asleep timestamp sections]))))
        [:awake nil []])
       (last)))

(defn ->guard-id-with-sleep-timeline
  "grouping된 guard shift timeline을 맵 자료구조화. sleep timeline은 {:start :end} 맵으로"
  [[[_timestamp shift-desc] & guard-schedules]]
  (let [guard-id (->> shift-desc
                      (re-find #"#(\d+)")
                      (second)
                      (parse-long))]
    {:guard-id guard-id
     :sleep-timeline (aggregate-sleep-timelines guard-schedules)}))

(defn sleep-timeline->sleep-minutes-freq
  "{:start :end} 맵으로 이루어진 timeline list를 minute별 frequencies 맵으로"
  [sleep-timeline]
  (->> sleep-timeline
       (map #(frequencies (iterate-minutes %)))
       (reduce #(merge-with + %1 %2))))

(defn aggregate-guard-sleep-minutes-freq
  "guard timeline list에서 guard id별로 sleep-minutes-freq만 집계"
  [guard-timelines]
  (reduce (fn [acc {:keys [guard-id sleep-minutes-freq]}]
            (update acc guard-id #(merge-with + %1 %2) sleep-minutes-freq))
          {}
          guard-timelines))

(defn- total-sleep [[_guard-id guard-sleep-minutes]]
  (->> guard-sleep-minutes
       (vals)
       (apply +)))

(defn find-most-sleeper
  "총 잠든 시간의 합이 가장 큰 경비 찾기"
  [guard-sleep-minutes]
  (apply max-key total-sleep guard-sleep-minutes))

(defn guard->answer
  "주어진 경비 ID와 minute map의 가장 빈번하게 잠든 분을 곱한 값 반환"
  [guard-id minute-map]
  (* guard-id (key (apply max-key val minute-map))))

(comment
  (->> real-input
       ;; Parsing
       (map parse-timestamp)
       (sort)
       ;; Processing
       (partition-by-guard-shifts) ;; [[#object[java.time] "Guard #.."] ... ]
       (map ->guard-id-with-sleep-timeline) ;; [{:guard-id 3 :sleep-timeline [{:start start-time :end end-time} ...]}]
       (remove (comp empty? :sleep-timeline))
       (map (fn [guard]
              (assoc guard
                     :sleep-minutes-freq
                     (sleep-timeline->sleep-minutes-freq (:sleep-timeline guard)))))
       ;; Aggregates
       (aggregate-guard-sleep-minutes-freq) ;; {1297 {0 1, 2 1, ...} .. }
       (find-most-sleeper)
       (apply guard->answer)))

;; 파트 2
;; 주어진 분(minute)에 가장 많이 잠들어 있던 가드의 ID과 그 분(minute)을 곱한 값을 구하라.
;; 
(defn- most-frequently-slept-minute
  "해당 경비의 가장 자주 잠들었던 minute의 빈도 수"
  [[_guard-id sleep-minutes-map]]
  (apply max (vals sleep-minutes-map)))

(comment
  (->> real-input
       ;; Parsing
       (map parse-timestamp)
       (sort)
       ;; Processing
       (partition-by-guard-shifts)
       (map ->guard-id-with-sleep-timeline) ;; shift timelines list
       (remove (comp empty? :sleep-timeline))
       (map (fn [guard]
              (assoc guard
                     :sleep-minutes-freq
                     (sleep-timeline->sleep-minutes-freq (:sleep-timeline guard)))))
       ;; Aggregates
       (aggregate-guard-sleep-minutes-freq) ;; {1297 {0 1, 2 1, ...} .. }
       (apply max-key most-frequently-slept-minute)
       (apply guard->answer)))
