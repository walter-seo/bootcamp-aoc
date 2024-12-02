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
  "[1518-11-01 00:00] Guard #10 begins shift
[1518-11-01 00:05] falls asleep
[1518-11-01 00:15] falls asleep
[1518-11-01 00:25] wakes up
[1518-11-01 00:30] falls asleep
[1518-11-01 00:55] wakes up
[1518-11-01 23:58] Guard #99 begins shift
[1518-11-02 00:40] falls asleep
[1518-11-02 00:50] wakes up
[1518-11-03 00:05] Guard #10 begins shift
[1518-11-03 00:24] falls asleep
[1518-11-03 00:29] wakes up
[1518-11-04 00:02] Guard #99 begins shift
[1518-11-04 00:36] falls asleep
[1518-11-04 00:46] wakes up
[1518-11-05 00:03] Guard #99 begins shift
[1518-11-05 00:45] falls asleep
[1518-11-05 00:55] wakes up")

(def real-input (slurp "resources/day4.input.txt"))

(comment
  (re-find #"#(\d+)" "[1518-11-01 00:00] Guard #10 begins shift")
  (def s1 "[1518-11-01 00:00] Guard #10 begins shift")
  (def s2 "[1518-11-01 00:20] Guard #10 begins shift")
  (jt/time-between
   (parse-time s1)
   (parse-time s2)
   :minutes)
  (jt/plus (parse-time s1) (jt/minutes 1))
  (->> (take-while #(jt/before? % (parse-time s2))
                   (iterate #(jt/plus % (jt/minutes 1)) (parse-time s1)))
       (map #(.getMinute %)))

  (update {} :wait (fnil + 0) 2)

  (concat [[1 2 3]] [4 5 6])
  (apply conj [[[] [1 2 3]] [4 5 6]])
  (let [[a & remain] '([1 2 3])]
    (prn a)
    (empty? remain))

  (->> real-input
       ;; Parsing
       (str/split-lines)
       (map parse-timestamp)
       (sort)
       ;; Processing
       (partition-by-shifts)
       (map calc-guard-shift)
       (filter #(not-empty (% :sleep-timeline)))
       (convert-guard-sleep-sections)
       ;; Aggregates
       (apply max-key
              #(->> %
                    val
                    vals
                    (reduce +)))
       (apply calc-answer-part-one)))

;; Functions

(defn iterate-minutes
  [start end]
  (->> (take-while #(jt/before? % end)
                   (iterate #(jt/plus % (jt/minutes 1)) start))
       (map #(.getMinute %))))

(defn parse-time
  "시간 추출"
  [line]
  (->> line
       (re-find #"\[(.*?)\]")
       (second)
       (jt/local-date-time "yyyy-MM-dd HH:mm")))
(defn parse-timestamp
  "주어진 line에서 timestamp 추출하여 description과 pair 생성"
  [line]
  (let [[_ description-part] (str/split line #"\] " 2)]
    [(parse-time line) description-part]))

(defn partition-by-shifts
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

(defn calc-sleep-sections
  [schedule-lines]
  (->> schedule-lines
       (reduce
        (fn
          ([[status prev-time sections] [timestamp description]]
           (let [next-action (last (str/split description #" "))]
             (case [status next-action]
               [:asleep "asleep"] (vector :asleep prev-time sections)
               [:asleep "up"] (vector :awake nil (conj sections [prev-time timestamp]))
               [:awake "up"] (vector :awake nil sections)
               [:awake "asleep"] (vector :asleep timestamp sections)))))
        [:awake nil []])
       (last)))

(defn calc-guard-shift
  "one shift timeline 계산하여 [start end] 쌍으로 변환"
  [[[_time desc] & guard-schedules]]
  (let [guard-id (->> desc
                      (re-find #"#(\d+)")
                      (second)
                      (parse-long))]
    {:guard-id guard-id
     :sleep-timeline (calc-sleep-sections guard-schedules)}))

(defn convert-guard-sleep-sections
  "각 guard별 sleep 구간을 minute 맵으로 변환"
  [guard-timelines]
  (loop [[{:keys [guard-id sleep-timeline]} & more] guard-timelines
         guard-sleep-minutes-count {}]
    (let [updated-map
          (->> sleep-timeline
               (map #(frequencies (apply iterate-minutes %)))
               (reduce #(merge-with + %1 %2))
               (update guard-sleep-minutes-count guard-id #(merge-with + %1 %2)))]
      (if (empty? more)
        updated-map
        (recur more updated-map)))))

(defn calc-answer-part-one
  "주어진 경비 ID와 minute map의 가장 빈번하게 잠든 분을 곱한 값 반환"
  [guard-id minute-map]
  (* guard-id (key (apply max-key val minute-map))))

;; 파트 2
;; 주어진 분(minute)에 가장 많이 잠들어 있던 가드의 ID과 그 분(minute)을 곱한 값을 구하라.
;; 

(comment

  (->> real-input
       ;; Parsing
       (str/split-lines)
       (map parse-timestamp)
       (sort)
       ;; Processing
       (partition-by-shifts)
       (map calc-guard-shift)
       (filter #(not-empty (% :sleep-timeline)))
       (convert-guard-sleep-sections)
       ;; Aggregates
       (apply max-key
              #(->> %
                    val
                    (apply max-key val)
                    (val)))
       (apply calc-answer-part-one)))