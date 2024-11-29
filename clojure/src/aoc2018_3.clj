(ns aoc2018-3
  (:require [clojure.string :as str]
            [clojure.set]))

;; 파트 1
;; Elf들은 굴뚝에 들어갈 수 있는 산타의 옷감을 드디어 찾았다
;; 이제 어떻게 옷감을 재단할지 정해야함
;; 각 변이 1000 inch 이상인 매우 큰 정사각형 옷감이 있는데, 
;; 옷감의 어느 **직사각형 구간**이 가장 옷감에 적합한지 각자 *주장*하기 시작함.
;; 문제는 이 주장들 중에서 *겹치는* 구간들이 생김. 일단 겹치는 것들을 찾아보자 
;; 다음과 같은 입력이 주어짐.

;; #1 @ 1,3: 4x4
;; #2 @ 3,1: 4x4
;; #3 @ 5,5: 2x2

;; # 뒤에 오는 숫자는 ID, @ 뒤에 오는 숫자 쌍 (a, b)는 시작 좌표, : 뒤에 오는 (c x d)는 격자를 나타냄.
;; 입력의 정보대로 격자 공간을 채우면 아래와 같이 됨.

;;      ........
;;      ...2222.
;;      ...2222.
;;      .11XX22.
;;      .11XX22.
;;      .111133.
;;      .111133.
;;      ........

;; 여기서 XX는 ID 1, 2, 3의 영역이 두번 이상 겹치는 지역.
;; 겹치는 지역의 갯수를 출력하시오. (위의 예시에서는 4)
(def sample-input "#1 @ 1,3: 4x4
                     #2 @ 3,1: 4x4
                     #3 @ 5,5: 2x2")

(def real-input (slurp "resources/day3.input.txt"))

(defn coverages [[top-x top-y] [width height]]
  (frequencies (for [x (range top-x (+ top-x width))
                     y (range top-y (+ top-y height))]
                 [x y])))

(defn parse-claim
  "parsing input to Claim"
  [claim-str]
  (let [[idx _ a b _ c d] (str/split claim-str #"[, :x]")]
    {:idx (parse-long (subs idx 1))
     :start [(parse-long a) (parse-long b)]
     :rect [(parse-long c) (parse-long d)]}))

(defn count-overlap-covers
  "Counting overlaps in given claims"
  [claims]
  (->> claims
       ;; processing
       (map #(coverages (% :start) (% :rect))) ; => ({[4 3] 1, [2 3] 1, ...} ..)
       ;; aggregate
       (apply merge-with +) ; => {[4 3] 2, [2 3] 1, ...}
       (vals)
       (filter (partial <= 2))
       (count)))

(comment
  (->> sample-input
       ;; parsing
       (str/split-lines)
       (map str/trim)
       (map parse-claim) ; => ({:idx 1, :start [1 3], :rect [4 4]} ..)
       ;; process & aggregate
       (count-overlap-covers)))
  ;;

;; 파트 2
;; 입력대로 모든 격자를 채우고 나면, 정확히 한 ID에 해당하는 영역이 다른 어떤 영역과도 겹치지 않음
;; 위의 예시에서는 ID 3 이 ID 1, 2와 겹치지 않음. 3을 출력.
;; 겹치지 않는 영역을 가진 ID를 출력하시오. (문제에서 답이 하나만 나옴을 보장함)

(defn parse-input [input]
  (->> input
       (str/split-lines)
       (map str/trim)
       (map #(str/split % #"[, :x]"))
       (map (fn [[idx _ a b _ c d]]
              {:idx (parse-long (subs idx 1))
               :start [(parse-long a) (parse-long b)]
               :rect [(parse-long c) (parse-long d)]}))))

(defn put-coverage [item]
  (-> item
      (assoc :cover (coverages (item :start) (item :rect)))
      (assoc :total (apply * (item :rect)))))

(defn total-overlaps [coll]
  (->> coll
       (map :cover)
       (apply merge-with +)
       (keep #(when (>= (second %) 2) (first %)))
       set))

(defn non-overlap? [coverages total-overlap]
  (empty? (clojure.set/intersection (set coverages) total-overlap)))

(defn find-first [fun coll]
  (some #(when (fun %) %) coll))

(defn find-non-overlap [coll]
  (let [total-overlaps (total-overlaps coll)]
    (find-first
     #(-> %
          (:cover)
          (keys)
          (non-overlap? total-overlaps))
     coll)))

(comment
  (->> real-input
       ;; parsing
       (str/split-lines)
       (map str/trim)
       (map parse-claim)
       (map put-coverage)
       ;; processing & aggregate
       (find-non-overlap)
       (:idx)))
