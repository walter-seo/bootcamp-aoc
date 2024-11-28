(ns aoc2018-3
  (:require [clojure.string :as str]
            [clojure.set]))

;; 파트 1
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

(comment
  (def sample-input "#1 @ 1,3: 4x4
                     #2 @ 3,1: 4x4
                     #3 @ 5,5: 2x2")

  (def real-input (slurp "resources/day3.input.txt"))

  (defn solve-1 [input]
    (->> input
       ;; parsing
         (str/split-lines)
         (map str/trim)
         (map #(str/split % #"[, :x]"))
         (map (fn [[idx _ a b _ c d]]
                {:idx (parse-long (subs idx 1))
                 :start [(parse-long a) (parse-long b)]
                 :rect [(parse-long c) (parse-long d)]}))
       ;; processing
         (map #(coverages (% :start) (% :rect)))
       ;; aggregate
         (apply merge-with +)
         (vals)
         (filter #(>= % 2))
         (count)))

  (defn coverages [[top-x top-y] [width height]]
    (frequencies (for [x (range top-x (+ top-x width))
                       y (range top-y (+ top-y height))]
                   [x y])))

  (solve-1 real-input)
  ;;
  )



;; 파트 2
;; 입력대로 모든 격자를 채우고 나면, 정확히 한 ID에 해당하는 영역이 다른 어떤 영역과도 겹치지 않음
;; 위의 예시에서는 ID 3 이 ID 1, 2와 겹치지 않음. 3을 출력.
;; 겹치지 않는 영역을 가진 ID를 출력하시오. (문제에서 답이 하나만 나옴을 보장함)

(comment

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
      (->> coll
           (find-first
            #(-> %
                 (:cover)
                 (keys)
                 (non-overlap? total-overlaps))))))

  (->> real-input
       ;; parsing
       (parse-input)
       ;; processing
       (map put-coverage)
       ;; aggregate
       (find-non-overlap)
       (:idx)
  ;;
       ))