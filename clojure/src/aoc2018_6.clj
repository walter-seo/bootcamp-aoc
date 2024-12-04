(ns aoc2018_6
  (:require
   [clojure.string :as str]))

;; 파트 1
;; 입력 : 좌표의 쌍이 N개 주어짐

;; 1, 1
;; 1, 6
;; 8, 3
;; 3, 4
;; 5, 5
;; 8, 9

;; 각 점은 1 tick이 지날때 마다 상,하,좌,우로 증식함.

;;  ..........
;;  .A........
;;  ..........
;;  ........C.
;;  ...D......
;;  .....E....
;;  .B........
;;  ..........
;;  ..........
;;  ........F.

;;  aaaaa.cccc
;;  aAaaa.cccc
;;  aaaddecccc
;;  aadddeccCc
;;  ..dDdeeccc
;;  bb.deEeecc
;;  bBb.eeee..
;;  bbb.eeefff
;;  bbb.eeffff
;;  bbb.ffffFf

;; 여기서 . 으로 표기된 부분은 각 출발 지점으로부터 '같은 거리'에 있는 부분을 뜻함.
;; 맵 크기에는 제한이 없어 무한으로 뻗어나간다고 할 때, 가장 큰 유한한 면적의 크기를 반환 (part-1)

(def sample-input
  "1, 1
1, 6
8, 3
3, 4
5, 5
8, 9")

(def real-input (slurp "resources/day6.input.txt"))

(defn str->coord
  [str]
  (mapv parse-long (str/split str #", ")))

(defn input->coords
  [input]
  (->> input
       str/split-lines
       (map str->coord)))

(defrecord Data [coords coords-range all-spaces])

(defn range-of-coords
  "주어진 좌표들의 최소값과 최대값 범위"
  [coords]
  (let [min-coord [0 0] ;; 최소는 (0, 0)으로 고정
        max-x (apply max (map first coords))
        max-y (apply max (map second coords))
        max-coord [max-x max-y]]
    {:min min-coord
     :max max-coord}))

(defn make-data
  "Data 구조체 생성"
  [coords]
  (->Data coords (range-of-coords coords) nil))

(defn all-spaces
  "모든 빈공간 좌표 맵 초기화"
  [data]
  (let [[min-x min-y] (get-in data [:coords-range :min])
        [max-x max-y] (get-in data [:coords-range :max])
        coords-set (set (:coords data))
        all-coords (for [x (range min-x max-x)
                         y (range min-y max-y)
                         :when (not (contains? coords-set [x y]))]
                     [[x y] nil])]
    (into {} all-coords)))

(defn put-all-spaces
  "data 자료구조에 빈공간 좌표 맵 넣기"
  [data]
  (assoc data :all-spaces (all-spaces data)))

(defn manht-dist
  "두 좌표 사이 맨하탄 거리"
  [[x1 y1] [x2 y2]]
  (let [abs-diff #(abs (- %1 %2))]
    (+ (abs-diff x1 x2) (abs-diff y1 y2))))

(defn find-closest-coord
  "타겟 좌표에 대해 주어진 좌표들 중 가장 가까운 좌표 목록"
  [[target-coord _] coords]
  (let [min-dist (apply min (map (partial manht-dist target-coord) coords))
        min-dist-coords (filterv #(= min-dist (manht-dist target-coord %)) coords)]
    [target-coord min-dist-coords]))

(defn fill-closest-coords
  "Data의 초기화된 빈공간 맵을 가장 가까운 좌표들 맵으로 value들 채워넣기"
  [data]
  (->> (:all-spaces data)
       (map #(find-closest-coord % (:coords data)))
       (assoc data :all-spaces)))

(defn spaces-group-by-closest-coord
  "공간좌표들을 가장 가까운 좌표들로 그룹화. 겹치는 공간은 배제"
  [all-spaces-map]
  (->> all-spaces-map
       (filter #(= 1 (count (second %))))
       (group-by (comp first second))
       (map #(vector (key %) (map (partial first) (val %))))))

(defn finite-coord?
  "무한 확장 불가능한지 여부: 가장자리에 위치한 좌표가 한개도 없을 경우"
  [[_target-coord close-covers] {:keys [min max]}]
  (let [out? #(or (= (first %) (first min))
                  (= (second %) (second max)))]
    (not-any? out? close-covers)))

(defn count-max-finite-coords
  "finite한 좌표들중에 가장 큰 범위의 좌표 수"
  [data]
  (let [closest-spaces-group (spaces-group-by-closest-coord (:all-spaces data))
        finite-coords (filter #(finite-coord? % (:coords-range data)) closest-spaces-group)]
    (->> finite-coords
         (map #(count (second %)))
         (apply max)
         inc)))

(comment
  (def sample-list [[1 [2 3]] [4 [5 6]] [7 [8]]])
  (for [[x [y z]] sample-list]
    [x [y z]])

  (->> sample-input
       input->coords
       make-data
       put-all-spaces
       fill-closest-coords
       count-max-finite-coords
       #_count))

;; 파트 2
;; 안전(safe) 한 지역은 근원지'들'로부터의 맨하탄거리(Manhattan distance, 격자를 상하좌우로만 움직일때의 최단 거리)의 '합'이 N 미만인 지역임.

;;  ..........
;;  .A........
;;  ..........
;;  ...###..C.
;;  ..#D###...
;;  ..###E#...
;;  .B.###....
;;  ..........
;;  ..........
;;  ........F.

;; Distance to coordinate A: abs(4-1) + abs(3-1) =  5
;; Distance to coordinate B: abs(4-1) + abs(3-6) =  6
;; Distance to coordinate C: abs(4-8) + abs(3-3) =  4
;; Distance to coordinate D: abs(4-3) + abs(3-4) =  2
;; Distance to coordinate E: abs(4-5) + abs(3-5) =  3
;; Distance to coordinate F: abs(4-8) + abs(3-9) = 10
;; Total distance: 5 + 6 + 4 + 2 + 3 + 10 = 30

;; N이 10000 미만인 안전한 지역의 사이즈를 구하시오.
