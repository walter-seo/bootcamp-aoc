(ns aoc2018-2
  (:require [clojure.string :as str]))

;; 파트 1
;; 주어진 각각의 문자열에서, 같은 문자가 두번 혹은 세번씩 나타난다면 각각을 한번씩 센다.
;; 두번 나타난 문자가 있는 문자열의 수 * 세번 나타난 문자가 있는 문자열의 수를 반환하시오.
;; 예)
;; abcdef 어떤 문자도 두번 혹은 세번 나타나지 않음 -> (두번 나오는 문자열 수: 0, 세번 나오는 문자열 수: 0)
;; bababc 2개의 a, 3개의 b -> (두번 나오는 문자열 수: 1, 세번 나오는 문자열 수: 1)
;; abbcde 2개의 b -> (두번 나오는 문자열 수: 2, 세번 나오는 문자열 수: 1)
;; abcccd 3개의 c -> (두번 나오는 문자열 수: 2, 세번 나오는 문자열 수: 2)
;; aabcdd 2개의 a, 2개의 d 이지만, 한 문자열에서 같은 갯수는 한번만 카운트함 -> (두번 나오는 문자열 수: 3, 세번 나오는 문자열 수: 2)
;; abcdee 2개의 e -> (두번 나오는 문자열 수: 4, 세번 나오는 문자열 수: 2)
;; ababab 3개의 a, 3개의 b 지만 한 문자열에서 같은 갯수는 한번만 카운트함 -> (두번 나오는 문자열 수: 4, 세번 나오는 문자열 수: 3)
;; 답 : 4 * 3 = 12
;; 

(def raw-input
  "abcde
    fghij
    klmno
    pqrst
    fguij
    axcye
    wvxyz")

(def sample-input-1 ["abcdef" "bababc" "abbcde" "abcccd" "aabcdd" "abcdee" "ababab"])
(def sample-input-2
  ["abcde" "fghij" "klmno" "pqrst" "fguij" "axcye" "wvxyz"])

(def real-input (map str/trim (str/split-lines (slurp "resources/day2.input.txt"))))

(defn checksum
  "주어진 box ids 마다 check-frequent 빈도를 갖는 character 수만 집계하여 만든 checksum 반환"
  ([box-ids-list] (checksum box-ids-list #{2 3}))
  ([box-ids-list check-frequent]
   (->> box-ids-list
        ;; processing
        (map (comp distinct vals frequencies)) ; (1) (1 2) (2 3) (3)
        (mapcat #(filter check-frequent %))
        (frequencies)
        (vals)
        ;; aggregate
        (apply *))))

(comment
  (checksum sample-input-1)

  (checksum sample-input-2)

  (checksum real-input)
  ;;
  )

;; 파트 2
;; 여러개의 문자열 중, 같은 위치에 정확히 하나의 문자가 다른 문자열 쌍에서 같은 부분만을 리턴하시오.
;; 예)
;; abcde
;; fghij
;; klmno
;; pqrst
;; fguij
;; axcye
;; wvxyz

;; 주어진 예시에서 fguij와 fghij는 같은 위치 (2번째 인덱스)에 정확히 한 문자 (u와 h)가 다름.
;; 따라서 같은 부분인 fgij를 리턴하면 됨.
(defn match-box-pair?
  "두 box ids 문자열이 한 문자만 차이나는 match 여부"
  [a-str b-str]
  (let [diff-count (->> (map vector a-str b-str)
                        (filter (fn [[ac bc]] (not= ac bc)))
                        (count))]
    (= 1 diff-count)))

(defn common-letters
  "공통 문자열 반환"
  [a-str b-str]
  (->> (map vector a-str b-str)
       (filter (partial apply identical?))
       (map first)
       (apply str)))

(defn box-pairs
  "가능한 모든 박스 pair 경우의 수 만들기"
  [words]
  (for [item1 words
        item2 words
        :while (not= item1 item2)]
    [item1 item2]))

(defn find-correct-box-pair
  "주어진 조합 안에서 correct box pair 찾아서 반환"
  [box-pairs]
  (loop [[[item1 item2 :as pair] & remain] box-pairs]
    (cond
      (empty? pair) nil
      (match-box-pair? item1 item2) [item1 item2]
      :else (recur remain))))

(defn find-correct-boxes
  "correct box 쌍 찾고 공통 문자열 반환"
  [box-id-seq]
  (->>  box-id-seq
        (box-pairs)
        (find-correct-box-pair)
        (apply common-letters)))

(def box-ids-seq real-input)

(comment
  (match-box-pair? "abcde" "abcdf")

  (find-correct-boxes box-ids-seq))

;; #################################
;; ###        Refactoring        ###
;; #################################

;; Part 2 Again
;; loop 대신 drop-while로 풀이
(defn find-correct-boxes-v2
  [box-id-seq]
  (->> box-id-seq
       (box-pairs)
       (drop-while #(apply (complement match-box-pair?) %))
       (first)
       (apply common-letters)))

(comment
  (find-correct-boxes box-ids-seq)

  (find-correct-boxes-v2 box-ids-seq))
