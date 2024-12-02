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
;; 
;; 

(defn box-ids-counts-uniq [ids-str]
  (->> ids-str
       str/trim
       frequencies
       vals
       distinct))

(defn checksum [input]
  (->> input
       ;; parsing
       (str/split-lines)
       ;; processing
       (map box-ids-counts-uniq) ; (1) (1 2) (2 3) (3)
       (mapcat #(filter #{2 3} %))
       (frequencies)
       (vals)
       ;; aggregate
       (reduce *)))
(def real-input (slurp "resources/day2.input.txt"))

(comment
  (def sample-input "abcdef
                     bababc
                     abbcde
                     abcccd
                     aabcdd
                     abcdee
                     ababab")

  (checksum real-input))

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

;; 주어진 예시에서 fguij와 fghij는 같은 위치 (2번째 인덱스)에 정확히 한 문자 (u와 h)가 다름. 따라서 같은 부분인 fgij를 리턴하면 됨.
(defn correct-boxes?
  "두 문자열이 correct match 인지 여부"
  [a-str b-str]
  (->> (map vector a-str b-str)
       (filter (fn [[ac bc]] (not= ac bc)))
       count
       (= 1)))

(defn common-letters
  "공통 문자열 반환"
  [a-str b-str]
  (->> (map vector a-str b-str)
       (filter (fn [[ac bc]] (= ac bc)))
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
  (->> box-pairs
       (reduce
        (fn [_ [item1 item2]]
          (if (correct-boxes? item1 item2)
            (reduced (vector item1 item2))
            nil)))))

(defn find-correct-boxes
  "correct box 쌍 찾고 공통 문자열 반환"
  [words]
    ;; loop & recur VS reduce & reduced
    ;;   (loop [[item & remaining] words]
    ;;       (if ( correct-boxes? item ))
    ;;     )
    ;; NOTE
    ;; It might be poor performance because it causes O(N^2) time complexity.
    ;; It will return nil when it is not able to find the correct boxes.
  (->>  words
        (box-pairs)
        (find-correct-box-pair)
        (apply common-letters)))

(defn solve-2 [input]
  (->> input
    ;; parsing
       str/split-lines
       (map str/trim)
    ;; processing & aggregate 
       find-correct-boxes))

(comment
  (def sample-input
    "abcde
    fghij
    klmno
    pqrst
    fguij
    axcye
    wvxyz")

  (solve-2 real-input))
  ;;

;; #################################
;; ###        Refactoring        ###
;; #################################
