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

(comment
  (def sample-input "abcdef
                     bababc
                     abbcde
                     abcccd
                     aabcdd
                     abcdee
                     ababab")

  (defn solve-1 [input]
    (->> input
    ;; parsing
         (str/split-lines)
         (map #(-> %
                   str/trim
                   frequencies
                   vals))
    ;; processing
         (reduce
          (fn [[two three] counts]
            (cond-> [two three]
              (some #{2} counts) (assoc 0 (+ two 1))
              (some #{3} counts) (assoc 1 (+ three 1))))
          [0 0])
    ;; aggregate
         (reduce *)))

  (def real-input (slurp "resources/day2.input.txt"))

  (solve-1 real-input))



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

(comment
  (def sample-input
    "abcde
    fghij
    klmno
    pqrst
    fguij
    axcye
    wvxyz")

  ;; 두 string이 correct match 인지 check
  (defn correct-boxes? [a_str b_str]
    (->> (map vector a_str b_str)
         (filter (fn [[ac bc]] (not= ac bc)))
         count
         (= 1)))

  ;; 공통부분 문자열
  (defn common-letters [a_str b_str]
    (->> (map vector a_str b_str)
         (filter (fn [[ac bc]] (= ac bc)))
         (map first)
         (apply str)))

  ;; match 결과 찾기
  (defn find-correct-boxes [words]
    ;; loop & recur VS reduce & reduced
    ;;   (loop [[item & remaining] words]
    ;;       (if ( correct-boxes? item ))
    ;;     )
    ;; NOTE
    ;; It might be poor performance because it causes O(N^2) time complexity.
    ;; It will return nil when it is not able to find the correct boxes.
    (->> (for [item1 words
               item2 words
               :while (not= item1 item2)]
           [item1 item2])
         (reduce
          (fn [_ [item1 item2]]
            (if (correct-boxes? item1 item2)
              (reduced (common-letters item1 item2))
              nil)))))

  (defn solve-2 [input]
    (->> input
    ;; parsing
         str/split-lines
         (map str/trim)
    ;; processing & aggregate 
         find-correct-boxes))

  (solve-2 real-input)
  ;;
  )


;; #################################
;; ###        Refactoring        ###
;; #################################
