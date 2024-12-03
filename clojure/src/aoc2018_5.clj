(ns aoc2018_5)
;; 파트 1
;; 입력: dabAcCaCBAcCcaDA

;; 같은 종류의 소문자와 대문자는 서로 ‘반응‘하여 사라짐. aABb -> ‘’
;; 사라진 자리는 진공이 되기 때문에 다른 문자들이 붙게 되고, 또 그 문자들끼리 반응할 수 있음.  abBA-> aA -> ‘’
;; 바로 옆에 붙어있어야만 서로 반응함. abAB -> abAB (반응 없음)
;; 대문자-대문자, 소문자-소문자는 서로 반응하지 않음. aabAAB-> aabAAB (반응 없음)
;; 예시 dabAcCaCBAcCcaDA => dabCBAcaDA

;; 주어진 input 에서 최종으로 남는 문자열을 리턴하시오.

(def sample-input "dabAcCaCBAcCcaDA")
(def real-input (slurp "resources/day5.input.txt"))

(defn react?
  "두 character가 반응하는가?"
  [a b]
  (let [A (Character/toUpperCase a)
        B (Character/toUpperCase b)]
    (= A B)))

(defn drop-pairs
  [f coll]
  (loop [remain coll
         acc '()]
    (cond
      (empty? remain) (reverse acc)
      (f (first remain)) (let [dropped (drop 2 remain)]
                           (recur (rest dropped) (conj acc (first dropped))))
      :else (recur (rest remain) (conj acc (first remain))))))

(defn string->polymer
  "주어진 string 전처리하여 list of pair 형태로"
  [polymer-string]
  (->> polymer-string
       (partitionv 2 1 [\0]) ;; 마지막 문자는 더미
       ))

(defn polymer->string
  "전처리된 polymer를 문자열로"
  [polymer]
  (->> polymer
       (map first)
       (apply str)))

(defn trigger-once
  "주어진 polymer가 한번 반응하여 변형"
  [polymer]
  (drop-pairs #(apply react? %) polymer))

(defn trigger-all
  "주어진 polymer가 더이상 반응하지 않을때까지 변형"
  [polymer]
  (loop [prev polymer]
    (let [curr (trigger-once prev)
          prev-count (count prev)
          curr-count (count curr)]
      (if (= prev-count curr-count)
        curr
        (recur curr)))))

(comment

  (conj '(1 2 3) '(4 5 6))

  (->> sample-input
       (string->polymer)
       (trigger-all)
       (polymer->string)
       #_#_(partitionv 2 1 [(first sample-input)])
         (map #(apply react? %))))

;; 파트 2
;; 주어진 문자열에서 한 유닛 (대문자와 소문자)을 전부 없앤 후 반응시켰을 때, 가장 짧은 문자열의 길이를 리턴하시오.
;; 예를 들어 dabAcCaCBAcCcaDA 에서 a/A를 없애고 모두 반응시키면 dbCBcD가 되고 길이는 6인데 비해,
;; 같은 문자열에서 c/C를 없애고 모두 반응시키면 daDA가 남고 길이가 4이므로 4가 가장 짧은 길이가 됨.

