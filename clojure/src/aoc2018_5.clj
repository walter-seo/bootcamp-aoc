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
  (let [diff (abs (- (int a) (int b)))]
    (= 32 diff)))

(defn reconnect-pair
  "중간에 destroy된 pair의 바로 앞 pair가 destroy된 character의 다음 character를 바라보도록 다시 연결"
  [front rear]
  (if (and (not-empty front) (not-empty rear))
    (let [before-dropped-pair (first front)
          after-dropped-pair (first rear)
          reconnected-pair [(first before-dropped-pair) (first after-dropped-pair)]]
      #_(if (or (nil? (first after-dropped-pair)) (nil? (first before-dropped-pair)))
          #dbg (prn "here"))
      (conj (rest front) reconnected-pair))
    front))

(defn destroy-pairs
  [f coll]
  (loop [remain coll
         acc '()]
    (cond
      (empty? remain) (reverse acc)
      (f (first remain)) (let [dropped (drop 2 remain)
                               last? (empty? dropped)
                               reconnected-acc (reconnect-pair acc dropped)]
                           (recur (rest dropped)
                                  (if last? reconnected-acc (conj reconnected-acc (first dropped)))))
      :else (recur (rest remain) (conj acc (first remain))))))

(defn destroy-pairs-while
  [match? polymer]
  (loop [rear (rest polymer)
         front (list (first polymer))]
    (cond
      (empty? rear) (reverse front) ;; end 조건: 끝까지 다 돌았을때
      ;; react 하면 pair 제거하고 앞뒤 다시 체크
      (and (not-empty front) (match? (first rear) (first front))) (recur (rest rear) (rest front))
      ;; 계속 진행
      :else (recur (rest rear) (conj front (first rear))))))

(defn trigger-react
  "주어진 polymer가 반응하지 않을때까지 변형"
  [react-fn polymer]
  (->> polymer
       (destroy-pairs-while react-fn)))

(comment

  (- (int \a) (int \A))

  (conj '(1 2 3) '(4 5 6))

  (Character/toUpperCase nil)

  (->> real-input
       (trigger-react react?)
       (apply str)
       (count)))

;; 파트 2
;; 주어진 문자열에서 한 유닛 (대문자와 소문자)을 전부 없앤 후 반응시켰을 때, 가장 짧은 문자열의 길이를 리턴하시오.
;; 예를 들어 dabAcCaCBAcCcaDA 에서 a/A를 없애고 모두 반응시키면 dbCBcD가 되고 길이는 6인데 비해,
;; 같은 문자열에서 c/C를 없애고 모두 반응시키면 daDA가 남고 길이가 4이므로 4가 가장 짧은 길이가 됨.
;; 
(defn trigger-with-unit-drop
  "polymer에서 주어진 한 문자(unit)를 전부 제외한 상태에서 trigger"
  [polymer unit]
  (->> polymer
       (remove #{unit})
       (trigger-react react?)
       (count)))

(comment
  (->> "abcdefghijklmnopqrstuvwxyz"
       (map #(trigger-with-unit-drop real-input %))))