(ns aoc2020-4
  (:require
   [clojure.string :as str]
   [clojure.spec.alpha :as s]))

;;
;; 여권이 유효한지 판단하려고 한다. 여권에는 다음과 같은 필드가 있음.
;; - byr (Birth Year)
;; - iyr (Issue Year)
;; - eyr (Expiration Year)
;; - hgt (Height)
;; - hcl (Hair Color)
;; - ecl (Eye Color)
;; - pid (Passport ID)
;; - cid (Country ID)
;;
;; 파트 1에서는 여권의 모든 필드가 존재하는지의 여부를 검사한다.
;;
;; 파트1에서는 필드의 유무만을 검사했다면, 파트2에서는 구체적인 범위가 주어진다.
;; - byr (Birth Year) - 4 자리 숫자; 최소 1920 & 최대 2002.
;; - iyr (Issue Year) - 4 자리 숫자; 최소 2010 & 최대 2020.
;; - eyr (Expiration Year) - 4 자리 숫자; 최소 2020 & 최대 2030.
;; - hgt (Height) - 마지막에 cm 혹은 in이 오는 숫자:
;; - cm의 경우, 숫자는 최소 150 & 최대 193.
;; - in의 경우, 숫자는 최소 59 & 최대 76.
;; - hcl (Hair Color) - #뒤에 오는 정확히 6개의 캐릭터 0-9 혹은 a-f.
;; - ecl (Eye Color) - 정확히 amb blu brn gry grn hzl oth 중 하나.
;; - pid (Passport ID) - 처음 올수도 있는  0을 포함하는 9자리 숫자.
;; - cid (Country ID) - 없어도 됨.

(def raw-input
  "eyr:1972 cid:100
hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926

iyr:2019
hcl:#602927 eyr:1967 hgt:170cm
ecl:grn pid:012533040 byr:1946

hcl:dab227 iyr:2012
ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277

hgt:59cm ecl:zzz
eyr:2038 hcl:74454a iyr:2023
pid:3556412378 byr:2007
")

(defn raw-parser
  [raw-input]
  (mapv
   #(mapv (fn [s] (str/split s #":"))
          (str/split % #"\s"))
   (str/split raw-input #"\n\n")))

(def part-one-sample
  [[["ecl" "gry"]
    ["pid" "860033327"]
    ["eyr" "2020"]
    ["hcl" "#fffffd"]
    ["byr" "1937"]
    ["iyr" "2017"]
    ["cid" "147"]
    ["hgt" "183cm"]]
   [["iyr" "2013"]
    ["ecl" "amb"]
    ["cid" "350"]
    ["eyr" "2023"]
    ["pid" "028048884"]
    ["hcl" "#cfa07d"]
    ["byr" "1929"]]
   [["hcl" "#ae17e1"]
    ["iyr" "2013"]
    ["eyr" "2024"]
    ["ecl" "brn"]
    ["pid" "760753108"]
    ["byr" "1931"]
    ["hgt" "179cm"]]
   [["hcl" "#cfa07d"]
    ["eyr" "2025"]
    ["pid" "166559648"]
    ["iyr" "2011"]
    ["ecl" "brn"]
    ["hgt" "59in"]]])

(def part-two-valid-sample
  [[["pid" "087499704"]
    ["hgt" "74in"]
    ["ecl" "grn"]
    ["iyr" "2012"]
    ["eyr" "2030"]
    ["byr" "1980"]
    ["hcl" "#623a2f"]]
   [["eyr" "2029"]
    ["ecl" "blu"]
    ["cid" "129"]
    ["byr" "1989"]
    ["iyr" "2014"]
    ["pid" "896056539"]
    ["hcl" "#a97842"]
    ["hgt" "165cm"]]
   [["hcl" "#888785"]
    ["hgt" "164cm"]
    ["byr" "2001"]
    ["iyr" "2015"]
    ["cid" "88"]
    ["pid" "545766238"]
    ["ecl" "hzl"]
    ["eyr" "2022"]]
   [["iyr" "2010"]
    ["hgt" "158cm"]
    ["hcl" "#b6652a"]
    ["ecl" "blu"]
    ["byr" "1944"]
    ["eyr" "2021"]
    ["pid" "093154719"]]])

(def part-two-invalid-sample
  [[["eyr" "1972"]
    ["cid" "100"]
    ["hcl" "#18171d"]
    ["ecl" "amb"]
    ["hgt" "170"]
    ["pid" "186cm"]
    ["iyr" "2018"]
    ["byr" "1926"]]
   [["iyr" "2019"]
    ["hcl" "#602927"]
    ["eyr" "1967"]
    ["hgt" "170cm"]
    ["ecl" "grn"]
    ["pid" "012533040"]
    ["byr" "1946"]]
   [["hcl" "dab227"]
    ["iyr" "2012"]
    ["ecl" "brn"]
    ["hgt" "182cm"]
    ["pid" "021572410"]
    ["eyr" "2020"]
    ["byr" "1992"]
    ["cid" "277"]]
   [["hgt" "59cm"]
    ["ecl" "zzz"]
    ["eyr" "2038"]
    ["hcl" "74454a"]
    ["iyr" "2023"]
    ["pid" "3556412378"]
    ["byr" "2007"]]])

(def real-input
  (raw-parser (slurp "resources/2020/day4.input.txt")))

;; clojure.spec declaration

(def string-to-int
  (s/conformer #(try (Integer. %) (catch Exception _ ::s/invalid))))

(def height-cm
  (s/and
   (s/conformer #(last (re-find #"(\d+)cm" %)))
   string-to-int
   #(and (>= % 150) (<= % 193))))

(def height-in
  (s/and
   (s/conformer #(last (re-find #"(\d+)in")))
   string-to-int
   #(and (>= % 59) (<= % 76))))

(def hair-color-regex #"^#[a-f0-9]{6}$")
(def eye-colors #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"})
(def passport-id-regex #"\d{9}")

(s/def ::birth-year (s/and string-to-int #(and (>= % 1920) (<= % 2002))))
(s/def ::iss-year (s/and string-to-int #(and (>= % 2010) (<= % 2020))))
(s/def ::exp-year (s/and string-to-int #(and (>= % 2020) (<= % 2030))))
(s/def ::height (s/and string? (s/or height-cm height-in)))
(s/def ::eye-color eye-colors)
(s/def ::hair-color (s/and string? #(re-matches hair-color-regex %)))
(s/def ::pass-id #(re-matches passport-id-regex %))

(s/def ::passport
  (s/keys :req-un [::birth-year ::iss-year ::exp-year
                   ::height ::eye-color ::pass-id ::hair-color]
          :opt-un [::country-id]))

(def passport-key-map
  {"byr" :birth-year "iyr" :iss-year "eyr" :exp-year
   "pid" :pass-id "hcl" :hair-color "hgt" :height
   "ecl" :eye-color "cid" :country-id})

(defn passport-keys?
  [passport]
  (every? (set (keys passport)) [:iss-year
                                 :exp-year
                                 :hair-color
                                 :birth-year
                                 :eye-color
                                 :pass-id
                                 :height]))

(defn cast-passport-keys
  "convert list to map"
  [kv-list]
  (update-keys (into {} kv-list)
               passport-key-map))

(comment
  (->> real-input
       (map cast-passport-keys)
       (filter #(s/valid? passport-keys? %))
       (count)))

;; Part 2
;;
;;
(comment
  (re-find #"(\d+)cm" "176cm")
  (s/conform height-cm "140cm")

  (->> real-input
       (map cast-passport-keys)
       (map (partial s/conform ::passport))
       (remove s/invalid?)
       (count)))


