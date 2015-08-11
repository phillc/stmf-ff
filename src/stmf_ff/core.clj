(ns stmf-ff.core
  (:require [clojure.set :refer [difference
                                 intersection]]))

(def seeded-managers
  ["Paul"
   "Dave"
   "Angie"
   "Brad"
   "Ian"
   "Jeff"
   "Kris"
   "Stewart"
   "Mike"
   "John"
   "Drew"
   "Phillip"
   "Annie"
   "Brian"])


(def family-matchups
  [#{"Phillip" "Angie"}
   #{"Dave" "Annie"}
   #{"Jeff" "John"}
   #{"Drew" "Mike"}])

(def rival-matchups
  [#{"Phillip" "Angie"}
   #{"Phillip" "Annie"}
   #{"Phillip" "Brian"}
   #{"Phillip" "Dave"}
   #{"Phillip" "Drew"}
   #{"Phillip" "Ian"}
   #{"Phillip" "Kris"}
   #{"Paul" "Drew"}
   #{"Paul" "Jeff"}
   #{"Paul" "Mike"}
   #{"Paul" "Stewart"}
   #{"Dave" "Angie"}
   #{"Dave" "Annie"}
   #{"Dave" "Brad"}
   #{"Dave" "Ian"}
   #{"Jeff" "Drew"}
   #{"Jeff" "John"}
   #{"Jeff" "Stewart"}
   #{"Angie" "Annie"}
   #{"Angie" "Brian"}
   #{"Angie" "Drew"}
   #{"Angie" "Ian"}
   #{"Ian" "Annie"}
   #{"Ian" "Brad"}
   #{"Ian" "Drew"}
   #{"Brad" "Annie"}
   #{"Stewart" "Drew"}
   #{"Drew" "Mike"}
   ])

;; (def family-week 12)

;; (def blank-schedule
;; (into (sorted-map) (map (fn [week] [week []]) (range 1 14))))

;; (defn find-schedules
;; rotate seeds (In manager order)
;; satisfies family week
;; satisfies everyone has a rivlary in week 11 (or 10?)
;; )

(def league-weeks 13)

;; (defn create-round [size remaining-weeks]
;; first-half)
;; )

(defn rotate [coll]
  (take (count coll) (drop (dec (count coll)) (cycle coll))))

(defn round-robin-schedule [size rounds]
  (loop [seeds (range size)
         remaining-rounds rounds
         result []]
    (if (zero? remaining-rounds)
      result
      (let [first-half (take (/ size 2) seeds)
            second-half (reverse (drop (/ size 2) seeds))]
        (recur
         (concat [(first seeds)] (rotate (rest seeds)))
         (dec remaining-rounds)
         (conj result (map vector first-half second-half)))))))

(defn matchups-included-in [matchups week]
  (count (intersection (set matchups) (set week))))

;; (defn all-families? [week]
;;   (= (count family-matchups) (matchups-included-in family-matchups week)))

;; (defn families-in-one-week? [schedule]
;;   (some (fn [week] (all-families? week)) schedule))

(defn insert-managers [managers week]
  (set (replace managers week)))

(defn manager-schedule [managers schedule]
  (map #(map (partial insert-managers managers) %) schedule))

(defn possible-schedules [seeded-schedule]
  (loop [managers seeded-managers
         remaining-rotations (count seeded-managers)
         result []]
    (let [schedule (manager-schedule managers seeded-schedule)]
      (if (zero? remaining-rotations)
        result
        (recur
         (into [] (rotate managers))
         (dec remaining-rotations)
         (conj result schedule))))))

(defn print-schedule [schedule]
  (doseq [[week-number week-matchups] (map vector (drop 1 (range)) schedule)]
    (let [number-of-family (matchups-included-in family-matchups week-matchups)
          number-of-rivalries (matchups-included-in rival-matchups week-matchups)
          flag (if (> number-of-family 2) "******" "")]
      (println week-number week-matchups number-of-family number-of-rivalries flag))))

(defn -main
  [& args]
  ;; check all rivalries and families are in managers (check typos)

  (let [rr-schedule (round-robin-schedule (count seeded-managers) league-weeks)
        schedules (possible-schedules rr-schedule)]

    (print-schedule rr-schedule)
    (println "----------")

    (doseq [schedule schedules]
      (println "*********")
      (print-schedule schedule)
      (println "*********"))
    (println "----------")


    ;; (println (families-in-one-week? manager-schedule)))

    ))
