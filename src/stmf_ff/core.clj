(ns stmf-ff.core
  (:require [clojure.math.combinatorics :as combinatorics]
            [clojure.set :refer [difference
                                 intersection]]))

(def managers
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
   #{"Paul" "Stewart"}
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

(defn print-schedule [schedule]
  (doseq [week schedule]
    (println week)))

(defn matchups-included-in [matchups week]
  (count (intersection (set matchups) (set week))))

(defn all-families? [week]
  (= (count family-matchups) (matchups-included-in family-matchups week)))

(defn families-in-one-week? [schedule]
  (some (fn [week] (all-families? week)) schedule))

(defn insert-managers [managers week]
  (set (replace managers week)))

(defn -main
  [& args]
  ;; check all rivalries and families are in managers (check typos)

  (let [schedule (round-robin-schedule (count managers) league-weeks)
        manager-schedule (map #(map (partial insert-managers managers) %) schedule)]

    (print-schedule schedule)
    (println "*********")
    (println "manager-schedule")
    (print-schedule manager-schedule)
    (println "*********")
    (println (families-in-one-week? manager-schedule)))
  )
;; (let [all-matchups (map set (combinatorics/combinations managers 2))
;;       existing-matchups (reduce concat (map second set-schedule))
;;       remaining-matchups (difference (set all-matchups) (set existing-matchups))]
;;   (println "all-matchups" (count all-matchups) ":" all-matchups)
;;   (println "****")
;;   (println "existing matchups" (count existing-matchups) ":" existing-matchups)
;;   (println "****")
;;   (println "matchups remaining" (count remaining-matchups) ":" remaining-matchups)
;;   (println "****")
;;   (println "set schedule:" set-schedule)
;;   (println "****")
;;   (let [new-schedule (schedule-matchups remaining-matchups set-schedule)
;;         scheduled-matchups (reduce concat (map second new-schedule))
;;         unscheduled-matchups (difference (set all-matchups) (set scheduled-matchups))]
;;     (println "new schedule:" new-schedule)
;;     (println "matchups not scheduled:" (count unscheduled-matchups) unscheduled-matchups)))
