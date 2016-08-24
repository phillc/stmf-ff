(ns stmf-ff.core
  (:require [clojure.math.combinatorics :as combinatorics]
            [clojure.set :refer [difference
                                 intersection]]))

(def seeded-managers
  [
   "Annie"
   "Angie"
   "John"
   "Drew"
   "Brad"
   "Paul"
   "Brian"
   "Ian"
   "Jeff"
   "Dave"
   "Mike"
   "Phillip"
   "Kris"
   "Cael"
   ])


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
   #{"Phillip" "Cael"}
   #{"Paul" "Drew"}
   #{"Paul" "Jeff"}
   #{"Paul" "Mike"}
   #{"Dave" "Angie"}
   #{"Dave" "Annie"}
   #{"Dave" "Brad"}
   #{"Dave" "Ian"}
   #{"Jeff" "Drew"}
   #{"Jeff" "John"}
   #{"Angie" "Annie"}
   #{"Angie" "Brian"}
   #{"Angie" "Drew"}
   #{"Angie" "Ian"}
   #{"Angie" "Cael"}
   #{"Ian" "Annie"}
   #{"Ian" "Brad"}
   #{"Ian" "Drew"}
   #{"Brad" "Annie"}
   #{"Drew" "Mike"}
   ])

(def league-weeks 13)

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

(defn already-exists? [players matchups]
  (some #(some players %) matchups))

(defn can-fit? [matchup matchups]
  (not (already-exists? matchup matchups)))

(defn insert-managers-matchup [managers matchup]
  (set (replace managers matchup)))

(defn insert-managers-schedule [seeds schedule]
  (map #(map (partial insert-managers-matchup seeds) %) schedule))

(defn ensures-family? [matchup]
  (if (some #(some matchup %) family-matchups)
    (contains? (set family-matchups) matchup)
    true))

(defn possible-weeks' [remaining-managers current-week]
  (if (zero? (count remaining-managers))
    [current-week]
    (let [remaining-matchups (map #(into #{} %) (combinatorics/combinations remaining-managers 2))
          possible-matchups (filter #(can-fit? % current-week) (filter ensures-family? remaining-matchups))]
      (apply concat (map (fn [matchup]
                           (possible-weeks'
                            (remove matchup remaining-managers)
                            (conj current-week matchup)))
                         possible-matchups)))))

(defn possible-weeks [managers]
  (possible-weeks' managers []))

(defn first-week->seeds [week]
  ;; TODO: not sure order is being preserved,
  ;; and really to get more possibilities, need both orderings
  (into (vector) (concat (map first week) (reverse (map second week)))))

(defn print-schedule [schedule]
  (doseq [[week-number week-matchups] (map vector (drop 1 (range)) schedule)]
    (let [number-of-family (matchups-included-in family-matchups week-matchups)
          number-of-rivalries (matchups-included-in rival-matchups week-matchups)
          flag1 (if (> number-of-family 3) "***" "   ")
          flag2 (condp < number-of-rivalries
                  5 "--*--"
                  4 "---"
                  3 "-"
                  "")]
      (println week-number week-matchups number-of-family number-of-rivalries flag1 flag2))))

(defn big-rivalry-week? [schedule]
  (let [minimum-rivalries 6]
    (some #(>= (matchups-included-in rival-matchups %) minimum-rivalries) schedule)))

(defn no-non-family-rivalries-in-first-week? [schedule]
  (let [first-week (first schedule)]
    (empty? (intersection (difference (set first-week) (set family-matchups)) (set rival-matchups)))))

(defn everyone-has-rivalry? [schedule]
  (let [rivalry-week-threshold 4
        rivalry-weeks (filter #(>= (matchups-included-in rival-matchups %) rivalry-week-threshold) schedule)
        scheduled-rivalries (intersection (set rival-matchups) (set (apply concat rivalry-weeks)))
        rivalry-participants (set (apply concat scheduled-rivalries))]
    (= rivalry-participants (set seeded-managers))))

(defn -main
  [& args]
  ;; TODO: check all rivalries and families are in managers (check typos)
  ;; TODO: check all family rivalries are in rivalries

  (let [rr-schedule (round-robin-schedule 14 13)
        weeks (possible-weeks (take 14 seeded-managers))
        seeds (map first-week->seeds weeks)
        manager-schedules (take 10 (filter (every-pred big-rivalry-week?
                                                       no-non-family-rivalries-in-first-week?
                                                       everyone-has-rivalry?)
                                           (map #(insert-managers-schedule % rr-schedule) seeds)))]

    (print-schedule rr-schedule)
    (println "----------")

    (doseq [schedule manager-schedules]
      (println "*********")
      (print-schedule schedule)
      (println "*********"))

    ))
