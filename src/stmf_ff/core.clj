(ns stmf-ff.core
  (:require [clojure.math.combinatorics :as combinatorics]
            [clojure.set :refer [difference
                                 intersection
                                 subset?
                                 union]]))

(def seeded-managers
  [
   "Jeff"
   "Brad"
   "Paul"
   "Phillip"
   "Mike"
   "Angie"
   "Brian"
   "Dave"
   "Cael"
   "Ian"
   "John"
   "Drew"
   "Kris"
   "Yeary"
   ])


(def family-matchups
  [#{"Phillip" "Angie"}
   #{"Jeff" "John"}
   #{"Drew" "Mike"}])

(def rival-matchups
  [#{"Phillip" "Angie"}
   #{"Phillip" "Yeary"}
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
   #{"Dave" "Brad"}
   #{"Dave" "Ian"}
   #{"Jeff" "Drew"}
   #{"Jeff" "John"}
   #{"Angie" "Yeary"}
   #{"Angie" "Brian"}
   #{"Angie" "Drew"}
   #{"Angie" "Ian"}
   #{"Angie" "Cael"}
   #{"Ian" "Brad"}
   #{"Ian" "Drew"}
   #{"Drew" "Mike"}
   #{"Cael" "Yeary"}
   ])

(def league-weeks 13)
(def family-week-number 12)

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

(defn possible-family-weeks [managers]
  (let [managers-in-family (reduce union family-matchups)
        remaining-managers (difference (set managers) managers-in-family)]
    (println ">>>> managers in family" managers-in-family)
    (println ">>>> remaining managers" remaining-managers)
    (possible-weeks' remaining-managers family-matchups)))

(defn first-week->seeds [week]
  ;; TODO: not sure order is being preserved,
  ;; and really to get more possibilities, need both orderings
  (into (vector) (concat (map first week) (reverse (map second week)))))

(defn print-schedule [schedule]
  (doseq [[week-number week-matchups] (map vector (drop 1 (range)) schedule)]
    (let [number-of-family (matchups-included-in family-matchups week-matchups)
          number-of-rivalries (matchups-included-in rival-matchups week-matchups)
          flag1 (if (= number-of-family (count family-matchups)) "***" "   ")
          flag2 (condp < number-of-rivalries
                  5 "--*--"
                  4 "---"
                  3 "-"
                  "")]
      (println week-number week-matchups number-of-family number-of-rivalries flag1 flag2))))

(defn big-rivalry-week? [schedule]
  (let [minimum-rivalries 5]
    (some #(>= (matchups-included-in rival-matchups %) minimum-rivalries) schedule)))

(defn everyone-has-rivalry? [schedule]
  (let [rivalry-week-threshold 3
        rivalry-weeks (filter #(>= (matchups-included-in rival-matchups %) rivalry-week-threshold) schedule)
        scheduled-rivalries (intersection (set rival-matchups) (set (apply concat rivalry-weeks)))
        rivalry-participants (set (apply concat scheduled-rivalries))]
    (= rivalry-participants (set seeded-managers))))

(defn -main
  [& args]
  (let [family-matchup-members (reduce union family-matchups)
        rival-matchup-members (reduce union rival-matchups)
        seeded-managers-members (set seeded-managers)]
    (if-not (subset? family-matchup-members seeded-managers-members)
      (do
        (println "family matchups not subset of seeded managers")
        (println (difference family-matchup-members seeded-managers-members)))
      (if-not (subset? rival-matchup-members seeded-managers-members)
        (do
          (println "rivalry matchups not subset of seeded managers")
          (println (difference rival-matchup-members seeded-managers-members)))
        (let [rr-schedule (round-robin-schedule 14 13)
              family-weeks (possible-family-weeks seeded-managers)
              family-seeds (map (fn [week]
                                  ;; (let [family-week-seeds (get rr-schedule (dec family-week-number))]
                                  ;;   (println ">>>??? -----" week)
                                  ;;   (println ">>!!!!!" family-week-seeds)
                                  ;;   )
                                  (into (vector) (concat (map first week) (reverse (map second week))))
                                  )
                                family-weeks)
              manager-schedules (take 100 (filter (every-pred big-rivalry-week?
                                                              everyone-has-rivalry?)
                                                  (map #(insert-managers-schedule % rr-schedule) family-seeds)))]

          (print-schedule rr-schedule)
          (println "----------" (count manager-schedules))
          (println "????" (take 2 family-seeds))

          (doseq [schedule manager-schedules]
            (println "*********")
            (print-schedule schedule)
            (println "*********"))
          )))))
