(ns stmf-ff.core
  (:require [clojure.math.combinatorics :as combinatorics]
            [clojure.set :refer [difference]]))

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

;; (def set-schedule
;;   [[11 ["Phillip" "Kris"]]
;;    [11 ["Paul" "Mike"]]
;;    [11 ["Dave" "Brad"]]
;;    [11 ["Jeff" "Stewart"]]
;;    [11 ["Angie" "Brian"]]
;;    [11 ["John" "Annie"]]
;;    [11 ["Ian" "Drew"]]
;;    [12 ["Phillip" "Angie"]]
;;    [12 ["Paul" "Stewart"]]
;;    [12 ["Dave" "Annie"]]
;;    [12 ["Jeff" "John"]]
;;    [12 ["Ian" "Brad"]]
;;    [12 ["Kris" "Brian"]]
;;    [12 ["Drew" "Mike"]]])

(def set-schedule
  (merge
   (into (sorted-map) (map (fn [week] [week []]) (range 1 14)))
   {11 [#{"Phillip" "Kris"}
        #{"Paul" "Mike"}
        #{"Dave" "Brad"}
        #{"Jeff" "Stewart"}
        #{"Angie" "Brian"}
        ;; #{"John" "Annie"}
        #{"Ian" "Drew"}]
    12 [#{"Phillip" "Angie"}
        #{"Paul" "Stewart"}
        #{"Dave" "Annie"}
        #{"Jeff" "John"}
        #{"Ian" "Brad"}
        ;; #{"Kris" "Brian"}
        #{"Drew" "Mike"}]}))


(defn already-exists? [players matchups]
  (some #(some players %) matchups))

(defn can-fit? [matchup [_ week-matchups]]
  (not (already-exists? matchup week-matchups)))

(defn find-week-for [matchup schedule]
  (first
   (for [[week-number _ :as week] schedule :when (can-fit? matchup week)] week-number)))

(defn schedule-matchups [needed-matchups current-schedule]
  ;; first week not full
  ;; for each remaining matchup
  ;;   if not already scheduled this week
  ;;     place in schedule
  ;;     schedule matchups (other remaining-matchups new-schedule)



  ;; first matchup
  ;; first week it can go in


  (if-let [needed-matchup (first needed-matchups)]
    ;; (if-let [fit-week (first (filter (partial can-fit-matchup? needed-matchup) current-schedule))]
    ;; (if-let [fit-week (some (partial can-fit-matchup? needed-matchup) current-schedule)]
    ;; (if-let [fit-week (first (keep (partial can-fit-matchup? needed-matchup) current-schedule))]
    ;; (if-let [fit-week (some (partial can-fit-matchup? needed-matchup) current-schedule)]
    ;; (if-let [fit-week (first (for [[week-number week-matchups] current-schedule :when ()] week-number))]
    (if-let [fit-week (find-week-for needed-matchup current-schedule)]
      (schedule-matchups (rest needed-matchups) (update current-schedule fit-week #(conj % needed-matchup)))
      current-schedule)
    current-schedule))

(defn -main
  [& args]
  (let [all-matchups (map set (combinatorics/combinations managers 2))
        existing-matchups (reduce concat (map second set-schedule))
        remaining-matchups (difference (set all-matchups) (set existing-matchups))]
    (println "all-matchups" (count all-matchups) ":" all-matchups)
    (println "****")
    (println "existing matchups" (count existing-matchups) ":" existing-matchups)
    (println "****")
    (println "matchups remaining" (count remaining-matchups) ":" remaining-matchups)
    (println "****")
    (println "set schedule:" set-schedule)
    (println "****")
    (let [new-schedule (schedule-matchups remaining-matchups set-schedule)
          scheduled-matchups (reduce concat (map second new-schedule))
          unscheduled-matchups (difference (set all-matchups) (set scheduled-matchups))]
      (println "new schedule:" new-schedule)
      (println "matchups not scheduled:" (count unscheduled-matchups) unscheduled-matchups)))
  )
