(ns othello.core
  (:require [clojure.pprint :as pp]
            [clojure.set :as s]
            [clojure.java.io :as io]))

(def player-symbol {:white 'o
                    :black 'x
                    :valid '_})

(defn empty-board [row col]
  (hash-map :row row
            :col col
            :board (vec (repeat row (vec (repeat col '.))))))

(defn center-start [board]
  (let [x2 (quot (:row board) 2)
        x1 (dec x2)
        y2 (quot (:col board) 2)
        y1 (dec y2)]
    (hash-map :white #{[x1 y1] [x2 y2]}
              :black #{[x1 y2] [x2 y1]})))

(def board (empty-board 8 8))

(def start-state (center-start board))

(defn format-board [b]
  (do (apply print "" (range (:row b)))
      (prn)
      (doseq [[i row] (map-indexed list (:board b))]
        (print row i "\n"))))

(defn spit-board
  ([state] (spit-board state board))
  ([state board]
   (format-board
     (reduce (fn [b [player positions]]
               (let [sym (player player-symbol)]
                 (reduce (fn [b [x y]]
                           (assoc-in b [:board x y] sym))
                         b
                         positions)))
             board
             state))))



(defn- potential-fields
  ([state] potential-fields state board)
  ([state board]
   (let [both-state (apply into (vals state))
         get-neighbour-1d (fn [x] (vector (dec x) x (inc x)))
         rows (:row board)
         cols (:col board)]
     (set (mapcat
            (fn [[row col]]
              (for [x (get-neighbour-1d row)
                    y (get-neighbour-1d col)
                    :when (and (not (both-state [x y]))
                               (< -1 x rows)
                               (< -1 y cols))]
                [x y]))
            both-state)))))

(def directions #{[-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1]})

(defn valid-field [field state player]
  (set (mapcat (fn [direction]
                 (let [player-positions (player state)
                       opponent-positions ((if (= :white player) :black :white) state)
                       chain (iterate #(map + direction %) field)
                       in-between (take-while opponent-positions (rest chain))
                       full (take (+ 2 (count in-between)) chain)]
                   (if (player-positions (last full))
                     in-between
                     '()
                     )))
               directions)))

(defn valid-fields=>to-reverse [state player board]
  (->> (potential-fields state board)
       (map #(vector % (valid-field % state player)))
       (remove (fn [[field to-reverse]] (empty? to-reverse)))
       (into {})))

(defn get-user-input [p valid-fields]
  (print (str "Player " (p player-symbol) " turn.\nEnter two integers separated by a space: "))
  (flush)
  (let [input-line (read-line)
        integers (map #(Integer/parseInt %) (re-seq #"\d+" input-line))]
    (cond
      (not= 2 (count integers)) (do (println "Invalid input. Please enter exactly two integers separated by a space.\n")
                                 (recur p valid-fields))
      (valid-fields (vec integers)) (vec integers)
      :else (do (println "Can't choose that field!\n")
                (recur p valid-fields)))))


(defn count-winner [final-state]
  (let [white (count (:white final-state))
        black (count (:black final-state))]
    (cond
      (< white black) (println (str "Black won " black " : " white))
      (> white black) (println (str "White won " white " : " black))
      :else (println "Its a draw!"))))

(declare play)

(defn- resume-play [state board move vf->tr]
  (let [valid-fields (set (keys vf->tr))
        next-move (if (= move :white) :black :white)
        _ (spit-board (assoc state :valid valid-fields))
        chosen-field (get-user-input move valid-fields)
        reverse-fields (get vf->tr chosen-field)
        state (-> state
                  (update move s/union (into reverse-fields (list chosen-field)))
                  (update next-move s/difference reverse-fields))]
    #(play state board next-move true)))


(defn play
  ([] (play start-state board :white true))
  ([state board move moved?]
   (let [vf->tr (valid-fields=>to-reverse state move board)
         next-move (if (= move :white) :black :white)]
     (cond
       (seq vf->tr) #(resume-play state board move vf->tr)
       moved? (recur state board next-move false)
       :else (do (spit-board state)
                 (count-winner state))
       ))
   ))

(defn -main []
  (trampoline play))


