(ns othello.core
  (:require [clojure.set :as s]))

(declare state
         board)

(def player-types {:2-player #()
                   :ai#1 #()
                   :ai#2 #()})

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


(defn next-player-turn [current-turn]
  (case current-turn
    :white :black
    :black :white
    :computer))


(defn- potential-fields
  ([state] (potential-fields state board))
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

(defn valid-fields=>to-reverse [played-fields player]
  (->> (potential-fields played-fields board)
       (map #(vector % (valid-field % played-fields player)))
       (remove (fn [[field to-reverse]] (empty? to-reverse)))
       (into {})))


(defn count-winner [final-state]
  (let [white (count (:white final-state))
        black (count (:black final-state))]
    (cond
      (< white black) (println (str "Black won " black " : " white))
      (> white black) (println (str "White won " white " : " black))
      :else (println "Its a draw!"))))

(declare update-game-state-with
         change-turn)


(defn play-next-move
  ([chosen-field] (play-next-move chosen-field @state))
  ([chosen-field {:keys [turn game-mode] :as s}]
   (let [next-turn (next-player-turn turn)
         vf->tr (:valid-fields->to-reverse s)
         reverse-fields (get vf->tr chosen-field)
         new-s (-> s
               (update-in [:played-fields turn] s/union (into reverse-fields (list chosen-field)))
               (update-in [:played-fields next-turn] s/difference reverse-fields)
               change-turn)]
     (when (= :waiting (update-game-state-with new-s))
       ((get player-types game-mode #()))     ; call for ai to make a turn
       ))))

(defn calc-valid-fields-map [{:keys [turn played-fields]}]
  (valid-fields=>to-reverse played-fields turn))

(defn update-turn [s]
  (update s :turn next-player-turn))

(defn update-valid-fields-map [s]
  (assoc s :valid-fields->to-reverse (calc-valid-fields-map s)))

(defn change-turn [s]
  (-> s
      (update-turn)
      (update-valid-fields-map)))


(defn check-game-state
  [s]
  (let [f #(seq (:valid-fields->to-reverse %))
        s-other (change-turn s)]
    (cond
      (f s) [s :waiting]
      (f s-other) [s-other :waiting]
      :else [s :end])))

(def board (empty-board 8 8))

(let [player-fields (center-start board)
      turn :white
      valid-fields->to-reverse (valid-fields=>to-reverse player-fields turn)]
  (def state (atom {:played-fields            player-fields
                    :valid-fields->to-reverse valid-fields->to-reverse
                    :turn                     turn
                    :game-mode                :2-player
                    })))

(defn update-game-state-with [s]
  (let [[s game-state] (check-game-state s)]
    (reset! state s)
    (case game-state
      :end (count-winner (:played-fields @state))
      :waiting)))



