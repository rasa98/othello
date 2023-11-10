(ns othello.core
  (:require [clojure.set :as s]))

(declare state
         board)

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

(defn format-board [b]
  (do (apply print "" (range (:row b)))
      (prn)
      (doseq [[i row] (map-indexed list (:board b))]
        (print row i "\n"))))


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

(declare check-game-state
         check-game-state-full)


(defn play-next-move [chosen-field]
  (let [s @state
        turn (:turn s)
        next-turn (next-player-turn turn)
        vf->tr (:valid-fields->to-reverse s)
        reverse-fields (get vf->tr chosen-field)
        s (-> s
              (update-in [:played-fields turn] s/union (into reverse-fields (list chosen-field)))
              (update-in [:played-fields next-turn] s/difference reverse-fields)
              (assoc :turn next-turn))
        s (assoc s :valid-fields->to-reverse (valid-fields=>to-reverse (:played-fields s) (:turn s)))]
    (reset! state s)
    (check-game-state)))

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

#_(defn check-game-state-full-old []
  (let [s @state
        f #(seq (:valid-fields->to-reverse %))]
    (if (f s)
      true
      (let [s (change-turn s)]
        (if (f s)
          (swap! state assoc :valid-fields->to-reverse s)
          :end)))))

(let [f #(seq (:valid-fields->to-reverse %))]
  (defn check-game-state-full
    ([]
     (if (f @state)
       true
       (check-game-state-full (change-turn @state))))
    ([s]
     (if (f s)
       (swap! state assoc :valid-fields->to-reverse s)
       :end))))

(def board (empty-board 8 8))

(let [player-fields (center-start board)
      turn :white
      valid-fields->to-reverse (valid-fields=>to-reverse player-fields turn)]
  (def state (atom {:played-fields            player-fields
                    :valid-fields->to-reverse valid-fields->to-reverse
                    :turn                     turn})))

(defn check-game-state []
  (case (check-game-state-full)
    :end (count-winner (:played-fields @state))
    :waiting))


