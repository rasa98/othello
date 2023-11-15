(ns othello.models.min-max
  (:require [othello.core :as game]))

(defn my-heuristic [{:keys [white black] :as position-state}]
  (- (count white) (count black)))

(declare minimax)



(defn maximize [state a b depth valid-fields]
  (let [f (fn [{max-res :res :keys [a b best-paths] :as agg} field]
            (let [res (:res (minimax (dec depth) a b (game/play-next-move field state)))
                  new-max-res (max max-res res)
                  fields (cond
                           (= max-res new-max-res res) (conj best-paths field)
                           (< max-res new-max-res) [field]
                           :else best-paths)
                  a (max a res)]
              (if (<= b a)
                (do
                  (swap! game/abc inc)
                  (reduced {:res new-max-res}))
                (-> agg
                    (assoc :a a)
                    (assoc :res new-max-res)
                    (assoc :best-paths fields)))))]
    (reduce f {:a a :b b :res Double/NEGATIVE_INFINITY :best-paths []} valid-fields)))

(defn minimize [state a b depth valid-fields]
  (let [f (fn [{min-res :res :keys [a b best-paths] :as agg} field]
            (let [res (:res (minimax (dec depth) a b (game/play-next-move field state)))
                  new-min-res (min min-res res)
                  fields (cond
                           (= min-res new-min-res res) (conj best-paths field)
                           (> min-res new-min-res) [field]
                           :else best-paths)
                  b (min b res)]
              (if (<= b a)
                (do
                  (swap! game/abc inc)
                  (reduced {:res new-min-res}))
                (-> agg
                    (assoc :b b)
                    (assoc :res new-min-res)
                    (assoc :best-paths fields)))))]
    (reduce f {:a a :b b :res Double/POSITIVE_INFINITY :best-paths []} valid-fields)))

(defn minimax
  ([depth game-state] (minimax depth
                               Double/NEGATIVE_INFINITY
                               Double/POSITIVE_INFINITY
                               game-state))
  ([depth a b game-state]
               (let [[{:keys [valid-fields->to-reverse turn] :as game-state} end] (game/check-game-state game-state)
                     {:keys [white black]} (game/count-chips game-state)
                     valid-fields (set (keys valid-fields->to-reverse))]
                 (cond
                   (= end :end) (hash-map :res (cond
                                                 (< white black) -100000
                                                 (> white black) 100000
                                                 :else 0))
                   (= depth 0) (hash-map :res (my-heuristic (:played-fields game-state)))
                   ;(= end :same-player-turn) (minimax depth a b game-state); todo: same-player-move is it good?!?
                   (= turn :white) (maximize game-state
                                             a
                                             b
                                             depth
                                             valid-fields)
                   :else (minimize game-state
                                   a
                                   b
                                   depth
                                   valid-fields)))))

