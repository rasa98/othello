(ns othello.models.min-max
  (:require [othello.core :as game]
            [clojure.set :refer [intersection union difference]]))

(def corners #{[0 0] [0 7] [7 0] [7 7]})

(def danger-fields #{[0 1] [1 0] [1 1]
                     [0 6] [1 6] [1 7]
                     [6 0] [6 1] [7 1]
                     [6 6] [6 7] [7 6]})

(def good-to-play #{[2 2] [2 3] [2 4] [2 5]
                    [3 2]             [3 5]
                    [4 2]             [4 5]
                    [5 2] [5 3] [5 4] [5 5]})

(defn my-heuristic [{:keys [white black] :as position-state}]
  (let [all-played (union white black)
        early-game-factor (if (seq (difference good-to-play all-played)) 3 0)]
   (+
     (* 15 (- (count (intersection white corners))
              (count (intersection black corners))))
     (* (+ 3 early-game-factor) (- (count (intersection black danger-fields))
                                   (count (intersection white danger-fields))))
     (* early-game-factor (- (count (intersection white good-to-play))
                             (count (intersection black good-to-play))))
     (- (count white) (count black)))))

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
                (assoc agg :a a :res new-max-res :best-paths fields))))]
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
                (assoc agg :b b :res new-min-res :best-paths fields))))]
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

