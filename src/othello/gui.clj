(ns othello.gui
  (:require [cljfx.api :as fx]
            [othello.core :as game]
            [othello.models.min-max :as minmax]))


(game/restart-state {:game-mode minmax/minimax})
(def *state game/state)
;(def *state (game/restart-state {:game-mode minmax/minimax}))

(defn empty-fields-without [used-fields]
  (for [i (range 8)
        j (range 8)
        :when (not (used-fields [i j]))]
    {:fx/type          :rectangle
     :width            75
     :height           75
     :fill             :silver
     :grid-pane/column j
     :grid-pane/row    i}))

(defn played-chips [{:keys [white black]}]
  (map (fn [[i j :as field]]
         {:fx/type          :circle
          :radius           37.5
          :fill             (cond
                              (white field) :snow
                              (black field) :black
                              :else :darkgray)
          :grid-pane/column j
          :grid-pane/row    i
          })
       (concat white black)))

(defn to-choose-fields [valid-fields turn game-mode]
  (let [color (case turn
                :white "lawngreen"
                "#ff0026")]
    (map (fn [[i j]]
           {:fx/type          :rectangle
            :width            75
            :height           75
            :style            (format "-fx-fill: silver; -fx-stroke-type: inside;
                                      -fx-stroke: %s; -fx-stroke-width: 7;" color)
            :grid-pane/column j
            :grid-pane/row    i
            :on-mouse-clicked (fn [_] (when (or (= game-mode :2-player) (= turn :white))
                                        (prn turn [i j])
                                        (game/take-player-move [i j])))})
         valid-fields)))

(defn grid-pane [{:keys [turn game-mode played-fields valid-fields->to-reverse]}]
  (let [valid-fields (set (keys valid-fields->to-reverse))]
    {:fx/type   :grid-pane
     :alignment :center
     :children  (concat (empty-fields-without valid-fields)
                        (played-chips played-fields)
                        (to-choose-fields valid-fields turn game-mode))
     :hgap      10
     :vgap      10
     :style     "-fx-background-color: grey;"
     }))


(defn root [state]
  {:fx/type          :stage
   :showing          true
   :title            "Othello"
   :on-close-request (fn [_] (System/exit 0))
   :scene            {:fx/type :scene
                      :root    {:fx/type     :stack-pane
                                :pref-width  800
                                :pref-height 800
                                :children    [(grid-pane state)]}}})

(def renderer
  (fx/create-renderer
    :middleware (fx/wrap-map-desc assoc :fx/type root)))


(fx/mount-renderer *state renderer)


