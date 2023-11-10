(ns othello.gui
  (:require [cljfx.api :as fx]
            [othello.core :as game]))

(def *state game/state)

(defn grid-pane [{:keys [played-fields valid-fields->to-reverse]}]
  {:fx/type :grid-pane
   :alignment :center
   :children (cons {:fx/type          :rectangle
                    :width           75
                    :height 75
                    :fill             :gray
                    :grid-pane/column 0
                    :grid-pane/row    0
                    :on-mouse-clicked (fn [event] (when ((set (keys valid-fields->to-reverse)) [0 0])
                                                    (game/play-next-move [0 0])))
                    } (for [i (range 8)
                            j (range 8)]
                        {:fx/type          :circle
                         :radius           37.5
                         ;:height 75
                         :fill             (let [{:keys [white black]} played-fields
                                                 valid-fields (set (keys valid-fields->to-reverse))
                                                 field [i j]]
                                             (cond
                                               (white field) :red
                                               (black field) :black
                                               (valid-fields field) :palegreen
                                               :else :white #_:darkgray))
                         :grid-pane/column j
                         :grid-pane/row    i
                         :on-mouse-clicked (fn [event] (when ((set (keys valid-fields->to-reverse)) [i j])
                                                         (game/play-next-move [i j])))
                         }))
   :grid-lines-visible true
   })


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


