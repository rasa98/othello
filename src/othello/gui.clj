(ns othello.gui
  (:require [cljfx.api :as fx]
            [othello.core :as game]))

(def *state game/state)

#_(defn grid-pane [{:keys [played-fields valid-fields->to-reverse]}]
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
(defn grid-pane [{:keys [turn ai played-fields valid-fields->to-reverse]}]
  {:fx/type :grid-pane
   :alignment :center
   :children (concat (for [i (range 8)
                           j (range 8)]
                             {:fx/type          :rectangle
                              :width            75
                              :height           75
                              :fill             :silver
                              :grid-pane/column j
                              :grid-pane/row    i
                              :on-mouse-clicked (fn [_] (when (and
                                                                (or (= :2-player ai)
                                                                    (= turn :white)) ;assuming ai can only be black
                                                                ((set (keys valid-fields->to-reverse)) [i j]))
                                                          (do (prn [i j]) (game/play-next-move [i j]))))})
                           (let [{:keys [white black]} played-fields]
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
                     (map (fn [[i j]]
                            {:fx/type          :rectangle
                             :width            75
                             :height           75
                             :style            "-fx-fill: silver; -fx-stroke-type: inside;
                                                -fx-stroke: lawngreen; -fx-stroke-width: 7;"
                             :grid-pane/column j
                             :grid-pane/row    i
                             :on-mouse-clicked (fn [_] (when (and
                                                               (or (= :2-player ai)
                                                                   (= turn :white)) ;assuming ai can only be black
                                                               ((set (keys valid-fields->to-reverse)) [i j]))
                                                         (do (prn [i j]) (game/play-next-move [i j]))))})
                          (set (keys valid-fields->to-reverse))))
   :hgap 10
   :vgap 10
   :style "-fx-background-color: grey;"
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


