(ns sandbox.core
  (:require [rum.core :as rum]
            [cognitect.transit :as t]
            [sandbox.tictactoe :as tictactoe]))

(defmethod tictactoe/move :human [{:keys [player] :as game}]
  (let [{:keys [index]} (get game player)]
    (update game :grid #(assoc % index player))))

(def human-player
  {:type :human
   :index -1})

(def tr (t/reader :json))

(defonce state (atom (tictactoe/new-game
                       (merge tictactoe/q-player
                              {:q (t/read tr (aget js/window "q"))
                               :epsilon 0})
                       human-player
                       :o)))

(defn move-with-check [game f]
  (let [game# (-> game
                  tictactoe/move
                  tictactoe/evaluate)]
    (if (= (:state game#) :playing)
      (f (update game# :player tictactoe/switch-player))
      game#)))

(defn play [index]
  (reset! state
          (-> @state
              (assoc-in [:o :index] index)
              (move-with-check #(move-with-check % identity)))))

(def one-px-transparent-image
  "data:image/gif;base64,R0lGODlhAQABAIAAAAAAAP///yH5BAEAAAAALAAAAAABAAEAAAIBRAA7")

(rum/defc app < rum/reactive []
  (let [*state (rum/react state)]
    [:div.cc
     [:h1 "q-p"]
     [:div.grid
      (map (fn [y]
             [:div.row {:key (str "row-" y)}
              (map (fn [x]
                     (let [index (+ x (* y 3))
                           token (get (:grid *state) index)]
                       [:div.cell {:key (str "cell-" y "-" x)
                                   :onClick #(when (tictactoe/_? token)
                                              (play index))}
                        [:img {:src one-px-transparent-image}]
                        (case token
                          :x [:div.token.x]
                          :o [:div.token.o]
                          nil)]))
                   (range 3))])
           (range 3))
      (when (not= (:state *state) :playing)
        [:div.end {:onClick #(reset! state (merge @state
                                                  {:state :playing
                                                   :player :o
                                                   :grid tictactoe/empty-grid}))}])]]))

(defn render []
  (rum/mount (app) (.getElementById js/document "app")))

(render)

(defn on-js-reload [])
