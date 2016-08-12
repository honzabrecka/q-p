(ns sandbox.tictactoe
  (:require [cognitect.transit :as t]
            [cljs.test :refer-macros [deftest is testing run-tests]]))

(def empty-grid
  [:_ :_ :_
   :_ :_ :_
   :_ :_ :_])

(defn filter-kv [pred]
  (comp (keep-indexed vector)
        (filter (fn [[i v]] (pred v)))
        (map first)))

(deftest filter-kv-test
  (is (= [1 3 4]
         (into [] (filter-kv #(= :b %)) [:a :b :a :b :b]))))

(defn _? [v]
  (= v :_))

(defn empty-indices [col]
  (into [] (filter-kv _?) col))

(defn non-empty-indices [col]
  (into [] (filter-kv (complement _?)) col))

(def win-patterns
  (into #{}
        (map non-empty-indices)
        [[:x :x :x
          :_ :_ :_
          :_ :_ :_]
         [:_ :_ :_
          :x :x :x
          :_ :_ :_]
         [:_ :_ :_
          :_ :_ :_
          :x :x :x]
         [:x :_ :_
          :x :_ :_
          :x :_ :_]
         [:_ :x :_
          :_ :x :_
          :_ :x :_]
         [:_ :_ :x
          :_ :_ :x
          :_ :_ :x]
         [:x :_ :_
          :_ :x :_
          :_ :_ :x]
         [:_ :_ :x
          :_ :x :_
          :x :_ :_]]))

(defn win? [token grid]
  (some (fn [pattern]
          (->> pattern
               (map #(get grid %))
               (set)
               (= #{token})))
        win-patterns))

(defn switch-player [v]
  (if (= v :x) :o :x))

(defn player-dispatch [{:keys [player] :as game}]
  (get-in game [player :type]))

(defmulti move player-dispatch)

(defmethod move :random [{:keys [player] :as game}]
  (update game :grid #(assoc % (rand-nth (empty-indices %)) player)))

(defn get-max-from-q [q grid actions]
  (let [evaluated-actions (->> actions
                               (map (fn [action]
                                      [action (get q [grid action] 1.0)]))
                               (sort-by second))
        max-action-value (->> evaluated-actions
                              (map second)
                              (sort)
                              (last))
        max-actions (filter #(= (second %) max-action-value) evaluated-actions)]
    (rand-nth (map first max-actions))))

(defmethod move :q [{:keys [player grid] :as game}]
  (let [{:keys [q epsilon]} (get game player)
        empty-indices# (empty-indices grid)
        move-index (if (< (rand) epsilon)
                     (rand-nth empty-indices#)
                     (get-max-from-q q grid empty-indices#))]
    (-> game
        (update :grid #(assoc % move-index player))
        (update player #(assoc % :last-grid grid
                                 :last-move move-index)))))

(defn evaluate [{:keys [player grid] :as game}]
  (cond
    (win? player grid)
      (assoc game :state :win)
    (= (set grid) #{:x :o})
      (assoc game :state :tie)
    :else
      game))

(defmulti reward player-dispatch)

(defmethod reward :random [game]
  game)

(defn learn [grid reward]
  (fn [{:keys [last-grid last-move alpha gamma q] :as m}]
    (if (= last-move -1)
      m
      (let [prev (get q [last-grid last-move] 1.0)
            maxv (apply max (map #(get q [grid %] 1.0) (empty-indices last-grid)))
            v (+ prev (* alpha (- (+ reward (* gamma maxv)) prev)))]
        (update m :q #(assoc % [last-grid last-move] v))))))

(defmethod reward :q [{:keys [state player grid] :as game}]
  (condp = state
    :tie
      (-> game
          (update :x (learn grid 0.5))
          (update :o (learn grid 0.5)))
    :win
      (-> game
          (update player (learn grid 1.0))
          (update (switch-player player) (learn grid -1.0)))
    (update game (switch-player player) (learn grid 0.0))))

(defn new-game [playerX playerO start-player]
  {:grid empty-grid
   :x playerX
   :o playerO
   :player start-player
   :state :playing})

(defn- play [playerX playerO start-player]
  (loop [{:keys [state] :as game} (new-game playerX playerO start-player)]
    (if (= state :playing)
      (recur (-> game
                 (move)
                 (evaluate)
                 (reward)
                 (update :player switch-player)))
      game)))

(def random-player
  {:type :random})

(def q-player
  {:type :q
   :q {}
   :alpha 0.3
   :gamma 0.9
   :epsilon 0.2
   :last-move -1
   :last-grid empty-grid})

(defn reset-q-player [m]
  (assoc m :last-move -1
           :last-grid empty-grid))

(defn train [playerX playerO iterations on-progress]
  (loop [playerX playerX
         playerO playerO
         start-player :x
         n 1]
    (if (= n iterations)
      {:x playerX
       :o playerO}
      (let [{:keys [x o player]} (play playerX playerO start-player)
            _ (on-progress n iterations)]
        (recur (reset-q-player x)
               (reset-q-player o)
               player
               (inc n))))))
