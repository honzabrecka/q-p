(ns sandbox.tictactoe
  (:require [cljs.nodejs :as node]
            [cognitect.transit :as t]
            [cljs.test :refer-macros [deftest is testing run-tests]]))

(node/enable-util-print!)

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

(defn switch-token [v]
  (if (= v :x) :o :x))

(defn player-dispatch [{:keys [player] :as game}]
  (get-in game [player :type]))

(defmulti move player-dispatch)

(defmethod move :random [{:keys [player] :as game}]
  (update game :moves #(let [grid (last %)]
                        (conj % (assoc grid (rand-nth (empty-indices grid)) player)))))

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
    (println evaluated-actions)
    (rand-nth (map first max-actions))))

(defmethod move :q [{:keys [player moves] :as game}]
  (let [grid (last moves)
        {:keys [q epsilon]} (get game player)
        empty-indices# (empty-indices grid)
        move-index (if (< (rand) epsilon)
                     (rand-nth empty-indices#)
                     (get-max-from-q q grid empty-indices#))]
    (-> game
        (update :moves #(conj % (assoc grid move-index player)))
        (update player #(assoc % :last-grid grid
                                 :last-move move-index)))))

(defn evaluate [{:keys [player moves] :as game}]
  (let [grid (last moves)]
    (cond
      (win? player grid)
        (assoc game :state :win)
      (= (set grid) #{:x :o})
        (assoc game :state :tie)
      :else
        game)))

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

(defmethod reward :q [{:keys [state player moves] :as game}]
  (let [grid (last moves)]
    (condp = state
      :tie
        (-> game
            (update :x (learn grid 0.5))
            (update :o (learn grid 0.5)))
      :win
        (-> game
            (update player (learn grid 1.0))
            (update (switch-token player) (learn grid -1.0)))
      (update game (switch-token player) (learn grid 0.0)))))

(defn- -play [game]
  (-> game
      (update :player switch-token)
      (move)
      (evaluate)
      (reward)))

(defn play [playerX playerO]
  (loop [{:keys [state]:as game}
         {:moves [empty-grid]
          :x playerX
          :o playerO
          :player :x
          :state :playing}]
    (if (= state :playing)
      (recur (-play game))
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

(defn train [playerX playerO n]
  (if (zero? n)
    {:x playerX
     :o playerO}
    (let [{:keys [x o]} (play playerX playerO)]
      (recur (reset-q-player x)
             (reset-q-player o)
             (dec n)))))

