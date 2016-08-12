(ns sandbox.n
  (:require [cljs.nodejs :as node]
            [cognitect.transit :as t]
            [sandbox.tictactoe :as tictactoe]))

(def fs (node/require "fs"))

(def file "tictactoe-x.json")

(def tw (t/writer :json))

(->> (tictactoe/train tictactoe/q-player
                      tictactoe/q-player
                      200000
                      (fn [a b] (.log js/console a b)))
     :x
     :q
     (t/write tw)
     (.writeFileSync fs file))

(set! *main-cli-fn* (fn []))
