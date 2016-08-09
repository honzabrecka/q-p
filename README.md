# q-p

Yet another implementation of tic-tac-toe game using Q-learning.

## Usage

```cljs
(let [{:keys [x o]} (train q-player q-player 200000)]
  (play x o))
```

## Next steps

Implement neural network to be able to handle larger grids & more complex states generally.

