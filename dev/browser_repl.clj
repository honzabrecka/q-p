(use 'figwheel-sidecar.repl-api)

(def config
  {:server-port     3459
   :builds          (figwheel-sidecar.config/get-project-builds)
   :builds-to-start ["browser-repl"]})

(start-figwheel! config)
(cljs-repl)
