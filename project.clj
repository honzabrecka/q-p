(defproject q-p "0.1.0-SNAPSHOT"

  :min-lein-version "2.6.1"

  :dependencies [[figwheel-sidecar "0.5.4-7"]
                 ;;;
                 [org.clojure/clojure "1.9.0-alpha10"]
                 [org.clojure/clojurescript "1.9.89"]
                 [org.clojure/test.check "0.9.0"]
                 ;;;
                 [com.cognitect/transit-cljs "0.8.239"]
                 [rum "0.10.5"]]

  :plugins [[lein-figwheel "0.5.4-7"]
            [lein-cljsbuild "1.1.3" :exclusions [[org.clojure/clojure]]]]

  :local-repo "local-m2"

  :source-paths ["src" "dev"]

  :clean-targets ^{:protect false} ["resources/public/js/browser-repl" "target"]

  :cljsbuild {:builds
              [{:id "browser-repl"
                :source-paths ["src"]
                :figwheel {:on-jsload "sandbox.core/on-js-reload"
                           :open-urls ["http://localhost:3459/"]}

                :compiler {:main sandbox.core
                           :asset-path "js/browser-repl"
                           :output-to "resources/public/js/browser-repl/index.js"
                           :output-dir "resources/public/js/browser-repl"
                           :source-map-timestamp true}}

               {:id "browser-dist"
                :source-paths ["src"]
                :compiler {:main sandbox.core
                           :asset-path "js/browser-dist"
                           :output-to "resources/public/js/browser-dist/index.js"
                           :output-dir "resources/public/js/browser-dist"
                           :optimizations :advanced}}

               {:id "node"
                :source-paths ["src"]
                :compiler {:main sandbox.n
                           :target :nodejs
                           :output-to "target/n/index.js"
                           :output-dir "target/n"
                           :optimizations :none}}]})
