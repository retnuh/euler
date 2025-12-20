(defproject euler "0.1.0-SNAPSHOT"
  :description "retnuH's clojue implementations of Project Euler solutions"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0-RC3"]
                 [criterium "0.4.3"]
                 [org.clojure/algo.monads "0.1.5"]
                 [org.clojure/core.logic "0.8.9"]
                 [org.clojars.gjahad/debug-repl "0.3.3"]
                 [org.clojure/tools.macro "0.1.5"]
                 [org.clojure/math.combinatorics "0.1.1"]
                 [incanter/incanter-core "1.5.6"]
                 [org.clojure/core.cache "0.6.4"]
                 [clatrix "0.5.0"]
                 [net.mikera/core.matrix "0.47.1"]
                 [org.clojure/core.match "0.3.0-alpha4"]
                 [aysylu/loom "0.5.4"]
                 ]
  :jvm-opts ["-Xmx12g" "-Xms1g" "-server"] 
  )
