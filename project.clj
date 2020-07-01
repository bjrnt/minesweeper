(defproject minesweeper "0.1.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.10.1"]]
  :profiles
    {:dev
      {:dependencies [[org.clojure/clojurescript "1.10.773"]
                      [thheller/shadow-cljs "2.10.13"]
                      [reagent "1.0.0-alpha2"]]
       :resource-paths ["target"]
       :clean-targets ^{:protect false} ["target"]}})
