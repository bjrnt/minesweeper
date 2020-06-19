(defproject minesweeper "0.1.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.10.1"]]
  :profiles
    {:dev
      {:dependencies [[org.clojure/clojurescript "1.10.773"]
                      [com.bhauman/figwheel-main "0.2.9"]
                      ;; optional but recommended
                      [com.bhauman/rebel-readline-cljs "0.1.4"]
                      [reagent "1.0.0-alpha2"]]
       :resource-paths ["target"]
       :clean-targets ^{:protect false} ["target"]}}
  :aliases {"fig" ["trampoline" "run" "-m" "figwheel.main"]})
