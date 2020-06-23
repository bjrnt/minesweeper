(ns ^:figwheel-hooks minesweeper.core
  (:require [reagent.core :as r]
            [reagent.dom]
            [minesweeper.game :as game]
            [clojure.string :as str]))

(def difficulties {:test         '(5 5 2)
                   :beginner     '(9 9 10)
                   :intermediate '(16 16 40)
                   :expert       '(30 16 99)})
(def default-difficulty :test)

(defonce selected-difficulty (r/atom default-difficulty))

(defonce grid (r/atom (apply game/reset (default-difficulty difficulties))))

(defonce playtime (r/atom 0))
(defonce timer-token (r/atom (js/setInterval #(swap! playtime inc) 1000)))

(defn cell-content [c]
  (cond
    (:flagged c) "\uD83D\uDEA9"
    (:bomb c) "\uD83D\uDCA3"
    (= (:count c) 0) " "
    :else (:count c)))

(defn prevent-default [f]
  "Wraps `f` in a handler to prevent the default event action."
  (fn [e] (do (.preventDefault e) (f e))))

(defn reveal-cell [[pt c]]
  (when-not (:flagged c) (swap! grid game/reveal pt)))

(defn toggle-flag-cell [[pt c]]
  (when-not (:revealed c) (swap! grid game/toggle-flagged pt)))

(defn reset-timer []
  (js/clearInterval @timer-token)
  (reset! timer-token (js/setInterval #(swap! playtime inc) 1000))
  (reset! playtime 0))

(defn stop-timer []
  (when (some? @timer-token) (js/clearInterval @timer-token)))

(defn reset-game []
  (reset-timer)
  (reset! grid (apply game/reset (@selected-difficulty difficulties))))

(defn grid-cell [[pt c]]
  ^{:key pt}
  [:div.cell
   {:class           (if (:revealed c) "revealed" "concealed")
    :on-click        #(reveal-cell [pt c])
    :on-context-menu (prevent-default #(toggle-flag-cell [pt c]))}
   (if (or (:revealed c) (:flagged c)) (cell-content c))])

(defn grid-row [[[row-num _] :as cells]]
  ^{:key row-num}
  [:div.row (map grid-cell cells)])

(defn endgame-overlay []
  ;; TODO: this will be called on each render, should be an event
  (stop-timer)
  [:div.overlay [:span (if (game/win? @grid) "🥳" "😭")]])

(defn grid-ui [grid]
  (let [rows (partition-all (game/grid-width grid) grid)]
    [:div.grid
     (map grid-row rows)
     (when-not (game/active? grid) [endgame-overlay])
     ]))

(defn timer []
  [:div.timer (str "⏱" @playtime)])

(defn difficulty-selector []
  [:select
   {:on-change #(reset! selected-difficulty (keyword (.-value (.-target %))))
    :value     @selected-difficulty}
   (map (fn [difficulty]
          ^{:key difficulty}
          [:option
           {:value difficulty}
           (str/capitalize (name difficulty))]
          ) (keys difficulties))])

(defn controls []
  [:div.controls
   [difficulty-selector]
   [:button.reset
    {:on-click reset-game}
    "Reset"]])

(defn bombs-remaining []
  [:div.bombs-remaining (str "💣" (game/remaining-flags-count @grid))])

(defn game-stats []
  [:div.game-stats
   [timer]
   [bombs-remaining]])

(defn simple-example []
  [:div.game
   [game-stats]
   [grid-ui @grid]
   [controls]])

(defn ^:after-load run []
  (reagent.dom/render [simple-example] (js/document.getElementById "app")))

(defonce init-block (run))

;; TODOs:
;; [OK] 1. Add proper game loop with win/lose
;; [OK] 2. Add difficulties
;; [OK] 3. Improve UI (bombs remaining, clock)
;; 4. Add space functionality
;; 5. Add distribution settings (dev/prod)
;; 6. Improve design
