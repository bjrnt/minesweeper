(ns ^:figwheel-hooks minesweeper.core
  (:require [reagent.core :as r]
            [reagent.dom]
            [minesweeper.game :as game]))

(defonce grid (r/atom (game/reset)))

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

(defn reset-game []
  (reset! grid (game/reset)))

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
  [:div.overlay [:span (if (game/win? @grid) "🥳" "😭")]])

(defn grid-ui [grid]
  (let [rows (partition-all game/width grid)]
    [:div.grid
     (map grid-row rows)
     (when-not (game/active? grid) [endgame-overlay])
     ]))

(defn controls []
  [:div
   [:button.reset
    {:on-click reset-game}
    "Reset"]])

(defn simple-example []
  [:div
   [grid-ui (into (sorted-map) @grid)]
   [controls]])

(defn ^:after-load run []
  (reagent.dom/render [simple-example] (js/document.getElementById "app")))

(defonce init-block (run))

;; TODOs:
;; 1. Add proper game loop with win/lose
;; 2. Add difficulties
;; 3. Improve UI (bombs remaining, clock)
;; 4. Add space functionality
