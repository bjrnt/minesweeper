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

(defn grid-cell [[pt c]]
  ^{:key pt}
  [:div.cell
   {:class           (if (:revealed c) "revealed" "concealed")
    :on-click        #(when-not (:flagged c) (swap! grid game/reveal pt))
    :on-context-menu (prevent-default #(when-not (:revealed c) (swap! grid game/toggle-flagged pt)))}
   (if (or (:revealed c) (:flagged c)) (cell-content c))])

(defn grid-row [[[row-num _] :as cells]]
  ^{:key row-num}
  [:div.row (map grid-cell cells)])

(defn grid-ui [grid]
  (let [rows (partition-all game/width grid)]
    (map grid-row rows)))

(defn simple-example []
  [:div.grid (grid-ui (into (sorted-map) @grid))])

(defn ^:after-load run []
  (reagent.dom/render [simple-example] (js/document.getElementById "app")))

(defonce init-block (run))

;; TODOs:
;; 1. Add proper game loop with win/lose
;; 2. Add difficulties
;; 3. Improve UI (bombs remaining, clock)
;; 4. Add space functionality
