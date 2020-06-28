(ns ^:figwheel-hooks minesweeper.core
  (:require [reagent.core :as r]
            [reagent.dom]
            [minesweeper.game :as game]
            [clojure.string :as str]))

(def count-colors {1 "rgb(0,0,254)"
                   2 "rgb(0,128,0)"
                   3 "rgb(254,0,0)"
                   4 "rgb(0,0,128)"
                   5 "rgb(128,0,0)"
                   6 "rgb(0,128,128)"
                   7 "rgb(0,0,0)"
                   8 "rgb(128,128,128)"})

(def difficulties {:test         '(5 5 2)
                   :beginner     '(9 9 10)
                   :intermediate '(16 16 40)
                   :expert       '(30 16 99)})
(def default-difficulty :test)

(defonce selected-difficulty (r/atom default-difficulty))

(defonce selected-cell (r/atom nil))

(defonce state (r/atom (apply game/new-game (default-difficulty difficulties))))

;; TODO: move into game state
(defonce playtime (r/atom 0))
(defonce timer-token (r/atom (js/setInterval #(swap! playtime inc) 1000)))

;; TODO: refactor
(defn cell-content [{:keys [flagged bombs counts]} pt]
  (cond
    (flagged pt) "\uD83D\uDEA9"
    (bombs pt) "\uD83D\uDCA3"
    (= (get counts pt) 0) " "
    :else [:span {:style {:color (count-colors (get counts pt))}} (get counts pt)]))

(defn prevent-default [f]
  "Wraps `f` in a handler to prevent the default event action."
  (fn [e] (do (.preventDefault e) (f e))))

(defn reveal-cell [pt]
  (swap! state game/take-action :reveal pt))

(defn toggle-flag-cell [pt]
  (swap! state game/take-action :flag pt))

(defn reset-timer []
  (js/clearInterval @timer-token)
  (reset! timer-token (js/setInterval #(swap! playtime inc) 1000))
  (reset! playtime 0))

(defn stop-timer []
  (when (some? @timer-token) (js/clearInterval @timer-token)))

(defn reset-game []
  (reset-timer)
  (reset! state (apply game/new-game (@selected-difficulty difficulties))))

(defn context-action []
  (when (some? @selected-cell) (swap! state game/take-action :context @selected-cell)))

(def space-keycode 32)
(defn when-space [f]
  (fn [e] (when (= space-keycode (.-keyCode e)) (do (.preventDefault e) (f)))))

(defn mouse-over [pt]
  (fn [] (reset! selected-cell pt)))

(defn mouse-leave [e]
  (reset! selected-cell nil))

(defn grid-cell [{:keys [revealed flagged] :as state} pt]
  ^{:key pt}
  [:div.cell
   {:class           (if (revealed pt) "revealed" "concealed")
    :on-click        #(reveal-cell pt)
    :on-context-menu (prevent-default #(toggle-flag-cell pt))
    :on-mouse-over   (mouse-over pt)}
   (if (or (revealed pt) (flagged pt)) (cell-content state pt))])

(defn grid-row [state [[row-num _] :as cells]]
  ^{:key row-num}
  [:div.row (map #(grid-cell state %) cells)])

(defn endgame-overlay []
  ;; TODO: this will be called on each render, should be an event
  (stop-timer)
  [:div.overlay [:span (if (game/win? @state) "🥳" "😭")]])

(defn grid-ui [{:keys [width height] :as state}]
  (let [pos (game/positions width height)
        rows (partition-all width pos)]
    [:div.grid
     {:on-mouse-leave mouse-leave}
     (map #(grid-row state %) rows)
     ;; TODO: use game's state
     (when (or (game/win? state) (game/lose? state)) [endgame-overlay])
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
  [:div.bombs-remaining (str "💣" (game/flags-remaining @state))])

(defn game-stats []
  [:div.game-stats
   [timer]
   [bombs-remaining]])

(defn minesweeper []
  [:div.game
   [game-stats]
   [grid-ui @state]
   [controls]])

(defn ^:after-load run []
  (reagent.dom/render [minesweeper] (js/document.getElementById "app")))

(defonce init-block
         (do
           (.addEventListener js/document "keydown" (when-space context-action))
           (run)))

;; TODOs:
;; [OK] 1. Add proper game loop with win/lose
;; [OK] 2. Add difficulties
;; [OK] 3. Improve UI (bombs remaining, clock)
;; [OK] 4. Add space functionality
;; 5. Fix first-click bomb
;; 6. Add distribution settings (dev/prod)
;; 7. Improve design
