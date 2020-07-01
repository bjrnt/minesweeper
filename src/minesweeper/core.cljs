(ns minesweeper.core
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

(def difficulties {
                   ;:test         '(5 5 2)
                   :beginner     '(9 9 10)
                   :intermediate '(16 16 40)
                   :expert       '(30 16 99)})

(def default-difficulty
  ;:test
  :beginner
  )

(defonce selected-difficulty (r/atom default-difficulty))

(defonce selected-cell (r/atom nil))

(defonce state (r/atom (apply game/new-game (default-difficulty difficulties))))

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

(defn reset-game []
  (reset! state (apply game/new-game (@selected-difficulty difficulties))))

(defn context-action []
  (when (some? @selected-cell) (swap! state game/take-action :context @selected-cell)))

(def space-keycode 32)
(defn when-space [f]
  (fn [e] (when (= space-keycode (.-keyCode e)) (do (.preventDefault e) (f)))))

(defn mouse-over [pt]
  (fn [] (println pt) (reset! selected-cell pt)))

(defn mouse-leave [_]
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

(defn endgame-overlay [state]
  (when (not= :active state)
    [:div.overlay
     [:span (case state
              :win "🥳"
              :lose "😭")]]))

(defn board [{:keys [width height state] :as game}]
  (let [pos (game/positions width height)
        rows (partition-all width pos)]
    [:div.grid
     {:on-mouse-leave mouse-leave}
     (map #(grid-row game %) rows)
     [endgame-overlay state]
     ]))

;; TODO: figure out how to simplify this
(defn timer []
  (let [timer-handle (r/atom nil)
        current-time (r/atom (js/Date.))]
    (r/create-class
      {:display-name
       "timer"
       :component-did-mount
       (fn [_] (reset! timer-handle (js/setInterval #(reset! current-time (js/Date.)) 1000)))
       :component-did-update
       (fn [this old-argv]
         (let [new-argv (rest (r/argv this))]
           (when (not= (first new-argv) (second old-argv))
             (println (first new-argv) (second old-argv))
             (js/clearInterval @timer-handle)
             (reset! timer-handle (js/setInterval #(reset! current-time (js/Date.)) 1000))
             (reset! current-time (js/Date.)))))
       :reagent-render
       (fn [start-time end-time]
         [:div.timer
          (str "⏱" (int (/ (- (.getTime (or end-time @current-time)) (.getTime start-time)) 1000)))])})))

(defn difficulty-selector []
  [:select
   {:on-change #(reset! selected-difficulty (keyword (.-value (.-target %))))
    :value     @selected-difficulty}
   (map (fn [difficulty]
          ^{:key difficulty}
          [:option
           {:value difficulty}
           (str/capitalize (name difficulty))])
        (keys difficulties))])

(defn controls []
  [:div.controls
   [difficulty-selector]
   [:button.reset
    {:on-click reset-game}
    "Reset"]])

(defn bombs-remaining []
  [:div.bombs-remaining (str "💣" (max 0 (game/flags-remaining @state)))])

(defn game-stats [state]
  [:div.game-stats
   [timer (:start-time state) (:end-time state)]
   [bombs-remaining]])

(defn minesweeper []
  [:div.game
   [game-stats @state]
   [board @state]
   [controls]])

(defn reload! []
  (reagent.dom/render [minesweeper] (js/document.getElementById "app")))

(defn main! []
  (do
    (.addEventListener js/document "keydown" (when-space context-action))
    (reload!)))

;; TODOs:
;; [OK] 1. Add proper game loop with win/lose
;; [OK] 2. Add difficulties
;; [OK] 3. Improve UI (bombs remaining, clock)
;; [OK] 4. Add space functionality
;; 5. Fix first-click bomb
;; 6. Add distribution settings (dev/prod)
;; 7. Improve design
