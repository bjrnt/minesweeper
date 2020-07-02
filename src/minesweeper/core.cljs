(ns minesweeper.core
  (:require [minesweeper.canvas :as canvas]
            [minesweeper.game :as game]
            [oops.core :refer [oget oset!]]))

(defonce ctx (canvas/rescale! (canvas/canvas-context-from-id "canvas")))
(def square-size 100)
(def square-stroke 3)

(def game (atom (game/new-game 5 5 1)))
(defonce mouse (atom [0 0]))

(defn to-rect [[y x]]
  (let [start-x (* square-size x)
        start-y (* square-size y)]
    (merge (canvas/->Rect [start-x start-y] [square-size square-size] [119 119 119 1.0])
           {:stroke       square-stroke
            :stroke-color [0 0 0 1.0]
            :hover        true
            :hover-color  [135 135 135 1.0]})))

(defn squares [{:keys [width height] :as game}]
  (let [pos (game/positions width height)]
    (map (partial to-rect) pos)))

(defn draw! [objs]
  (canvas/clear ctx)
  (canvas/draw-scene ctx objs))

(defn update-hover [objs]
  (let [[mX mY] (canvas/pt-in-canvas ctx @mouse)]
    (map (fn [obj]
           (if-not (satisfies? canvas/Hitable obj)
             (assoc-in obj [:hovered] false)
             (assoc-in obj [:hovered] (.isPointInPath (:ctx ctx) (canvas/hit-box obj) mX mY)))) objs)))

(defn update! []
  (let [objs (concat (squares @game))]
    (-> objs
        (update-hover)
        (draw!))
    (.requestAnimationFrame js/window update!)))

(defn update-mouse [event]
  (reset! mouse [(.-clientX event) (.-clientY event)]))

(defn reload! []
  (.requestAnimationFrame js/window update!))

(defn main! []
  (.addEventListener js/document "mousemove" update-mouse)
  (oset! (:ctx ctx) "font" "24px -apple-system")
  (.requestAnimationFrame js/window update!))

;; CONSTANTS

;(def count-colors {1 "rgb(0,0,254)"
;                   2 "rgb(0,128,0)"
;                   3 "rgb(254,0,0)"
;                   4 "rgb(0,0,128)"
;                   5 "rgb(128,0,0)"
;                   6 "rgb(0,128,128)"
;                   7 "rgb(0,0,0)"
;                   8 "rgb(128,128,128)"})
;
;(defonce difficulties (merge
;                        {:beginner     '(9 9 10)
;                         :intermediate '(16 16 40)
;                         :expert       '(30 16 99)}
;                        (when util/DEV
;                          {:test '(5 5 2)})))
;
;(defonce default-difficulty (if util/DEV :test :beginner))
;
;;; STATE
;
;(defonce selected-difficulty (r/atom default-difficulty))
;
;(defonce selected-cell (r/atom nil))
;
;(defonce state (r/atom (apply game/new-game (default-difficulty difficulties))))
;
;;; STATE MUTATORS
;
;(defn reveal-cell [pt]
;  (swap! state game/take-action :reveal pt))
;
;(defn toggle-flag-cell [pt]
;  (swap! state game/take-action :flag pt))
;
;(defn reset-game []
;  (reset! state (apply game/new-game (@selected-difficulty difficulties))))
;
;(defn context-action []
;  (when (some? @selected-cell) (swap! state game/take-action :context @selected-cell)))
;
;(defn mouse-over [pt]
;  (reset! selected-cell pt))
;
;(defn mouse-leave [_]
;  (reset! selected-cell nil))
;
;;; EVENT HANDLERS
;
;(defn prevent-default [f]
;  "Wraps `f` in a handler to prevent the default event action."
;  (fn [event] (do (.preventDefault event) (f event))))
;
;(defn no-modifiers [event]
;  (not (or (.-shiftKey event) (.-metaKey event) (.-altKey event) (.-ctrlKey event))))
;
;(defn when-key [key-code f]
;  "Returns an event handler that executes `f` only if the event matches the given `key-code`, and no keyboard modifiers are present."
;  (fn [event]
;    (when (and (= key-code (.-keyCode event)) (no-modifiers event))
;      (.preventDefault event)
;      (f))))
;(def when-space (partial when-key 32))
;(def when-r (partial when-key 82))
;
;;; UI
;
;(defn cell-content [{:keys [flagged bombs counts]} pt]
;  (cond
;    (flagged pt) "\uD83D\uDEA9"
;    (bombs pt) "\uD83D\uDCA3"
;    (= (counts pt) 0) " "
;    :else [:span {:style {:color (count-colors (counts pt))}} (counts pt)]))
;
;(defn grid-cell [{:keys [revealed flagged] :as state} pt]
;  ^{:key pt}
;  [:div.cell
;   {:class           (if (revealed pt) "revealed" "concealed")
;    :on-click        #(reveal-cell pt)
;    :on-context-menu (prevent-default #(toggle-flag-cell pt))
;    :on-mouse-over   #(mouse-over pt)}
;   (if (or (revealed pt) (flagged pt)) (cell-content state pt))])
;
;(defn board-row [state [[row-num _] :as cells]]
;  ^{:key row-num}
;  [:div.row (map #(grid-cell state %) cells)])
;
;(defn endgame-overlay [state]
;  (when (not= :active state)
;    [:div.overlay
;     [:span (case state
;              :win "🥳"
;              :lose "😭")]]))
;
;(defn board [{:keys [width height state] :as game}]
;  (let [pos (game/positions width height)
;        rows (partition-all width pos)]
;    [:div.grid
;     {:on-mouse-leave mouse-leave}
;     (map #(board-row game %) rows)
;     [endgame-overlay state]]))
;
;;; TODO: figure out how to simplify this
;(defn timer []
;  (let [timer-handle (r/atom nil)
;        current-time (r/atom (js/Date.))]
;    (r/create-class
;      {:display-name
;       "timer"
;       :component-did-mount
;       (fn [_] (reset! timer-handle (js/setInterval #(reset! current-time (js/Date.)) 1000)))
;       :component-did-update
;       (fn [this old-argv]
;         (let [new-argv (rest (r/argv this))]
;           (when (not= (first new-argv) (second old-argv))
;             (println (first new-argv) (second old-argv))
;             (js/clearInterval @timer-handle)
;             (reset! timer-handle (js/setInterval #(reset! current-time (js/Date.)) 1000))
;             (reset! current-time (js/Date.)))))
;       :reagent-render
;       (fn [start-time end-time]
;         [:div.timer
;          (str "⏱" (int (/ (- (.getTime (or end-time @current-time)) (.getTime start-time)) 1000)))])})))
;
;(defn difficulty-selector []
;  [:select
;   {:on-change #(reset! selected-difficulty (keyword (.-value (.-target %))))
;    :value     @selected-difficulty}
;   (map (fn [difficulty]
;          ^{:key difficulty}
;          [:option {:value difficulty} (str/capitalize (name difficulty))])
;        (keys difficulties))])
;
;(defn controls []
;  [:div.controls
;   [difficulty-selector]
;   [:button.reset {:on-click reset-game} "Reset"]])
;
;(defn bombs-remaining []
;  [:div.bombs-remaining (str "💣" (max 0 (game/flags-remaining @state)))])
;
;(defn game-stats [state]
;  [:div.game-stats
;   [timer (:start-time state) (:end-time state)]
;   [bombs-remaining]])
;
;(defn instructions []
;  [:ul.instructions
;   [:li [:strong "Click"] " on a covered square to reveal it."]
;   [:li [:strong "Right-click"] " on a covered square to flag/unflag it."]
;   [:li [:strong "Press space"] " while hovering over a cell to take a contextual action: flag/unflag covered squares, uncover all neighbors of a uncovered square as long as there are a sufficient number of flags around it."]
;   [:li [:strong "Press R"] " to reset the game."]])
;
;(defn minesweeper []
;  [:div
;   [:div.game
;    [game-stats @state]
;    [board @state]
;    [controls]]
;   [instructions]])
;
;;; HOOKS
;
;(defn reload! []
;  (reagent.dom/render [minesweeper] (js/document.getElementById "app")))
;
;(defn main! []
;  (do
;    (.addEventListener js/document "keydown" (when-space context-action))
;    (.addEventListener js/document "keydown" (when-r reset-game))
;    (reload!)))
;
;;; TODOs:
;;; [OK] 1. Add proper game loop with win/lose
;;; [OK] 2. Add difficulties
;;; [OK] 3. Improve UI (bombs remaining, clock)
;;; [OK] 4. Add space functionality
;;; 5. Fix first-click bomb
;;; [OK] 6. Add distribution settings (dev/prod)
;;; 7. Improve design

