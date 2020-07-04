(ns minesweeper.core
  (:require [minesweeper.canvas :as canvas]
            [minesweeper.game :as game]
            [oops.core :refer [oget oset!]]))

(def count-colors {1 [0 0 254 1.0]
                   2 [0 128 0 1.0]
                   3 [254 0 0 1.0]
                   4 [0 0 128 1.0]
                   5 [128 0 0 1.0]
                   6 [0 128 128 1.0]
                   7 [0 0 0 1.0]
                   8 [128 128 128 1.0]})

(defonce ctx (canvas/rescale! (canvas/canvas-context-from-id "canvas")))
(def square-size 40)
(def square-stroke 1)
(def font-size 22)

(defonce game-state (atom (game/new-game 5 5 5)))
(defonce mouse (atom [0 0]))
(defonce mouse-clicked (atom false))
(defonce mouse-right-clicked (atom false))

(defn reveal-cell [pt]
  (swap! game-state game/take-action :reveal pt))

;(defn toggle-flag-cell [pt]
;  (swap! game-state game/take-action :flag pt))

(defn context-action [pt]
  (swap! game-state game/take-action :context pt))

(defn condj [coll x]
  (if x (conj coll x) coll))

(defn prevent-default [f]
  "Wraps `f` in a handler to prevent the default event action."
  (fn [event] (do (.preventDefault event) (f event) false)))

(defn click-mouse [event]
  (case (oget event "button")
    0 (reset! mouse-clicked true)
    2 (reset! mouse-right-clicked true)))

(defn text-for-cell [[x y]]
  (let [bomb ((:bombs @game-state) [y x])
        count ((:counts @game-state) [y x])
        revealed ((:revealed @game-state) [y x])
        flagged ((:flagged @game-state) [y x])]
    (cond
      flagged "\uD83D\uDEA9"
      (not revealed) nil
      bomb "💣"
      (zero? count) " "
      :else (str count))))

(defn text-color-for-cell [[x y]]
  (or (count-colors ((:counts @game-state) [y x]))
      [0 0 0 1.0]))

(defn cell-content [[x y] cell]
  (when-let [txt (text-for-cell [x y])]
    (let [[rx ry] (:pos cell)
          text (canvas/->Text
                 txt
                 [(+ rx (/ square-size 2)) (+ ry (/ square-size 2))]
                 (text-color-for-cell [x y]))]
      (-> text
          (update-in [:pos 0] #(- % (/ (canvas/text-width text (:ctx ctx)) 2)))
          (update-in [:pos 1] #(+ % (/ font-size 2)))))))

(defn cell [[x y]]
  (let [revealed ((:revealed @game-state) [y x])
        start-x (* square-size x)
        start-y (* square-size y)
        rect (merge (canvas/->Rect
                      [start-x start-y]
                      [square-size square-size]
                      (if revealed [187 187 187 1.0] [119 119 119 1.0]))
                    {:stroke         square-stroke
                     :stroke-color   [0 0 0 1.0]
                     :hover          true
                     :hover-color    (if revealed [200 200 200 1.0] [131 131 131 1.0])
                     :on-click       #(reveal-cell [y x])
                     :on-right-click #(context-action [y x])})]
    (condj [rect] (cell-content [x y] rect))))

(defn squares [{:keys [width height] :as game}]
  (let [pos (game/positions width height)]
    (mapcat (partial cell) pos)))

(defn draw! [objs]
  (canvas/clear ctx)
  (canvas/draw-scene ctx objs))

(defn update-hover [objs]
  (let [[mX mY] (canvas/pt-in-canvas ctx @mouse)]
    (map (fn [obj]
           (if-not (satisfies? canvas/Hitable obj)
             (assoc-in obj [:hovered] false)
             (assoc-in obj [:hovered] (.isPointInPath (:ctx ctx) (canvas/hit-box obj) mX mY)))) objs)))

(defn update-clicked [objs]
  (when (or @mouse-clicked @mouse-right-clicked)
    (let [[mX mY] (canvas/pt-in-canvas ctx @mouse)]
      (doseq [obj objs]
        (when (and (or (:on-right-click obj) (:on-click obj)) (satisfies? canvas/Hitable obj) (.isPointInPath (:ctx ctx) (canvas/hit-box obj) mX mY))
          (when (and @mouse-clicked (:on-click obj)) ((:on-click obj)))
          (when (and @mouse-right-clicked (:on-right-click obj)) ((:on-right-click obj))))))))

(defn update! []
  (let [objs (concat (squares @game-state))]
    (update-clicked objs)
    (reset! mouse-clicked false)
    (reset! mouse-right-clicked false)
    (-> objs
        (update-hover)
        (draw!))
    (.requestAnimationFrame js/window update!)))

(defn update-mouse [event]
  (reset! mouse [(.-clientX event) (.-clientY event)]))

(defn reload! []
  (.requestAnimationFrame js/window update!))

(defn main! []
  (.addEventListener (:canvas ctx) "contextmenu" (prevent-default click-mouse))
  (.addEventListener (:canvas ctx) "click" (prevent-default click-mouse))
  (.addEventListener (:canvas ctx) "mousemove" (prevent-default update-mouse))
  (oset! (:ctx ctx) "font" (str font-size "pt -apple-system"))
  (.requestAnimationFrame js/window update!))

;; CONSTANTS


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
;
;(defn reset-game []
;  (reset! state (apply game/new-game (@selected-difficulty difficulties))))
;

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

