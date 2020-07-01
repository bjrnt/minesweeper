(ns minesweeper.game)

;; UTILITIES

(defn toggle [coll e]
  "Returns `coll` without `e` if it was a member, with `e` if it wasn't."
  (if (contains? coll e) (disj coll e) (conj coll e)))

;; 2D GRID

(defn positions [width height]
  "Returns a sorted set of all coordinates from [0 0] to [height-1 width-1]."
  (apply sorted-set (for [row (range height)
                          col (range width)]
                      [row col])))

(defn add-coords [& coords]
  (vec (apply map + coords)))

(def neighbor-deltas [[-1 -1] [-1 0] [-1 1] [0 1] [0 -1] [1 1] [1 0] [1 -1]])
(defn neighbors [grid pos]
  (filter (set grid) (map (partial add-coords pos) neighbor-deltas)))

;; STATE INITIALIZATION

(defn new-bombs [grid bomb-count]
  (set (take bomb-count (shuffle grid))))

(defn inc-neighbors [grid counts pos]
  (let [neighbors (neighbors grid pos)]
    (reduce #(update-in %1 [%2] inc) counts neighbors)))

(defn new-counts [grid bombs]
  (let [counts (apply hash-map (interleave grid (repeat 0)))]
    (reduce (partial inc-neighbors grid) counts bombs)))

(defn new-game [width height bomb-count]
  (let [grid (positions width height)
        bombs (new-bombs grid bomb-count)
        counts (new-counts grid bombs)]
    {:width      width
     :height     height
     :state      :active
     :bombs      bombs
     :flagged    #{}
     :revealed   #{}
     :counts     counts
     :start-time (js/Date.)}))

;; STATE UTILITIES

(defn win? [{:keys [revealed bombs flagged width height]}]
  (let [total-count (* width height)
        revealed-count (count revealed)]
    (and (= total-count (+ revealed-count (count flagged)))
         (= flagged bombs))))

(defn lose? [{:keys [revealed bombs]}]
  (boolean (some revealed bombs)))

(defn flags-remaining [{:keys [flagged bombs]}]
  (- (count bombs) (count flagged)))

;; MUTATIONS

(defn update-state [game]
  "Update the `game`'s `:state` based on the current conditions of the grid. Adds an `:end-time` if the `game` has finished."
  (let [state' (cond
                 (win? game) :win
                 (lose? game) :lose
                 :else :active)]
    (merge game
           {:state state'}
           (when (not= state' :active) {:end-time (js/Date.)}))))

(defn toggle-flag [{:keys [revealed] :as game} pos]
  (if (revealed pos)
    game
    (update-state (update-in game [:flagged] toggle pos))))

(defn reveal-tile [game pos]
  (-> game
      (update-in [:revealed] conj pos)
      (update-in [:flagged] disj pos)))

(defn reveal-from [{:keys [revealed counts bombs] :as game} pos]
  (let [neighbors (neighbors (keys counts) pos)
        game' (reveal-tile game pos)]
    (if (and (> (counts pos) 0) (not (bombs pos)))
      game'
      (reduce reveal-from game' (filter #(not (revealed %)) neighbors)))))

(defn reveal [{:keys [revealed flagged counts bombs] :as game} pos]
  (if (or (revealed pos) (flagged pos))
    game
    (update-state (let [game' (reveal-tile game pos)]
                    (if (and (zero? (counts pos)) (not (bombs pos)))
                      (reveal-from game' pos)
                      game')))))

(defn action-from-context [{:keys [revealed counts flagged] :as game} pos]
  (if-not (revealed pos)
    (toggle-flag game pos)
    (let [neighbors (neighbors (keys counts) pos)
          flagged-neighbors (filter flagged neighbors)]
      (if (>= (count flagged-neighbors) (counts pos))
        (reduce reveal game neighbors)
        game))))

(defn take-action [game action pos]
  (case action
    :flag (toggle-flag game pos)
    :reveal (reveal game pos)
    :context (action-from-context game pos)))
