(ns minesweeper.game)

(defn toggle [coll e]
  "Returns `coll` without `e` if it was a member, with `e` if it wasn't."
  (if (contains? coll e) (disj coll e) (conj coll e)))

(defn positions [width height]
  (apply sorted-set (for [row (range height)
                          col (range width)]
                      [row col])))

(defn new-bombs [grid bomb-count]
  (set (take bomb-count (shuffle grid))))

(defn add-coords [& coords]
  (vec (apply map + coords)))

(def neighbor-deltas [[-1 -1] [-1 0] [-1 1] [0 1] [0 -1] [1 1] [1 0] [1 -1]])
(defn neighbors [grid pos]
  (filter (set grid) (map (partial add-coords pos) neighbor-deltas)))

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

(defn win? [{:keys [revealed bombs flagged width height]}]
  (let [total-count (* width height)
        revealed-count (count revealed)]
    (and (= total-count (+ revealed-count (count flagged)))
         (= flagged bombs))))

(defn lose? [{:keys [revealed bombs]}]
  (boolean (some revealed bombs)))

(defn update-state [game]
  (let [state' (cond
                 (win? game) :win
                 (lose? game) :lose
                 :else :active)]
    (merge game
           {:state state'}
           (when (not= state' :active) {:end-time (js/Date.)}))))

(defn flags-remaining [{:keys [flagged bombs]}]
  (- (count bombs) (count flagged)))

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
    (if (and (> (get counts pos) 0) (not (bombs pos)))
      game'
      (reduce reveal-from game' (filter #(not (revealed %)) neighbors)))))

(defn reveal [{:keys [revealed flagged counts bombs] :as game} pos]
  (if (or (revealed pos) (flagged pos))
    game
    (update-state (let [game' (reveal-tile game pos)]
                    (if-not (and (zero? (get counts pos)) (not (bombs pos)))
                      game'
                      (reveal-from game' pos))))))

(defn action-from-context [{:keys [revealed counts flagged] :as game} pos]
  (if-not (revealed pos)
    (toggle-flag game pos)
    (let [neighbors (neighbors (keys counts) pos)
          flagged-neighbors (filter flagged neighbors)]
      (if (>= (count flagged-neighbors) (get counts pos))
        (reduce reveal game neighbors)
        game))))

(defn take-action [game action pos]
  (case action
    :flag (toggle-flag game pos)
    :reveal (reveal game pos)
    :context (action-from-context game pos)))

