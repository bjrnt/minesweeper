(ns minesweeper.game)

(def neighbor-deltas (for [y (range -1 2) x (range -1 2)] [y x]))

(defn grid-height [grid]
  (inc (first (last (keys grid)))))

(defn grid-width [grid]
  (inc (second (last (keys grid)))))

(defn in-bounds? [grid [y x]]
  (and (> (grid-height grid) y -1)
       (> (grid-width grid) x -1)))

(defn win? [grid]
  (let [[bombs non-bombs] ((juxt filter remove) :bomb (vals grid))]
    (and (every? :flagged bombs)
         (every? :revealed non-bombs))))

(defn lose? [grid]
  (some :revealed (filter :bomb (vals grid))))

(defn active? [grid]
  "Returns true if the game hasn't ended yet."
  (not (or (win? grid) (lose? grid))))

(def empty-cell
  {:revealed false
   :bomb     false
   :count    0
   :flagged  false})

(defn create-pts [width height]
  (for [row (range height)
        col (range width)]
    [row col]))

(defn create-grid [pts]
  (apply sorted-map (interleave pts (repeat empty-cell))))

(defn add-points [& pts]
  (vec (apply map + pts)))

(defn neighbor-pts [grid pt]
  (filter (partial in-bounds? grid) (map (partial add-points pt) neighbor-deltas)))

(defn create-bomb-pts [pts bomb-count]
  (take bomb-count (shuffle pts)))

(defn inc-counts [grid pt]
  (reduce #(update-in %1 [%2 :count] inc) grid (neighbor-pts grid pt)))

(defn unflag [grid pt]
  (assoc-in grid [pt :flagged] false))

(defn set-revealed [grid pt]
  (-> grid
      (unflag pt)
      (assoc-in [pt :revealed] true)))

(defn add-bomb [grid pt]
  (-> grid
      (assoc-in [pt :bomb] true)
      (inc-counts pt)))

(defn add-bombs [grid bombs]
  (reduce add-bomb grid bombs))

(def auto-revealable? (complement :bomb))

(defn auto-continue? [{bomb :bomb, revealed :revealed, count :count}]
  (and (not bomb) (not revealed) (zero? count)))

(defn reveal-from [grid pt]
  (if (auto-revealable? (get grid pt))
    ;; TODO: may have perf problems in large grids
    (if (auto-continue? (get grid pt))
      (reduce reveal-from (set-revealed grid pt) (neighbor-pts grid pt))
      (set-revealed grid pt))
    (grid)))

(defn reveal [grid pt]
  (if (auto-continue? (get grid pt))
    (reveal-from grid pt)
    (set-revealed grid pt)))

(defn toggle-flagged [grid pt]
  (update-in grid [pt :flagged] not))

(defn reset [width height bomb-count]
  (let [pts (create-pts width height)]
    (add-bombs (create-grid pts) (create-bomb-pts pts bomb-count))))

;(defn print-cell [[[_ x] cell]]
;  (let [symbol (if (:bomb cell) "B" (:count cell))
;        linebreak (if (= x (dec width)) "\n" "")
;        revealed (if (:revealed cell) "'" " ")]
;    (print (str symbol revealed linebreak))))

;(defn print-grid [grid]
;  (map print-cell (into (sorted-map) grid)))
