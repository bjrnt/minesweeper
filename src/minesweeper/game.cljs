(ns minesweeper.game)

(defn height [grid]
  (inc (first (last (keys grid)))))

(defn width [grid]
  (inc (second (last (keys grid)))))

(defn cell-type [pred]
  (fn [grid] (filter pred (vals grid))))

(def bombs (cell-type :bomb))
(def flagged (cell-type :flagged))

(defn in-bounds? [grid [y x]]
  (and (> (height grid) y -1)
       (> (width grid) x -1)))

(defn win? [grid]
  (let [[bombs non-bombs] ((juxt filter remove) :bomb (vals grid))]
    (and (every? :flagged bombs)
         (every? :revealed non-bombs))))

(defn lose? [grid] (some :revealed (bombs grid)))

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

(def neighbor-deltas (for [y (range -1 2) x (range -1 2)] [y x]))
(defn neighbor-pts [grid pt]
  (filter (partial in-bounds? grid) (map (partial add-points pt) neighbor-deltas)))
(defn neighbor-cells [grid pt]
  (map grid (neighbor-pts grid pt)))

(defn create-bomb-pts [pts bomb-count]
  (take bomb-count (shuffle pts)))

(defn inc-counts [grid pt]
  (reduce #(update-in %1 [%2 :count] inc) grid (neighbor-pts grid pt)))

(defn assoc-cell-prop [property value]
  (fn [grid pt] (assoc-in grid [pt property] value)))

(def unflag (assoc-cell-prop :flagged false))
(defn reveal [grid pt]
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

(defn auto-reveal [grid pt]
  (if (auto-revealable? (get grid pt))
    ;; TODO: may have perf problems in large grids
    (if (auto-continue? (get grid pt))
      (reduce auto-reveal (reveal grid pt) (neighbor-pts grid pt))
      (reveal grid pt))
    (grid)))

(defn reveal-neighbors [grid pt]
  "Reveal all neighbors of the given point."
  (let [non-flagged (filter #(not (:flagged (get grid %))) (neighbor-pts grid pt))]
    (reduce reveal grid non-flagged)))

(defn flag-count-ok? [grid pt]
  "Returns true if `pt` in `grid` has at least as many flags around it as its count."
  (let [want (get-in grid [pt :count])
        got (count (filter :flagged (neighbor-cells grid pt)))]
    (>= got want)))

;; STATS

(defn flags-remaining [grid]
  (let [bombs (count (bombs grid))
        flags (count (flagged grid))]
    (max 0 (- bombs flags))))

;; ACTIONS

(defn action-toggle-flagged [grid pt]
  (update-in grid [pt :flagged] not))

(defn action-reveal [grid pt]
  "Start revealing from the given point. May reveal only the given point, or more if its count is zero."
  (if (auto-continue? (get grid pt))
    (auto-reveal grid pt)
    (reveal grid pt)))

(defn action-from-context [grid pt]
  "Take action depending on which point was selected. If it is an unrevealed tile, toggle it's flagged state. If it is a revealed tile, reveal all its neighbors."
  (if (:revealed (get grid pt))
    (if (flag-count-ok? grid pt) (reveal-neighbors grid pt) grid)
    (action-toggle-flagged grid pt)))

(defn action-reset [width height bomb-count]
  (let [pts (create-pts width height)]
    (add-bombs (create-grid pts) (create-bomb-pts pts bomb-count))))

;(defn print-cell [[[_ x] cell]]
;  (let [symbol (if (:bomb cell) "B" (:count cell))
;        linebreak (if (= x (dec width)) "\n" "")
;        revealed (if (:revealed cell) "'" " ")]
;    (print (str symbol revealed linebreak))))

;(defn print-grid [grid]
;  (map print-cell (into (sorted-map) grid)))
