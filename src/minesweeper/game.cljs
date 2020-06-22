(ns minesweeper.game)

(def width 5)
(def height 5)
(def bomb-count 1)

(def neighbor-deltas '[[-1 -1]                              ;; NW
                       [-1 0]                               ;; N
                       [0 -1]                               ;; W
                       [1 1]                                ;; SE
                       [1 0]                                ;; S
                       [0 1]                                ;; E
                       [-1 1]                               ;; NE
                       [1 -1]])                             ;; SW

(defn in-bounds? [[y x]]
  (and (> height y -1)
       (> width x -1)))

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

(defn create-pts []
  (for [row (range height)
        col (range width)]
    [row col]))

(defn create-grid []
  (apply hash-map (interleave (create-pts) (repeat empty-cell))))

(defn add-points [& pts]
  (vec (apply map + pts)))

(defn neighbor-pts [pt]
  (filter in-bounds? (map (partial add-points pt) neighbor-deltas)))

(defn create-bomb-pts []
  (take bomb-count (shuffle (create-pts))))

(defn inc-counts [grid pt]
  (reduce #(update-in %1 [%2 :count] inc) grid (neighbor-pts pt)))

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
      (reduce reveal-from (set-revealed grid pt) (neighbor-pts pt))
      (set-revealed grid pt))
    (grid)))

(defn reveal [grid pt]
  (if (auto-continue? (get grid pt))
    (reveal-from grid pt)
    (set-revealed grid pt)))

(defn toggle-flagged [grid pt]
  (update-in grid [pt :flagged] not))

(defn reset []
  (add-bombs (create-grid) (create-bomb-pts)))

;(defn print-cell [[[_ x] cell]]
;  (let [symbol (if (:bomb cell) "B" (:count cell))
;        linebreak (if (= x (dec width)) "\n" "")
;        revealed (if (:revealed cell) "'" " ")]
;    (print (str symbol revealed linebreak))))

;(defn print-grid [grid]
;  (map print-cell (into (sorted-map) grid)))
