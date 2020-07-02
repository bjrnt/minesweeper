(ns minesweeper.canvas
  (:require [oops.core :refer [oget oset!]]))

(defonce pixel-ratio (oget js/window "devicePixelRatio"))

(defn rescale! [{:keys [canvas ctx] :as obj}]
  (let [rect (.getBoundingClientRect canvas)
        width (oget rect "width")
        height (oget rect "height")
        width' (* width pixel-ratio)
        height' (* height pixel-ratio)]
    (doto ctx
      (oset! "canvas.width" width')
      (oset! "canvas.height" height')
      (oset! "canvas.style.width" (str width "px"))
      (oset! "canvas.style.height" (str height "px"))
      (.scale pixel-ratio pixel-ratio))
    (merge obj {:width width' :height height'})))

(defn set-defaults [ctx]
  (doto ctx
    (oset! "textBaseline" "hanging")
    (oset! "fillStyle" "black")))

(defn canvas-context-from-id [id]
  (let [canvas (.getElementById js/document id)
        ctx (.getContext canvas "2d")]
    {:canvas canvas
     :width  (oget canvas "width")
     :height (oget canvas "height")
     :ctx    (set-defaults ctx)}))

(defn clear [{:keys [ctx width height]}]
  (doto ctx
    (.save)
    (.setTransform 1 0 0 1 0 0)
    (.clearRect 0 0 width height)
    (.restore)))

(defn to-color [rgbas]
  (let [csv (apply str (interpose "," rgbas))]
    (str "rgba(" csv ")")))

(defprotocol Drawable
  (draw [this ctx] "Draw the object on the canvas."))

(defprotocol Hitable
  (hit-box [this] "Returns the Path2D hit box for this object."))

(defn draw-scene [ctx objs]
  (doseq [obj objs]
    (if (seq? obj)
      (draw-scene ctx obj)
      (do
        ;(println "Drawing:" obj)
        (draw obj ctx)))))

(defrecord Text [text pos color]
  Drawable
  (draw [this {ctx :ctx}]
    (let [[x y] pos]
      (doto ctx
        (oset! "fillStyle" (to-color (:color this)))
        (.fillText (:text this) x y)
        (.fill)))))

(defn add-pts [& coords]
  (vec (apply map + coords)))

(defn pt-in-canvas [{:keys [canvas width height]} [x y]]
  (let [rect (.getBoundingClientRect canvas)
        x-scale (/ width (.-width rect))
        y-scale (/ height (.-height rect))]
    [(* (- x (.-x rect)) x-scale) (* (- y (.-y rect)) y-scale)]))

(defrecord Rect [pos size color]
  Hitable
  (hit-box [this]
    (let [[x y] pos
          [width height] size
          path (js/Path2D.)]
      (if-let [stroke (:stroke this)]
        (.rect path (+ x stroke) (+ y stroke) (- width stroke stroke) (- height stroke stroke))
        (.rect path x y width height))
      path))

  Drawable
  (draw [this {ctx :ctx}]
    (let [[x y] pos
          [width height] size
          stroke (or (:stroke this) 0)
          color (if (and (:hover this) (:hovered this)) (:hover-color this) (:color this))]
      (when (:stroke this)
        (doto ctx
          (oset! "fillStyle" (to-color (:stroke-color this)))
          (.fillRect x y width height)
          ))
      (doto ctx
        (oset! "fillStyle" (to-color color))
        (.fillRect (+ x stroke) (+ y stroke) (- width stroke stroke) (- height stroke stroke)))
      )))
