(ns tetris.core
  (:import [java.awt Frame Dimension Color]))

(def pieces
  [[[:i]
    [:i]
    [:i]
    [:i]]

   [[nil :j]
    [nil :j]
    [:j  :j]]

   [[:l nil]
    [:l nil]
    [:l :l]]

   [[:o :o]
    [:o :o]]

   [[nil :s :s]
    [:s :s nil]]

   [[:t :t :t]
    [nil :t nil]]

   [[:z :z nil]
    [nil :z :z]]])

(def colors {:i Color/CYAN
             :j Color/BLUE
             :l Color/ORANGE
             :o Color/YELLOW
             :s Color/GREEN
             :t Color/MAGENTA
             :z Color/RED})


;; Matrix functions

(defn print-m [m]
  (dorun (map println m)))

(defn empty-board [dimr dimc]
  (let [empty-row (vec (take dimc (repeat nil)))]
    (vec (take dimr (repeat empty-row)))))

(def board (atom (empty-board 10 10)))

(defn transpose [m]
  (apply map vector m))

(defn rotate-ccw [m]
  (reverse (transpose m)))

(defn rotate-cw [m]
  (transpose (reverse m)))

(defn submat [mat [row col] [width height]]
  "extract region from matrix"
  (map #(subvec % col (+ col width))
       (subvec mat row (+ row height))))

(defn intersects-v [v1 v2]
  (map #(and %1 %2) v1 v2))

(defn intersects-m [m1 m2]
  (not-every? nil? (mapcat intersects-v m1 m2)))

(defn coords [m]
  (let [rows (count m)
        cols (count (first m))]
    (for [r (range rows)
          c (range cols)]
      [r c])))

(defn mask-v [base over pos]
  (let [end (+ pos (count over))]
    (vec (concat
           (subvec base 0 pos)
           (map #(or %1 %2) over (subvec base pos end))
           (subvec base end)))))

(defn mask-m [base over [row col]]
  "overlay non-nil values of matrix 'over' onto 'base' at offset [row col]"
  (let [end (+ row (count over))]
    (vec (concat
           (subvec base 0 row)
           (map mask-v
                (subvec base row end)
                over
                (repeat col))
           (subvec base end)))))


;; Graphics

(def frame
  (doto (Frame.)
    (.setSize (Dimension. 200 200))
    (.setVisible true)))

(def gfx
  (doto (.getGraphics frame)
    (.translate 0 (.. frame getInsets -top))))

(defn draw-square! [gfx color size [x y]]
  (doto gfx
    (.setColor color)
    (.fillRect x y size size)))

(defn draw-board! [gfx board]
  (let [filled (filter #(get-in board %) (coords board))
        size 20]
    (doseq [[r c :as coord] filled]
      (draw-square! gfx
                    (colors (get-in board coord))
                    size [(* c size) (* r size)]))))

;; (.repaint frame)
;; (draw-board! gfx (-> @board (mask-m T [3 3]) (mask-m Z [0 1]) (mask-m I [4 2])))

