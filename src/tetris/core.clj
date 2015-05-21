(ns tetris.core)

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

