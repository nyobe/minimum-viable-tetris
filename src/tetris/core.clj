(ns tetris.core
  (:require [clojure.core.async :as async :refer [go go-loop chan <! <!! >! >!! alts! alts!! timeout close!]])
  (:import [java.awt Frame Dimension Color]
           [java.awt.event KeyEvent KeyAdapter WindowAdapter]))

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

(defn mat-width [m]
  (count (first m)))

(defn mat-height [m]
  (count m))

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

(defn intersects-v? [v1 v2]
  (map #(and %1 %2) v1 v2))

(defn intersects-m? [m1 m2]
  (not-every? nil? (mapcat intersects-v? m1 m2)))

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

(defn in-bounds? [board piece [row col]]
  (let [w (+ col (mat-width piece))
        h (+ row (mat-height piece))]
    (and (< -1 col w (mat-width board))
         (< -1 row h (mat-height board)))))

(defn can-move? [board piece [row col]]
  "check if piece positioned at [row col] will intersect
  or go out of bounds"
  (when (in-bounds? board piece [row col])
    (let [sub (submat board [row col] [(mat-width piece) (mat-height piece)])]
      (not (intersects-m? sub piece)))))


;; Graphics

(defn main-window! []
  (doto (Frame.)
    ;; Let this window actually close X_X
    (.addWindowListener
      (proxy
        [WindowAdapter] []
        (windowClosing [e]
          (.dispose (.getSource e)))))

    (.setSize (Dimension. 200 200))
    (.setVisible true)))

(defn graphics [frame]
  (doto (.getGraphics frame)
    (.translate 0 (.. frame getInsets -top))))

(defn draw-square! [gfx color size [x y]]
  (let [arc (/ size 3)]
    (doto gfx
      (.setColor color)
      (.fillRoundRect x y size size arc arc)
      (.setColor (.darker color))
      (.drawRoundRect x y size size arc arc))))

(defn draw-board! [gfx board]
  (.clearRect gfx 0 0 200 200)
  (let [filled (filter #(get-in board %) (coords board))
        size 20]
    (doseq [[r c :as coord] filled]
      (draw-square! gfx
                    (colors (get-in board coord))
                    size [(* c size) (* r size)]))))


;; Core.Async Channels

(defn poll! [c]
  "read from channel without blocking"
  (first (alts!! [c] :default nil)))

(defn interval [msecs]
  "channel that ticks every msecs ms"
  (let [c (chan)]
    (go (while (>! c :tick)
          (<! (timeout msecs))))
    c))

(defn do-repeatedly [f]
  (let [running? (interval 250)]
    (go (while (<! running?)
          (f)))
    running?))


;; Input

(defn attach-key-listener! [frame keytable]
  (let [input (async/chan (async/sliding-buffer 1))]
    (doto frame
      (.addKeyListener
        (proxy
          [KeyAdapter] []
          (keyPressed [e]
            (when-let [k (keytable (.getKeyCode e))]
              (>!! input k))))))
    input))


;; Game state

(defn initial-state []
  {:board (empty-board 10 10)
   :piece nil
   :pos [0 0]
   :score 0})

(defn spawn-piece [{:keys [board] :as state}]
  (let [next-piece (rand-nth pieces)
        start-pos [0 (- (/ (mat-width board) 2)
                        (/ (mat-width next-piece) 2))]]
    (assoc state
           :piece next-piece
           :pos start-pos)))

(defn fuse-piece [{:keys [board piece pos] :as state}]
  (spawn-piece
    (assoc state :board (mask-m board piece pos))))

(defn move-piece [{:keys [board piece pos] :as state}
                  offset]
  (let [next-pos (map + pos offset)]
    (when (can-move? board piece pos)
      (assoc state :pos next-pos))))

(defn drop-piece [{:keys [board piece pos] :as state}]
  (let [downward (for [down (range)]
                   (move-piece state [down 0]))]
    (fuse-piece (last (take-while some? downward)))))

(defn rotate-piece [{:keys [board piece pos] :as state}]
  (let [rotated (rotate-ccw piece)]
    (when (can-move? board rotated pos)
      (assoc state :piece rotated))))

(defn game-loop [frame]
  (let [gfx       (graphics frame)
        tick-chan (interval 1000)
        move-chan (attach-key-listener!
                    frame {KeyEvent/VK_LEFT [0 -1]
                           KeyEvent/VK_RIGHT [0 1]})
        rot-chan  (attach-key-listener!
                    frame {KeyEvent/VK_UP :rotate})
        drop-chan (attach-key-listener!
                    frame {KeyEvent/VK_DOWN :drop})]
    (go-loop [{:keys [board piece pos] :as state} (spawn-piece (initial-state))]
      (let [[value ch] (alts! [tick-chan move-chan rot-chan drop-chan])]
        (draw-board! gfx (mask-m board piece pos))
        (recur
          (or (condp = ch
                move-chan (move-piece state value)
                rot-chan  (rotate-piece state)
                drop-chan (drop-piece state)
                tick-chan (or (move-piece state [1 0])
                              (fuse-piece state)))
              state))))))


(defn -main [& args]
  (let [frame (main-window!)]
    (game-loop frame)))

;; (def running (-main))
;; (close! running)

