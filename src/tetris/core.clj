(ns tetris.core)

(defn print-m [m]
  (dorun (map println m)))

(defn empty-board [dimr dimc]
  (let [empty-row (vec (take dimc (repeat nil)))]
    (vec (take dimr (repeat empty-row)))))

(def board (atom (empty-board 10 10)))

