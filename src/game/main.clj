(ns game.main
  (:require [clojure.core.async :as a]
            [lanterna.terminal :as t]
            [game.consts :refer :all]))

(def ^:dynamic TERM)

(defn victory [board]
  (some #(= % WIN_VALUE) (flatten board)))

(defn lost [board]
  (and
    (not (some zero? (flatten board)))
    (every? identity
            (for [r (range SIZE)
                  c (range (dec SIZE))]
              (not= (get-in board [r c])
                    (get-in board [r (inc c)]))))
    (every? identity
            (for [c (range SIZE)
                  r (range (dec SIZE))]
              (not= (get-in board [r c])
                    (get-in board [(inc r) c]))))))

(defn render [board]
  (t/clear TERM)
  (doall
    (for [r (range SIZE)
          c (range SIZE)
          :let [n (get-in board [r c])
                x (+ 1 (* BOX_WIDTH c))
                y (+ 1 (* 2 r))]]
      (t/put-string TERM (.toString n) x y)))
  (cond
    (victory board) (do
                      (t/set-fg-color TERM :green)
                      (t/put-string TERM "You Won!" 1 (inc (* 2 SIZE))))
    (lost board) (do
                   (t/set-fg-color TERM :red)
                   (t/put-string TERM "You Lost!" 1 (inc (* 2 SIZE)))))
  (t/put-string TERM "" 0 (* 2 (inc SIZE))))

(defn -main []
  (binding [TERM (t/get-terminal :unix)]
    (t/start TERM)
    (render TEST_BOARD)))

(defn init_board []
  (let [r (rand-int 4)
        c (rand-int 4)
        val (* 2 (inc (rand-int 2)))]
    (assoc-in ZERO_BOARD [r c] val)))

(defn slide_right [board]
  (vec
    (for [r (range SIZE)
          :let [nums (vec (filter pos? (board r)))
                nums_compressed (loop [nums nums]
                                  (let [pair_indexes (filter
                                                       (fn [idx]
                                                         (= (nums idx)
                                                            (nums (inc idx))))
                                                       (range (dec (count nums))))]
                                    (if (empty? pair_indexes)
                                      nums
                                      (let [pidx (first pair_indexes)]
                                        (recur
                                          (vec (concat (subvec nums 0 pidx)
                                                       [(* 2 (nums pidx))]
                                                       (subvec nums (+ 2 pidx)))))))))]]
      (vec
        (concat
          (repeat (- SIZE (count nums_compressed)) 0)
          nums_compressed)))))

(defn run-game
  "Takes a channel as argument
   input is for sending instructions [:Up :Down :Left :Right]
   ends when input is closed
   must run in a context in which TERM is defined properly"
  [input]
  (a/go-loop [board (init_board)]
    (render board)
    (let [instr (a/<! input)
          new_board (case instr
                      :Up board
                      :Down board
                      :Left board
                      :Right (slide_right board)
                      nil)]
      (if new_board
        (recur new_board)))))