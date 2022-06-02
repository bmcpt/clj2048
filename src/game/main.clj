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
                y (+ 1 (* 2 r))]
          :when (not (zero? n))]
      (do
        (t/set-fg-color TERM (color_for n))
        (t/put-string TERM (.toString n) x y))))
  (cond
    (victory board) (do
                      (t/set-fg-color TERM :green)
                      (t/put-string TERM "You Won!" 1 (inc (* 2 SIZE))))
    (lost board) (do
                   (t/set-fg-color TERM :red)
                   (t/put-string TERM "You Lost!" 1 (inc (* 2 SIZE)))))
  (t/put-string TERM "" 0 (* 2 (inc SIZE))))

(declare run_game)

(defn -main []
  (binding [TERM (t/get-terminal :unix)]
    (t/start TERM)
    (let [input (a/chan 64)
          quit (fn []
                 (do
                   (t/stop TERM)
                   (a/close! input)))]
      (a/go-loop []
        (let [continue (try
                         (let [c (t/get-key-blocking TERM)]
                           (case c
                             :up (do
                                   (a/>! input :Up)
                                   true)
                             :down (do
                                     (a/>! input :Down)
                                     true)
                             :left (do
                                     (a/>! input :Left)
                                     true)
                             :right (do
                                      (a/>! input :Right)
                                      true)
                             \r (do
                                  (a/>! input :Restart)
                                  true)
                             \q (do
                                  (quit)
                                  false)
                             true))
                         (catch Exception _
                           (quit)
                           false))]
          (if continue
            (recur))))
      (a/<!! (run_game input)))))

(defn compress_nums [nums]
  (loop [nums nums]
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
                         (subvec nums (+ 2 pidx))))))))))

(defn spawn_num [board]
  (let [val (* 2 (inc (rand-int 2)))
        zeroes (filter
                 #(zero? (get-in board %))
                 (for [r (range SIZE)
                       c (range SIZE)]
                   [r c]))]
    (if (empty? zeroes)
      board
      (assoc-in board (rand-nth zeroes) val))))

(defn slide_right [board]
  (vec
    (for [r (range SIZE)
          :let [nums (vec (filter pos? (board r)))
                compressed (compress_nums nums)]]
      (vec
        (concat
          (repeat (- SIZE (count compressed)) 0)
          compressed)))))

(defn slide_left [board]
  (vec
    (for [r (range SIZE)
          :let [nums (vec (filter pos? (board r)))
                compressed (compress_nums nums)]]
      (vec
        (concat
          compressed
          (repeat (- SIZE (count compressed)) 0))))))

(defn run_game
  "Takes a channel as argument
   input is for sending instructions [:Up :Down :Left :Right]
   ends when input is closed
   must run in a context in which TERM is defined properly"
  [input]
  (a/go-loop [board ZERO_BOARD]
    (let [board (spawn_num board)]
      (render board)
      (let [instr (a/<! input)
            shifted_board (case instr
                            :Up board
                            :Down board
                            :Left (slide_left board)
                            :Right (slide_right board)
                            :Restart ZERO_BOARD
                            nil)]
        (if shifted_board
          (recur shifted_board))))))