(ns game.main
  (:require [clojure.core.async :as a]
            [lanterna.terminal :as t]
            [game.consts :refer :all]))

(def ^:dynamic TERM)

(defn max_value [board]
  (reduce max (flatten board)))

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
  (t/set-fg-color TERM :default)

  (let [max_c (* BOX_WIDTH SIZE)
        max_r (* 2 SIZE)]
    (doall
      (for [c (range 1 max_c)]
        (t/put-character TERM \- c 0)))
    (doall
      (for [c (range 1 max_c)]
        (t/put-character TERM \- c max_r)))
    (doall
      (for [r (range 1 max_r)]
        (t/put-character TERM \| 0 r)))
    (doall
      (for [r (range 1 max_r)]
        (t/put-character TERM \| max_c r))))

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
    (>= (max_value board) WIN_VALUE) (do
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
          output (a/chan 64)
          quit (fn []
                 (do
                   (t/stop TERM)
                   (a/close! input)
                   (a/close! output)))]

      ; Rendering event loop
      (a/go-loop []
        (let [board (a/<! output)]
          (when board
            (render board)
            (recur))))

      ; Input event loop
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
      (a/<!! (run_game input output)))))

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

(defn flip [board]
  (vec
    (for [r (range SIZE)]
      (vec
        (for [c (range SIZE)]
          (get-in board [c r]))))))

(defn slide_down [board]
  (-> board
      flip
      slide_right
      flip))

(defn slide_up [board]
  (-> board
      flip
      slide_left
      flip))

(defn run_game
  "Takes two channel as arguments

   output receives board states
   input is for sending instructions [:Up :Down :Left :Right :Restart]

   ends when input is closed"
  [input output]
  (a/go-loop [board ZERO_BOARD]
    (let [board (spawn_num board)]
      (a/>! output board)
      (let [instr (a/<! input)
            shifted_board (case instr
                            :Up (slide_up board)
                            :Down (slide_down board)
                            :Left (slide_left board)
                            :Right (slide_right board)
                            :Restart ZERO_BOARD
                            nil)]
        (if shifted_board
          (recur shifted_board))))))