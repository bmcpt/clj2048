(ns game.bot
  (:require [clojure.core.async :as a]
            [lanterna.terminal :as t]
            [game.consts :refer :all]
            [game.main :refer [TERM max_value lost render run_game]]))

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
            (if (not (or (>= (max_value board) WIN_VALUE)
                         (lost board)))
              (do
                (Thread/sleep 50)
                (a/>! input (rand-nth [:Up :Down :Left :Right])))
              (do
                (Thread/sleep 3000)
                (a/>! input :Restart)))
            (recur))))

      (a/<!! (run_game input output)))))