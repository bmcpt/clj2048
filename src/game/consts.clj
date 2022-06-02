(ns game.consts)

(def WIN_VALUE 2048)
(def SIZE 4)
(def BOX_WIDTH 6)

(def TEST_BOARD [[2 4 8 16]
                 [32 64 128 256]
                 [512 1024 2048 4096]
                 [1024 2048 4096 8192]])

(def ZERO_BOARD (vec (repeat SIZE (vec (repeat SIZE 0)))))

