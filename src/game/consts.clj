(ns game.consts)

(def WIN_VALUE 2048)
(def SIZE 4)
(def BOX_WIDTH 6)

(def TEST_BOARD [[2 4 8 16]
                 [32 64 128 256]
                 [512 8 16 4096]
                 [32 32 0 0]])

(def ZERO_BOARD (vec (repeat SIZE (vec (repeat SIZE 0)))))

