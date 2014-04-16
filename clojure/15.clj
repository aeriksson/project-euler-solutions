;; Starting in the top left corner of a 2x2 grid, and only being able to move
;; to the right and down, there are exactly 6 routes to the bottom right corner.
;;
;; How many such routes are there through a 20x20 grid?

(load "util")

(defn euler-15 [n m]
  (long (choose (+ n m) n)))

(run (euler-15 20 20))
