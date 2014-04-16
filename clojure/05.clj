;; 2520 is the smallest number that can be divided by each of the numbers from
;; 1 to 10 without any remainder.
;;
;; What is the smallest positive number that is evenly divisible by all of the
;; numbers from 1 to 20?

(load "util")

(defn euler-5 [numbers]
  (reduce least-common-multiple numbers))

(run (euler-5 (range 1 21)))
