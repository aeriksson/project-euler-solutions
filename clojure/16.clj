;; 2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.
;;
;; What is the sum of the digits of the number 2^1000?

(load "util")

(defn euler-16 [n m]
  (sum (digits-of (exp n m))))

(run (euler-16 2 1000))
