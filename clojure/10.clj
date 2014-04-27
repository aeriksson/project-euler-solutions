;; The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
;;
;; Find the sum of all the primes below two million.

(load "util")

(defn euler-10 [n]
  (sum (take-while (partial > n) primes)))

(run (euler-10 2e6))
