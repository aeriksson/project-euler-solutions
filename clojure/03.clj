;; The prime factors of 13195 are 5, 7, 13 and 29.
;;
;; What is the largest prime factor of the number 600851475143 ?

(load "util")

(defn euler-3 [n]
  (reduce max (prime-factors n)))

(run (euler-3 600851475143))
