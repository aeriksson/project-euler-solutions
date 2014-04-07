;; The prime factors of 13195 are 5, 7, 13 and 29.
;;
;; What is the largest prime factor of the number 600851475143 ?

(load "util")

(defn prime-factors-of [n]
  (filter (comp zero? (partial rem n))
          (take-while #(> n (* % %))
                      primes)))

(run (reduce max (prime-factors-of 600851475143)))
