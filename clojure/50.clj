;; The prime 41, can be written as the sum of six consecutive primes:
;;
;; 41 = 2 + 3 + 5 + 7 + 11 + 13
;; This is the longest sum of consecutive primes that adds to a prime below
;; one-hundred.
;;
;; The longest sum of consecutive primes below one-thousand that adds to a
;; prime, contains 21 terms, and is equal to 953.
;;
;; Which prime, below one-million, can be written as the sum of the most
;; consecutive primes?

(load "util")

(defn euler-50 [n]
  (let [sums (take-while (partial > n) (reductions + primes))
        n-sums (count sums)]
    (first
      (for [i (range 0 n-sums)
            parts (partition (- n-sums i) 1 sums)
            :let [d (- (last parts) (first parts))]
            :when (prime? d)]
        d))))

(run (euler-50 1000000))
