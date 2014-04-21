;; The arithmetic sequence, 1487, 4817, 8147, in which each of the terms
;; increases by 3330, is unusual in two ways: (i) each of the three terms are
;; prime, and, (ii) each of the 4-digit numbers are permutations of one another.
;;
;; There are no arithmetic sequences made up of three 1-, 2-, or 3-digit primes,
;; exhibiting this property, but there is one other 4-digit increasing sequence.
;;
;; What 12-digit number do you form by concatenating the three terms in this
;; sequence?

(load "util")

(defn euler-49 [n-digits]
  (let [lower-bound (exp 10 (dec n-digits))
        upper-bound (* 10 lower-bound)
        ps (take-while (partial > upper-bound)
                       (drop-while (partial > lower-bound) primes))]
    (for [a ps
          b (take-while (partial > a) ps)
          :let [d (- a b)
                c (+ a d)]
          :when (and (> upper-bound c)
                     (is-permutation? (str a) (str b))
                     (is-permutation? (str a) (str c))
                     (prime? c))]
      (str b a c))))

(run (second (euler-49 4)))
