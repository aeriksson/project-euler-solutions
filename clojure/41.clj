;; We shall say that an n-digit number is pandigital if it makes use of all the
;; digits 1 to n exactly once. For example, 2143 is a 4-digit pandigital and is
;; also prime.
;;
;; What is the largest n-digit pandigital prime that exists?

(load "util")

(defn n-digit-primes [n]
  (take-while #(< % (exp 10 n))
              (drop-while #(< % (exp 10 (dec n))) primes)))

(defn primes-with-digits [digits]
  (let [primes (reverse (n-digit-primes (count digits)))]
    (first (filter #(is-permutation? digits (str %)) primes))))

(defn euler-41 [digits]
  (first
    (for [ds (reverse (reductions str digits))
          :let [digit-sum (reduce #(+ %1 (Integer. (str %2))) 0 ds)]
          :when (and (not (divides? 3 digit-sum))
                     (not (divides? 9 digit-sum)))]
      (primes-with-digits ds))))

(run (euler-41 "123456789"))
