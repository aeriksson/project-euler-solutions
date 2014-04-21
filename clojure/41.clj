;; We shall say that an n-digit number is pandigital if it makes use of all the
;; digits 1 to n exactly once. For example, 2143 is a 4-digit pandigital and is
;; also prime.
;;
;; What is the largest n-digit pandigital prime that exists?

(load "util")

(defn euler-41 [digits]
  (first
    (for [ds (reverse (reductions str digits))
          :let [digit-sum (reduce #(+ %1 (Integer. (str %2))) 0 ds)]
          :when (not (divides? 3 digit-sum))]
      (let [lower-bound (exp 10 (dec (count ds)))
            upper-bound (* 10 lower-bound)
            ps (reverse (take-while (partial > upper-bound)
                                    (drop-while (partial > lower-bound)
                                                primes)))]
        (first (filter #(is-permutation? ds (str %)) ps))))))

(run (euler-41 "123456789"))
