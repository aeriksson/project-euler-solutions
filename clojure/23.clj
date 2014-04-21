;; A perfect number is a number for which the sum of its proper divisors is
;; exactly equal to the number. For example, the sum of the proper divisors of
;; 28 would be 1 + 2 + 4 + 7 + 14 = 28, which means that 28 is a perfect number.
;;
;; A number n is called deficient if the sum of its proper divisors is less than
;; n and it is called abundant if this sum exceeds n.
;;
;; As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the smallest
;; number that can be written as the sum of two abundant numbers is 24. By
;; mathematical analysis, it can be shown that all integers greater than 28123
;; can be written as the sum of two abundant numbers. However, this upper limit
;; cannot be reduced any further by analysis even though it is known that the
;; greatest number that cannot be expressed as the sum of two abundant numbers
;; is less than this limit.
;;
;; Find the sum of all the positive integers which cannot be written as the sum
;; of two abundant numbers.

(load "util")

(defn abundant? [n]
  (< n (proper-divisor-sum n)))

(defn euler-23 [n]
  (let [candidates (range 1 n)
        n-candidates (count candidates)
        is-abundant (vec (map abundant? candidates))
        abundants (vec (keep-indexed #(when %2 (inc %1)) is-abundant))
        n-abundants (count abundants)
        abundant-sums (loop [is-sum (transient (vec (repeat n-candidates true)))
                             i 0
                             j 0]
                        (if (= i n-abundants)
                          (persistent! is-sum)
                          (let [sum (+ (nth abundants i) (nth abundants j))
                                switch (or (= n-candidates j)
                                           (>= sum n-candidates))
                                new-i (if switch (inc i) i)
                                new-j (if switch new-i (inc j))
                                in-range (<= sum n-candidates)
                                new-is-sum (if in-range
                                             (assoc! is-sum (dec sum) false)
                                             is-sum)]
                            (recur new-is-sum new-i new-j))))]
    (reduce + (filter #(nth abundant-sums (dec %)) candidates))))

(run (euler-23 28123))
