;; The first two consecutive numbers to have two distinct prime factors are:
;;
;; 14 = 2 × 7
;; 15 = 3 × 5
;;
;; The first three consecutive numbers to have three distinct prime factors are:
;;
;; 644 = 2^2 × 7 × 23
;; 645 = 3 × 5 × 43
;; 646 = 2 × 17 × 19.
;;
;; Find the first four consecutive integers to have four distinct prime factors.
;; What is the first of these numbers?

(load "util")

(defn euler-47 [n-consecutive n-distinct-factors]
  (first
    (for [is (partition n-consecutive
                        1
                        (map #(vector % (count (distinct (prime-factors %))))
                             (range)))
          :when (every? #(<= n-distinct-factors (second %)) is)]
      (first (first is)))))

(run (euler-47 4 4))
