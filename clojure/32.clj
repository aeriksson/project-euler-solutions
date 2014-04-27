;; We shall say that an n-digit number is pandigital if it makes use of all the
;; digits 1 to n exactly once; for example, the 5-digit number, 15234, is 1
;; through 5 pandigital.
;;
;; The product 7254 is unusual, as the identity, 39 × 186 = 7254, containing
;; multiplicand, multiplier, and product is 1 through 9 pandigital.
;;
;; Find the sum of all products whose multiplicand/multiplier/product identity
;; can be written as a 1 through 9 pandigital.
;;
;; HINT: Some products can be obtained in more than one way so be sure to only
;; include it once in your sum.

(load "util")

(defn euler-32 [digits]
  "Generalizes the problem to arbitrary sets of digits. Resticts the search
  to regions that give the correct number of digits."
  (let [bounds (case (count digits)
                 10 [[(range 10 100) #(range (quot 10000 %) 1000)]
                     [(range 1 10) #(range (quot 10000 %) 10000)]]
                 9  [[(range 10 100) #(range 100 (quot 10000 %))]
                     [(range 1 10) #(range 1000 (quot 10000 %))]]
                 8  [[(range 10 100) #(range (quot 1000 %) 100)]
                     [(range 1 10) #(range (quot 1000 %) 1000)]]
                 7  [[(range 10 100) #(range 10 (quot 1000 %))]
                     [(range 1 10) #(range 100 (quot 1000 %))]]
                 6  [[(range 1 10) #(range (quot 100 %) 100)]]
                 5  [[(range 1 10) #(range 10 (quot 100 %))]]
                 4  [[(range 1 10) #(range (quot 10 %) 10)]]
                 3  [[(range 1 10) #(range 10 (quot 10 %))]]
                 :else [])]
    (reduce +
            (distinct
              (for [[as bs] bounds
                    a as
                    b (bs a)
                    :let [p (* a b)]
                    :when (is-permutation? (str a b p) digits)]
                p)))))

(run (euler-32 "123456789"))
