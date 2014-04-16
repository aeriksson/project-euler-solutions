;; A palindromic number reads the same both ways. The largest palindrome made from
;; the product of two 2-digit numbers is 9009 = 91 * 99.
;;
;; Find the largest palindrome made from the product of two 3-digit numbers.

(load "util")

(run (reduce max
             (for [i (range 100 1000)
                   j (range i 1000)
                   :let [p (* i j)]
                   :when (palindrome? (digits-of p))]
               p)))
