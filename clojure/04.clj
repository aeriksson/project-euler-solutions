;; A palindromic number reads the same both ways. The largest palindrome made from
;; the product of two 2-digit numbers is 9009 = 91 * 99.
;;
;; Find the largest palindrome made from the product of two 3-digit numbers.

(load "util")

(defn euler-4 [n]
  (let [upper-bound (exp 10 n)
        lower-bound (exp 10 (dec n))]
    (reduce max
            (for [i (range lower-bound upper-bound)
                  j (range i upper-bound)
                  :let [p (* i j)]
                  :when (palindrome? (digits-of p))]
              p))))

(run (euler-4 3))
