;; The decimal number, 585 = 1001001001b (binary), is palindromic in both bases.
;;
;; Find the sum of all numbers, less than one million, which are palindromic in
;; base 10 and base 2.
;;
;; (Please note that the palindromic number, in either base, may not include
;; leading zeros.)

(load "util")

(defn euler-36 [n]
  (sum (filter #(and (palindrome? (digits-of %))
                     (palindrome? (digits-of % 2))) (range 1 n 2))))

(run (euler-36 1000000))
