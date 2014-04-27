;; The sum of the squares of the first ten natural numbers is
;; 1^2 + 2^2 + ... + 10^2 = 385
;;
;; The square of the sum of the first ten natural numbers is
;;
;; (1 + 2 + ... + 10)^2 = 55^2 = 3025
;; Hence the difference between the sum of the squares of the first ten natural
;; numbers and the square of the sum is 3025 − 385 = 2640.
;;
;; Find the difference between the sum of the squares of the first one hundred
;; natural numbers and the square of the sum.

(load "util")

(defn sum-of-squares [numbers]
  (sum (map #(* % %) numbers)))

(defn square-of-sum [numbers]
  (#(* % %) (sum numbers)))

(defn euler-6 [numbers]
  (- (square-of-sum numbers) (sum-of-squares numbers)))

(run (euler-6 (range 1 101)))
