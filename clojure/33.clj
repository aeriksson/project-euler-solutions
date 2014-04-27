;; The fraction 49/98 is a curious fraction, as an inexperienced mathematician
;; in attempting to simplify it may incorrectly believe that 49/98 = 4/8, which
;; is correct, is obtained by cancelling the 9s.
;;
;; We shall consider fractions like, 30/50 = 3/5, to be trivial examples.
;;
;; There are exactly four non-trivial examples of this type of fraction, less
;; than one in value, and containing two digits in the numerator and denominator.
;;
;; If the product of these four fractions is given in its lowest common terms,
;; find the value of the denominator.

(load "util")

(defn curious? [x y z]
  (= (/ x z) (/ (+ (* 10 x) y) (+ (* 10 y) z))))

(defn euler-33 []
  (denominator (product (for [x (range 1 10)
                              y (range 1 10)
                              z (remove #(= x %) (range 1 10))
                              :when (curious? x y z)]
                          (/ x z)))))

(run (euler-33))
