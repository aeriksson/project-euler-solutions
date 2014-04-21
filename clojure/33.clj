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

(defn curious-fraction? [n d]
  (let [n1 (quot n 10)
        n2 (rem n 10)
        d1 (quot d 10)
        d2 (rem d 10)]
    (and (not (zero? d2))
         (= n2 d1)
         (= (/ n1 d2) (/ n d)))))

(defn euler-33 []
  (denominator
    (reduce *
            (for [n (range 10 99)
                  d (range (inc n) 100)
                  :when (curious-fraction? n d)]
              (/ n d)))))

(run (euler-33))
