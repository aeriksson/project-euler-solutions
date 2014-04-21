;; The number, 1406357289, is a 0 to 9 pandigital number because it is made up
;; of each of the digits 0 to 9 in some order, but it also has a rather
;; interesting sub-string divisibility property.
;;
;; Let d1 be the 1st digit, d2 be the 2nd digit, and so on. In this way, we note
;; the following:
;;
;; d2d3d4=406 is divisible by 2
;; d3d4d5=063 is divisible by 3
;; d4d5d6=635 is divisible by 5
;; d5d6d7=357 is divisible by 7
;; d6d7d8=572 is divisible by 11
;; d7d8d9=728 is divisible by 13
;; d8d9d10=289 is divisible by 17
;;
;; Find the sum of all 0 to 9 pandigital numbers with this property.

(load "util")

(defn euler-43 [digits divisors]
  (let [comp-digits #(remove (partial (set %)) digits)
        candidates (fn [divisor suffix digits]
                     (let [n (if (zero? (count suffix))
                               (filter #(= 3 (count (distinct (str %))))
                                       (range 100 1000))
                               digits)]
                     (filter #(divides? divisor (Integer. (str % suffix)))
                             (map str n))))
        f (fn f [digits [div & divs]]
            (let [ds (comp-digits digits)]
              (if (nil? div)
                (str (first ds) digits)
                (flatten
                  (for [c (candidates div (apply str (take 2 digits)) ds)]
                    (f (str c digits) divs))))))]
  (reduce #(+ %1 (read-string %2)) 0 (f "" (reverse divisors)))))

(run (euler-43 "0123456789" [2 3 5 7 11 13 17]))
