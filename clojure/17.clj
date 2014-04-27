;; If the numbers 1 to 5 are written out in words: one, two, three, four, five,
;; then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.
;;
;; If all the numbers from 1 to 1000 (one thousand) inclusive were written out
;; in words, how many letters would be used?
;;
;;
;; NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and
;; forty-two) contains 23 letters and 115 (one hundred and fifteen) contains 20
;; letters. The use of "and" when writing out numbers is in compliance with
;; British usage.

(load "util")

(use '[clojure.pprint :only (cl-format)])

(defn euler-17 [numbers]
  (let [spelled-out (map (partial cl-format nil "~R") numbers)
        n-ands      (count (filter #(and (> % 100)
                                         (not (divides? 100 %))) numbers))]
    (+ (* 3 n-ands)
       (count (filter #(Character/isLetter %) (reduce str spelled-out))))))

(run (euler-17 (range 1 1001)))
