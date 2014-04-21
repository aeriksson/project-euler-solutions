;; 145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.
;;
;; Find the sum of all numbers which are equal to the sum of the factorial of
;; their digits.
;;
;; Note: as 1! = 1 and 2! = 2 are not sums they are not included.

(load "util")

(defn euler-34 []
  (reduce +
          (let [facs (vec (map factorial (range 10)))
                upper-bound (* 7 (factorial 9))]
            (for [i (range 2 upper-bound)
                  :when (= i (apply + (map #(nth facs %) (digits-of i))))]
              i))))

(run (euler-34))
