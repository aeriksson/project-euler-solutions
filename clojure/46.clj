;; It was proposed by Christian Goldbach that every odd composite number can be
;; written as the sum of a prime and twice a square.
;;
;; 9 = 7 + 2×12
;; 15 = 7 + 2×22
;; 21 = 3 + 2×32
;; 25 = 7 + 2×32
;; 27 = 19 + 2×22
;; 33 = 31 + 2×12
;;
;; It turns out that the conjecture was false.
;;
;; What is the smallest odd composite that cannot be written as the sum of a
;; prime and twice a square?

(load "util")

(defn counterexample? [n]
  (let [double-squares (map #(* 2 % %) (range))]
    (not-any? #(prime? (- n %)) (take-while #(< % n) double-squares))))

(defn euler-46 [n]
  (let [odds (iterate (partial + 2) 3)
        odd-composites (remove prime? odds)
        counterexamples (filter counterexample? odd-composites)]
    (nth counterexamples (dec n))))

(run (euler-46 3))
