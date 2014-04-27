;; The series, 1^1 + 2^2 + 3^3 + ... + 10^10 = 10405071317.
;;
;; Find the last ten digits of the series, 1^1 + 2^2 + 3^3 + ... + 1000^1000.

(load "util")

(defn euler-48 [n m]
  (let [xs (map #(exp % %) (range (inc n)))
        sum (reduce +' xs)]
    (reduce str (take-last m (digits-of sum)))))

(run (euler-48 1000 10))
