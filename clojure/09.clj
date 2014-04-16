;; A Pythagorean triplet is a set of three natural numbers, a < b < c, for which
;;
;; a^2 + b&2 = c^2
;; For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.
;;
;; There exists exactly one Pythagorean triplet for which a + b + c = 1000.
;; Find the product abc.

(load "util")

(defn euler-9 [n]
  (first
    (for [a (range 1 n)
          b (range a n)
          :let [c (- n a b)]
          :when (= (+ (* a a) (* b b)) (* c c))]
      (* a b c))))

(run (euler-9 1000))
