;; A permutation is an ordered arrangement of objects. For example, 3124 is one
;; possible permutation of the digits 1, 2, 3 and 4. If all of the permutations
;; are listed numerically or alphabetically, we call it lexicographic order.
;; The lexicographic permutations of 0, 1 and 2 are:
;;
;; 012   021   102   120   201   210
;;
;; What is the millionth lexicographic permutation of the digits 0, 1, 2, 3, 4,
;; 5, 6, 7, 8 and 9?

(load "util")

(defn nth-permutation [l n]
  (if (= n 1)
    l
    (let [len (count l)
          total-permutations (factorial (dec len))
          quotient (quot n total-permutations)
          remainder (rem n total-permutations)
          elem (nth l quotient)
          others (remove (partial = elem) l)]
      (cons elem (nth-permutation others remainder)))))

(defn euler-24 [l n]
  (read-string (apply str (nth-permutation l n))))

(run (euler-24 (range 10) (int (dec 1e6))))
