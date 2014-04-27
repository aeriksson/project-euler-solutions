;; In England the currency is made up of pound, £, and pence, p, and there are
;; eight coins in general circulation:
;;
;; 1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p).
;; It is possible to make £2 in the following way:
;;
;; 1x£1 + 1x50p + 2x20p + 1x5p + 1x2p + 3x1p
;; How many different ways can £2 be made using any number of coins?

(load "util")

(defn euler-31 [coins n]
  (loop [v (vec (cons 1 (repeat n 0)))
         i (first coins)
         [c & cs :as coins] coins]
    (let [ways (+' (nth v i) (nth v (- i c)))
          v' (assoc v i ways)]
      (cond (> n i)        (recur v' (inc i) coins)
            (not-empty cs) (recur v' (first cs) cs)
            :else          ways))))

(run (euler-31 [1 2 5 10 20 50 100 200] 200))
