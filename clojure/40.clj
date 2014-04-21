;; An irrational decimal fraction is created by concatenating the positive
;; integers:
;;
;; 0.123456789101112131415161718192021...
;;
;; It can be seen that the 12th digit of the fractional part is 1.
;;
;; If dn represents the nth digit of the fractional part, find the value of the
;; following expression.
;;
;; d1 × d10 × d100 × d1000 × d10000 × d100000 × d1000000

(load "util")

(defn euler-40 [ds]
  (let [f (fn [dig-offset digs-per-num]
            (let [split (* 9 (exp 10 (dec digs-per-num)) digs-per-num)]
              (if (< split dig-offset)
                  (recur (- dig-offset split) (inc digs-per-num))
                (let [start-num (exp 10 (dec digs-per-num))
                      num-offset (quot dig-offset digs-per-num)
                      target-num (+ start-num num-offset)
                      target-dig-index (dec (rem dig-offset digs-per-num))
                      target-dig (nth (digits-of target-num) target-dig-index)]
                  target-dig))))]
    (reduce * (map #(if (< % 10) % (f % 1)) ds))))

(run (euler-40 [1 10 100 1000 10000 100000 1000000]))
