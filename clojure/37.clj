;; The number 3797 has an interesting property. Being prime itself, it is
;; possible to continuously remove digits from left to right, and remain prime
;; at each stage: 3797, 797, 97, and 7. Similarly we can work from right to
;; left: 3797, 379, 37, and 3.
;;
;; Find the sum of the only eleven primes that are both truncatable from left
;; to right and right to left.
;;
;; NOTE: 2, 3, 5, and 7 are not considered to be truncatable primes.

(load "util")

(defn euler-37 []
  (loop [ps [3 5 7], acc [], d 10]
    (if (== 11 (count acc))
      (sum acc)
      (let [new-ps (filter prime? (for [p ps, i (range 1 10)] (+ p (* d i))))
            new-acc (filter (fn [n]
                              (every? prime?
                                      (take-while (comp not zero?)
                                                  (iterate #(quot % 10) n))))
                            new-ps)]
        (recur new-ps (concat acc new-acc) (* 10 d))))))

(run (euler-37))
