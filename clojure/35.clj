;; The number, 197, is called a circular prime because all rotations of the
;; digits: 197, 971, and 719, are themselves prime.
;;
;; There are thirteen such primes below 100: 2, 3, 5, 7, 11, 13, 17, 31, 37, 71,
;; 73, 79, and 97.
;;
;; How many circular primes are there below one million?

(load "util")

(defn cycle-int [n]
  (let [d ((comp dec count str) n)]
    (+ (quot n 10) (* (exp 10 d) (rem n 10)))))

(defn derp [nums]
  (let [cycled (map cycle-int nums)
        filtered (set (filter (partial contains? nums) cycled))]
    filtered))

(defn euler-35 [n]
  (count
    (let [primes-by-digits (map set
                                (partition-by (comp count digits-of)
                                              (take-while (partial > n)
                                                          primes)))]
      (for [[d ps] (map-indexed vector primes-by-digits)
            circulars (nth (iterate derp ps) d)]
        circulars))))

(run (euler-35 1000000))
