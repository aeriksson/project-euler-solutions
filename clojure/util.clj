(defn sum [n]
  (reduce +' n))

(defn product [n]
  (reduce *' n))

(defn exp [x n]
  (product (repeat n x)))

(defn factorial [n]
  (product (range 2 (inc n))))

(defn choose [n k]
  (product (for [i (range 1 (inc k))]
             (/(- (inc n) i) i))))

(defn greatest-common-divisor [a b]
  (if (zero? b) a (recur b (mod a b))))

(defn coprime? [a b]
  (= 1 (greatest-common-divisor a b)))

(defn least-common-multiple [a b]
  (/ (* a b) (greatest-common-divisor a b)))

(defn is-permutation? [a b]
  (= (sort a) (sort b)))

(defn integral? [n]
  (== n (int n)))

(defn digits-of
  "Returns a vector of the digits of n as integers"
  ([n] (digits-of n 10))
  ([n b]
   (loop [n n acc ()]
     (if (= 0 n)
       acc
       (recur (quot n b) (cons (int (mod n b)) acc))))))

(def fibonacci-numbers
  (lazy-cat [0 1] (map +' (rest fibonacci-numbers) fibonacci-numbers)))

(defn palindrome? [n]
  (= (reverse n) n))

(def primes
  "An infinite sequence of primes, generated using the sieve of Eratosthenes.
  Defers adding numbers to the sieve until needed -- uses O(sqrt(n)) space."
  (letfn [(sieve-add [sv start step]
            (assoc! sv
                    (loop [i (+ start step)]
                      (if (nil? (sv i)) i (recur (+ step i))))
                    step))
          (sieve [c sv p primes]
            (if (nil? (sv c))
              (if (< c (* p p))
                (lazy-seq (cons c (sieve (+ 2 c) sv p primes)))
                (let [step (* 2 p)
                      new-sieve (sieve-add sv c step)]
                  (recur (+ 2 c) new-sieve (first primes) (rest primes))))
              (let [step (get sv c)
                    new-sieve (sieve-add (dissoc! sv c) c step)]
                (recur (+ 2 c) new-sieve p primes))))]
    (lazy-cat [2 3 5 7]
      (sieve 9 (transient {}) 3 (drop 2 primes)))))

(defn divides? [n m]
  (= 0 (mod m n)))

(defn prime? [n]
  (if (< n 2)
    false
    (not-any? #(divides? % n) (take-while #(<= % (Math/sqrt n)) primes))))

(defn prime-factors [n]
  (letfn [(factor [n primes]
            (let [p (first primes)]
              (if (<= (* p p) n)
                (if (= 0 (mod n p))
                  (cons p (factor (quot n p) primes))
                  (recur n (rest primes)))
                (list n))))]
    (factor n primes)))

(defn proper-divisors [n]
  (filter #(= 0 (mod n %)) (range 1 n)))

(defn divisors [n]
  (filter #(= 0 (mod n %)) (range 1 (inc n))))

(defn max-index [s]
  (first (apply max-key second (map-indexed vector s))))

(defn n-divisors [n]
  (product (map #(inc (count %)) (partition-by identity (prime-factors n)))))

(defn proper-divisor-sum [n]
  (let [f (fn [primes acc]
            (if (empty? primes)
              acc
              (let [p (first primes)
                    n (count p)
                    x (first p)
                    powers-of-x (iterate (partial * x) x)
                    multiplier (inc (sum (take n powers-of-x))) ]
                (recur (rest primes) (* acc multiplier)))))
        primes (prime-factors n)
        partitioned-primes (partition-by identity primes)]
    (- (f partitioned-primes 1) n)))

(defmacro run [expr]
  "Times code and prints its results."
  `(println (time ~expr)))
