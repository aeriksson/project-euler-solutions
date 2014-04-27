;; If p is the perimeter of a right angle triangle with integral length sides,
;; {a,b,c}, there are exactly three solutions for p = 120.
;;
;; {20,48,52}, {24,45,51}, {30,40,50}
;;
;; For which value of p ≤ 1000, is the number of solutions maximised?

(load "util")

(def pythagorean-triples-by-perimiter
  (letfn [(triples-with-perimiter [p acc]
            (let [lower-bound (inc (int (Math/sqrt (/ p 4))))
                  upper-bound (Math/sqrt (/ p 2))
                  candidates (filter #(divides? (* 2 %) p)
                                     (range lower-bound upper-bound))
                  new-triples (for [m candidates
                                    :let [n (- (/ p (* 2 m)) m)
                                          a (- (* m m) (* n n))
                                          b (* 2 m n)
                                          c (+ (* m m) (* n n))]]
                                [(min a b) (max a b) c])
                  old-triples (for [d (proper-divisors p)
                                    ts (nth acc d)]
                                (map #(* (quot p d) %) ts))]
              (distinct (concat new-triples old-triples))))
          (f [n acc]
            (let [ts (if (odd? n) () (triples-with-perimiter n acc))]
              (lazy-seq (cons ts (f (inc n) (conj acc ts))))))]
    (f 0 [])))

(defn euler-39 [n]
  (max-index (map count (take n pythagorean-triples-by-perimiter))))

(run (euler-39 1000))
