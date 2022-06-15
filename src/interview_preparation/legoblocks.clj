(ns interview-preparation.legoblocks)

(def lego-mod 1000000007)

(defn seq-row-perm
  []
  (let [initial-values [1 2 4 8]
        add-mod #(mod (+ %1 %2 %3 %4) lego-mod)]
    (map first
         (iterate (fn [[a b c d]] [b c d (add-mod a b c d)]) initial-values))))

(defn one-row-perm
  [n]
  (nth (seq-row-perm) n))

(def mem-perm (memoize one-row-perm))

(defn seq-all-combinations
  [one-perm] (iterate #(mod (* % one-perm) lego-mod) one-perm))

(defn all-combinations
  [h w] (nth (seq-all-combinations (mem-perm (- w 1))) (- h 1)))

(def mem-comb (memoize all-combinations))

(defn calc-next
  [coll h]
  (loop [c coll
         acc 0]
    (if (empty? c)
      (conj coll (mod (- ( mem-comb h (+ 1 (count coll))) acc) lego-mod))
      (recur 
       (rest c) 
       (mod (+ acc (* (first c) (mem-comb h (count c)))) lego-mod)
       ))))

(def mem-cal-next (memoize calc-next))

(defn calc 
  [h w]
  (loop [coll [1]]
    (if (= w (count coll))
      coll
      (recur (mem-cal-next coll h))
      )
    ))

(def mem-calc (memoize calc))

(defn legoBlocks [n m]
  (last (mem-calc n m)))

(legoBlocks 7 10)