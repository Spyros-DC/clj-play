(ns interview-preparation.legoblocks)

(def lego-mod 1000000007)

(def lego-length 1000)

(defn loop-arr []
  (let [^longs initial-values [1 2 4 8]
        add-mod #(mod (+ ^long %1 ^long %2 ^long %3 ^long %4) ^long lego-mod)]
    (loop [size 4
           arr initial-values]
      (if (= size lego-length)
        arr
        (recur
         (inc size)
         (conj arr (add-mod
                    (nth arr (- size 1))
                    (nth arr (- size 2))
                    (nth arr (- size 3))
                    (nth arr (- size 4)))))))))


(def row-arr (loop-arr))

(defn power-elem ^long
  [base exponent]
  (loop [n base e 1]
    (if (= e exponent)
      n
      (recur (mod (* n base) lego-mod) (inc e)))))

(defn power-row ^longs
  [h w arr]
  (loop
   [new-arr ^longs []
    width 0]
    (if (= width w)
      new-arr
      (recur  (conj new-arr (power-elem (nth arr width) h)) (inc width)))))

(defn calc-next-sum
  [^longs coll ^longs power-arr]
  (loop [acc 0
         c-idx 0
         p-idx (- (count coll) 1)]
    (if (= p-idx -1)
      acc
      (recur
       (mod (+ acc (mod (* (nth coll c-idx) (nth power-arr p-idx)) lego-mod)) lego-mod)
       (inc c-idx)
       (dec p-idx)))))

(defn calc-next
  [^longs coll ^longs power-arr]
  (mod (- (nth power-arr (count coll)) (calc-next-sum coll power-arr)) lego-mod))

(defn calc
  [h w]
  (let [pow-arr (power-row h w row-arr)]
    (loop [coll (transient [1])]
      (if (= w (count coll))
        (persistent! coll)
        (recur (conj! coll (calc-next coll pow-arr)))))))

(def memo-calc (memoize calc))

(defn legoBlocks [n m]
  (peek (memo-calc n m)))

;; (calc-next [1] 2)
;; (legoBlocks 271 700)
;; (legoBlocks 418 840)
;; (legoBlocks 570 364)
;; (legoBlocks 623 795)
;; (legoBlocks 174 848)
;; (legoBlocks 432 463)
;; (legoBlocks 683 391)
;; (legoBlocks 293 792)