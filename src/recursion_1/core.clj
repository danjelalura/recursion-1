
(ns recursion-1.core
  (:gen-class))

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))



(defn sum ([coll] 
           (sum coll 0))

           ([coll acc-value]
            (if (empty? coll)
              acc-value
              (sum (rest coll) (+ acc-value (first coll))))))

(sum [1 2 3])



(defn singleton? [coll]
  (if (empty? coll)
    false
    (empty? (rest coll))))

(defn my-last [coll]
  (if (empty? (rest coll))
    (first coll)
    (my-last (rest coll))))

(defn max-element [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (max (first a-seq)
         (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1)
         (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (seq-max (first a-seq)
         (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    '()
    (if (pred? (first a-seq))
      (cons (first a-seq)
            (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq)
     false
   (= elem (first a-seq))
     true
   :else
     (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
     (empty? a-seq)
       '()
     (pred? (first a-seq))
       (cons (first a-seq) (my-take-while pred? (rest a-seq)))
     :else
       '()))



(defn my-take [n coll]
  (cond
    (empty? coll)
    '()
    (> n 0)
    (cons (first coll )(my-take (dec n) (rest coll)))
    :else
    '()))

(my-take 2 [1 2 3 4])

(defn my-drop [n coll]
  (cond
    (empty? coll)
    '()
    (> n 0)
    (my-drop (dec n) (rest coll))
    :else
    coll))

(defn my-drop-while [pred? a-seq]
  (cond
     (empty? a-seq)
       '()
     (pred? (first a-seq))
       (my-drop-while pred? (rest a-seq))
     :else
     a-seq))

(my-drop-while odd? [1 2 3 4])

(defn halve [coll]
  (vector (my-take (int (/(count coll) 2)) coll)
         (my-drop (int (/ (count coll) 2)) coll)))

                                        ;(seq-merge [4] [1 2 6 7])

                                       ;(seq-merge [1 5 7 9] [2 2 8 10])
(concat [1 2] [12 3])

(defn seq-merge [a-seq b-seq]
  (let [merged (concat a-seq b-seq)]
    (if (< (first merged)))))

(defn seq= [a-seq b-seq]
  (cond
     (and (empty? a-seq)
          (empty? b-seq))
       true
     (and (seq a-seq)
          (seq b-seq)
          (= (first a-seq)
             (first b-seq)))
       (seq= (rest a-seq)
             (rest b-seq))
     :else
       false))

(defn my-map [f a-seq b-seq]
   (if (or (empty? a-seq)
            (empty? b-seq))
       '()
     (cons (f (first a-seq)
              (first b-seq))
       (my-map f (rest a-seq)
                 (rest b-seq)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))


(defn fib [n]
  (if (> n 1)
    (+ (fib (dec n))
       (fib (dec (dec n))))
    n))

;; Write the function (my-repeat how-many-times what-to-repeat) that generates a list with
;; what-to-repeat repeated how-many-times number of times.

(defn my-repeat [times val]
  (if (zero? times)
   '()
    (cons val (my-repeat (dec times) val))))

(my-repeat 2 :a) 



;; Write the function (my-range up-to) that works like this

(defn my-range [n]
  (if (< n 1)
    '()
    (cons (dec n) (my-range (dec n)))))



(my-range 0)  ;=> ()
(my-range 1)  ;=> (0)
(my-range 2)  ;=> (1 0)
(my-range 3)  ;=> (2 1 0)
(my-range 4)

;; Write the functions tails and inits that return all the suffixes and prefixes of a sequence, respectively.

(defn tails [coll]
  (if (empty? coll)
    '()
    (cons coll (tails (rest coll)))))
(tails [1 2 3 4])


(defn inits [coll]
  (if (empty? coll)(concat coll )))

(inits [1 2 3 4])


(defn inits [coll]
  (let [count (count coll)]
    (if (= count 0)
      '()
      (cons coll (inits (take (dec count) coll))))))


(nth [1 2 3] 0)

(defn rotations [a-seq]
  (if (empty? a-seq)
    [[]]
    (loop [a []
           b a-seq
           acc []]
      (if (empty? b)
        acc
        (let [f (first b)
              r (rest b)]
          (recur (conj a f)
                 r
                 (conj acc (concat b a))))))))

;; Write the function (my-frequencies a-seq) that computes a map of how many times each element occurs in a sequence. E.g.:

(defn count-elem-helper [n elem coll]
  (if (empty? coll)
    n
    (let [new-count (if (= elem (first coll))
                      (inc n)
                      n)]
      (count-elem-helper new-count
                         elem
                         (rest coll)))))

(defn count-elem [elem coll]
  (count-elem-helper 0 elem coll))


(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    (let [el (first a-seq)
          sofar (get freqs el 0)
          new-freqs (assoc freqs el (inc sofar))]
      (my-frequencies-helper new-freqs (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(my-frequencies [:a "moi" :a "moi" "moi" :a 1])


(defn un-frequencies [a-map]
  (reduce concat '()
          (map (fn [[k v]] (repeat v k)) a-map)))



(un-frequencies (my-frequencies [:a "moi" :a "moi" "moi" :a 1]))



(defn seq-merge [a-seq b-seq]
  (cond
    (empty? a-seq)
    b-seq
    (empty? b-seq)
    a-seq
    :else
    (let [fa (first a-seq)
          fb (first b-seq)]
      (if (< fa fb)
        (cons fa (seq-merge (rest a-seq) b-seq))
        (cons fb (seq-merge a-seq (rest b-seq)))))))

(seq-merge [4] [1 2 6 7])

(seq-merge [1 5 7 9] [2 2 8 10])
