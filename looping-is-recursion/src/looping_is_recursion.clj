(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [base exp current-product]
                 (if (zero? exp)
                   current-product
                   (recur base
                          (dec exp)
                          (* base current-product))))]

    (helper base exp 1)))

(defn last-element [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (recur (rest a-seq))))


(defn seq= [seq1 seq2]
  (cond
    (and (empty? seq1) (empty? seq2)) true
    (or (empty? seq1) (empty? seq2)) false
    (= (first seq1) (first seq2)) (recur (rest seq1) (rest seq2))
    :else false))


(defn find-first-index [pred a-seq]
  (loop [current-a-seq a-seq
         count 0]
    (cond
     (empty? current-a-seq) nil
     (pred (first current-a-seq)) count
     :else (recur (rest current-a-seq) (inc count)))))

(defn avg [a-seq]
  (loop [current-a-seq a-seq
         count 0
         current-sum 0]
    (if (empty? current-a-seq)
      (/ current-sum count)
      (recur (rest current-a-seq)
        (inc count)
        (+ (first current-a-seq) current-sum)))))

(defn parity [a-seq]
  (loop [current-a-seq a-seq
         print-seq #{}]
      (cond
         (empty? current-a-seq) print-seq
         (contains? print-seq (first current-a-seq))
           (recur (rest current-a-seq) (disj print-seq (first current-a-seq)))
         :else
           (recur (rest current-a-seq) (conj print-seq (first current-a-seq))))))

(defn fast-fibo [n]
  (cond
   (zero? n) 0
   (= 1 n) 1
   :else
     (loop [count 2
            fib-1 0
            fib-2 1]

       (if (= count n)
         (+ fib-1 fib-2)
         (recur (inc count) fib-2 (+ fib-1 fib-2))))))


(defn cut-at-repetition [a-seq]
  (loop [current-a-seq a-seq
         repe '[]]
      (cond
       (empty? current-a-seq) repe
       (contains? (set repe) (first current-a-seq)) repe
       :else (recur (rest current-a-seq) (conj repe (first current-a-seq))))))
