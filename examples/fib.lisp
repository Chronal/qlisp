(defn (fibIter : Num) [(a : Num) (b : Num) (n : Num)]
      (if (eq n 0)
          b
          (fibIter b (add a b) (minus n 1))))

(defn (fib : Num) [(n : Num)]
      (fibIter 0 0 n))

(def (n : Num) 10)
(defn (main : Num) []
    (fib n))
