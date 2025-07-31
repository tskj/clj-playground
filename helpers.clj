(ns helpers)

(defn cartesian-product [colls]
  (if (empty? colls)
    '(())
    (for [x (first colls)
          xs (cartesian-product (rest colls))]
      (cons x xs))))

(defn expand-template [template]
  (let [keys (keys template)
        value-options (map #(if (vector? %) % [%]) (vals template))
        combinations (cartesian-product value-options)]
    (map #(zipmap keys %) combinations)))

(defn test-associativity [evaluator]
  (let [{:keys [nd r]} evaluator
        test-values [1 0 :p :b]]
    (and
      ; Test and associativity: (a ∧ b) ∧ c = a ∧ (b ∧ c)
      (every? (fn [[a b c]]
                (= (nd (nd a b) c)
                   (nd a (nd b c))))
              (for [a test-values b test-values c test-values] [a b c]))
      ; Test or associativity: (a ∨ b) ∨ c = a ∨ (b ∨ c)  
      (every? (fn [[a b c]]
                (= (r (r a b) c)
                   (r a (r b c))))
              (for [a test-values b test-values c test-values] [a b c])))))
