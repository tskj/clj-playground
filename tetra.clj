(load-file "helpers.clj")
(load-file "evaluator.clj")

;; this one defines the tetra logic for this file
(def not {0   1
          1   0
          :p  :p
          :b  :b})

(def and-template {[0 0]   0
                   [0 1]   0
                   [1 1]   1
                   [:b :b] [0 :b :p]
                   [:b :p] [0 1 :b :p]
                   [:p :p] 1
                   [1 :b]  :b
                   [1 :p]  :p
                   [0 :b]  0
                   [0 :p]  0})

(def or-template {[0 0]   0
                  [0 1]   1
                  [1 1]   1
                  [:b :b] 0
                  [:b :p] [0 1 :b :p]
                  [:p :p] [1 :b :p]
                  [0 :b]  :b
                  [0 :p]  :p
                  [1 :b]  1
                  [1 :p]  1})

(def and-candidates (helpers/expand-template and-template))
(def or-candidates (helpers/expand-template or-template))

(defn abcd [{:keys [nd r n]} x] ;; nd = and, r = or, n = not
  [x
   (n x)
   (nd x (n x))
   (n (r x (n x)))])

(def valid-combinations
  (for [and-candidate and-candidates
        or-candidate or-candidates
        :let [evaluator (evaluator/make-evaluator and-candidate or-candidate not)
              results (map #(abcd evaluator %) [1 0 :p :b])
              valid? (every? #(= 1 (count (filter #{1} %))) results)]
        :when valid?]
    {:and-candidate and-candidate
     :or-candidate or-candidate
     :results results}))

valid-combinations
(count valid-combinations)

(filter (fn [x] (= (:results x) '([0 1 0 0] [1 0 0 0] [:p :p 1 :p] [:b :b :b 1])))
        valid-combinations)

(count (filter (fn [x] (= (:results x) '([0 1 0 0] [1 0 0 0] [:p :p 1 :p] [:b :b :b 1])))
               valid-combinations))

(def associative-combinations
  (filter (fn [{:keys [and-candidate or-candidate]}]
            (let [evaluator (evaluator/make-evaluator and-candidate or-candidate not)]
              (helpers/test-associativity evaluator)))
          valid-combinations))

(defn one-hot-encoder-1 [{:keys [nd r n]} a b c d]
  (r (nd a (n b) (n c) (n d))
     (nd (n a) b (n c) (n d))
     (nd (n a) (n b) c (n d))
     (nd (n a) (n b) (n c) d)))

(defn one-hot-encoder-2 [{:keys [nd r n]} a b c d]
  (r (nd a (n (r b c d)))
     (nd b (n (r a c d)))
     (nd c (n (r a b d)))
     (nd d (n (r a b c)))))

(defn one-hot-encoder-3 [{:keys [nd r n]} a b c d]
  (nd (r a b c d)  ; at least one is true
      (n (r (nd a b) (nd a c) (nd a d)  ; no pairs are both true
            (nd b c) (nd b d) (nd c d)))))

(defn one-hot-encoder-3-2 [{:keys [nd r n]} a b c d]
  (nd (r a b c d)  ; at least one is true
      (n (r (nd a b) (nd a c) (nd a d)      ; no pairs are both true
            (nd b c) (nd b d) (nd c d)))
      (n (r (nd a b c) (nd a b d)           ; no triples are all true
            (nd a c d) (nd b c d)))
      (n (nd a b c d))))                    ; not all four are true

(defn one-hot-encoder-4 [{:keys [nd r n]} a b c d]
  (nd (r a b c d)          ; at least one is true
      (n (nd a b))         ; not (a and b)
      (n (nd a c))         ; not (a and c)
      (n (nd a d))         ; not (a and d)
      (n (nd b c))         ; not (b and c)
      (n (nd b d))         ; not (b and d)
      (n (nd c d))))       ; not (c and d)

(defn is-zero [{:keys [nd r n]} x]
  (r :b (n (nd :b x))))

(defn is-true [{:keys [n] :as evaluator} x]
  (is-zero evaluator (n x)))

(defn one-hot-encoder-tetra [{:keys [nd r n] :as evaluator} a b c d]
  (let [z? #(n (is-true evaluator %))  ; not-true
        t? #(is-true evaluator %)]
    (r (nd (t? a) (z? b) (z? c) (z? d))
       (nd (z? a) (t? b) (z? c) (z? d))
       (nd (z? a) (z? b) (t? c) (z? d))
       (nd (z? a) (z? b) (z? c) (t? d)))))

(->>
 associative-combinations
 last)

(defn test-encoders-on-combo [combo]
  (let [evaluator (evaluator/make-evaluator (:and-candidate combo)
                                            (:or-candidate combo)
                                            not)]
    {:encoder-1 [(one-hot-encoder-1 evaluator 1 0 0 0)
                 (one-hot-encoder-1 evaluator 0 1 0 0)
                 (one-hot-encoder-1 evaluator 0 0 1 0)
                 (one-hot-encoder-1 evaluator 0 0 0 1)
                 (one-hot-encoder-1 evaluator 1 1 0 0)
                 (one-hot-encoder-1 evaluator 0 0 0 0)]
     :encoder-2 [(one-hot-encoder-2 evaluator 1 0 0 0)
                 (one-hot-encoder-2 evaluator 0 1 0 0)
                 (one-hot-encoder-2 evaluator 0 0 1 0)
                 (one-hot-encoder-2 evaluator 0 0 0 1)
                 (one-hot-encoder-2 evaluator 1 1 0 0)
                 (one-hot-encoder-2 evaluator 0 0 0 0)]
     :encoder-3 [(one-hot-encoder-3 evaluator 1 0 0 0)
                 (one-hot-encoder-3 evaluator 0 1 0 0)
                 (one-hot-encoder-3 evaluator 0 0 1 0)
                 (one-hot-encoder-3 evaluator 0 0 0 1)
                 (one-hot-encoder-3 evaluator 1 1 0 0)
                 (one-hot-encoder-3 evaluator 0 0 0 0)]
     :encoder-3-2 [(one-hot-encoder-3-2 evaluator 1 0 0 0)
                   (one-hot-encoder-3-2 evaluator 0 1 0 0)
                   (one-hot-encoder-3-2 evaluator 0 0 1 0)
                   (one-hot-encoder-3-2 evaluator 0 0 0 1)
                   (one-hot-encoder-3-2 evaluator 1 1 0 0)
                   (one-hot-encoder-3-2 evaluator 0 0 0 0)]
     :encoder-4 [(one-hot-encoder-4 evaluator 1 0 0 0)
                 (one-hot-encoder-4 evaluator 0 1 0 0)
                 (one-hot-encoder-4 evaluator 0 0 1 0)
                 (one-hot-encoder-4 evaluator 0 0 0 1)
                 (one-hot-encoder-4 evaluator 1 1 0 0)
                 (one-hot-encoder-4 evaluator 0 0 0 0)]}))

(map-indexed (fn [idx combo]
               {:combo-index idx
                :results (:results combo)
                :encoder-tests (test-encoders-on-combo combo)})
             associative-combinations)

;; Test one-hot encoders on the abcd results themselves for all combinations
(defn test-encoders-on-abcd-results [combo]
  (let [evaluator (evaluator/make-evaluator (:and-candidate combo)
                                            (:or-candidate combo)
                                            not)
        results (:results combo)]
    {:encoder-1-on-results (map (fn [abcd-result]
                                  (apply one-hot-encoder-1 evaluator abcd-result))
                                results)
     :encoder-2-on-results (map (fn [abcd-result]
                                  (apply one-hot-encoder-2 evaluator abcd-result))
                                results)
     :encoder-3-on-results (map (fn [abcd-result]
                                  (apply one-hot-encoder-3 evaluator abcd-result))
                                results)
     :encoder-3-2-on-results (map (fn [abcd-result]
                                    (apply one-hot-encoder-3-2 evaluator abcd-result))
                                  results)
     :encoder-4-on-results (map (fn [abcd-result]
                                  (apply one-hot-encoder-4 evaluator abcd-result))
                                results)}))

(map-indexed (fn [idx combo]
               {:combo-index idx
                :abcd-results (:results combo)
                :encoder-tests-on-abcd (test-encoders-on-abcd-results combo)})
             associative-combinations)

;; Test is-zero and is-true functions on all four associative combinations
(defn test-is-zero-is-true [combo]
  (let [evaluator (evaluator/make-evaluator (:and-candidate combo)
                                            (:or-candidate combo)
                                            not)
        test-values [1 0 :p :b]]
    {:is-zero-results (map #(is-zero evaluator %) test-values)
     :is-true-results (map #(is-true evaluator %) test-values)}))

(map-indexed (fn [idx combo]
               {:combo-index idx
                :tests (test-is-zero-is-true combo)})
             associative-combinations)

;; Test tetralogic one-hot encoder on the last system
(let [last-combo (last associative-combinations)
      evaluator (evaluator/make-evaluator (:and-candidate last-combo)
                                          (:or-candidate last-combo)
                                          not)
      test-inputs [[1 0 0 0] [0 1 0 0] [0 0 1 0] [0 0 0 1]
                   [:p :p 1 0] [:b :b 0 1] [:p :p 1 :p] [:b :b :b 1]]]
  (map (fn [input]
         {:input input
          :tetra-encoder (apply one-hot-encoder-tetra evaluator input)})
       test-inputs))

;; Test all 256 combinations to verify one-hot encoder correctness
(let [last-combo (last associative-combinations)
      evaluator (evaluator/make-evaluator (:and-candidate last-combo)
                                          (:or-candidate last-combo)
                                          not)
      all-values [0 1 :p :b]
      all-combinations (for [a all-values b all-values c all-values d all-values]
                         [a b c d])
      results (map (fn [combo]
                     {:input combo
                      :tetra-result (apply one-hot-encoder-tetra evaluator combo)
                      :expected-one-hot? (= 1 (count (filter #{1} combo)))})
                   all-combinations)]
  {:total-combinations (count results)
   :one-hot-cases (filter :expected-one-hot? results)
   :non-one-hot-cases (remove :expected-one-hot? results)
   :correct-true-cases (count (filter #(and (:expected-one-hot? %)
                                            (= 1 (:tetra-result %)))
                                      results))
   :correct-false-cases (count (filter #(and (not (:expected-one-hot? %))
                                             (not= 1 (:tetra-result %)))
                                       results))
   :total-correct (count (filter #(= (:expected-one-hot? %)
                                     (= 1 (:tetra-result %)))
                                 results))})
