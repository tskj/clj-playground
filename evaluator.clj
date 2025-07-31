(ns evaluator)

(defn make-evaluator [and-candidate or-candidate not-table]
  (let [and-fn (fn [& args]
                 (let [args-vec (vec args)]
                   (loop [result (first args-vec)
                          remaining (rest args-vec)]
                     (if (empty? remaining)
                       result
                       (let [next-arg (first remaining)
                             lookup-result (or (get and-candidate [result next-arg])
                                             (get and-candidate [next-arg result]))]
                         (assert lookup-result
                                 (str "No and-rule found for inputs: " [result next-arg]))
                         (recur lookup-result (rest remaining)))))))
        or-fn (fn [& args]
                (let [args-vec (vec args)]
                  (loop [result (first args-vec)
                         remaining (rest args-vec)]
                    (if (empty? remaining)
                      result
                      (let [next-arg (first remaining)
                            lookup-result (or (get or-candidate [result next-arg])
                                            (get or-candidate [next-arg result]))]
                        (assert lookup-result
                                (str "No or-rule found for inputs: " [result next-arg]))
                        (recur lookup-result (rest remaining)))))))
        not-fn (fn [x] (get not-table x))]
    {:nd and-fn
     :r or-fn
     :n not-fn}))
