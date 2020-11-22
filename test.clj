(defmacro fn->
  [& args]
  `(fn [x#] (-> x# ~@args)))

(defmacro when->
  [basis pred & args]
  `(when-not (~pred ~basis) (-> ~basis ~@args)))

(defmacro when-not->
  [basis pred & args]
  `(when-> ~basis (complement ~pred) ~@args))

(defn snowball
  []
  (agent (with-meta {} {
                        :snowball true
                        :sent-keys #{}})))

(defn key-sent?
  [target-key]
  (comp target-key :sent-keys meta deref))

(defn base!
  [sb base-key base-val]
  (when-not-> sb (key-sent? base-key)
    (send (fn->
      (assoc base-key base-val)
      (vary-meta update :sent-keys
        #(conj % base-key))))))

(defn step!
  [sb result needed-keys func]
  (let [
        uses (map @sb needed-keys)]
    (when (every? identity uses)
      (when-not-> sb (key-sent? result)
        (send (fn->
          (assoc result (apply func uses))
          (vary-meta update :sent-keys
            #(conj % result))))))))

;; TEST

(def sb (snowball))

(println (macroexpand
  '(when-not-> sb (key-sent? :base-key)
    (send (fn->
      (assoc :base-key :base-val)
      (vary-meta update :sent-keys
        #(conj % :base-key)))))))

#_(comment

(while
    ((complement :result) @sb)
  (base! sb :a 10)
  (base! sb :b 42)
  (step! sb :sum
    [:a :b] +)
  (step! sb :result
    [:sum] dec))

(println @sb)

(assert (= @sb {
                :a 10 :b 42
                :sum 52
                :result 51}))

)

(shutdown-agents)
