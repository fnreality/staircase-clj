(defn snowball
  []
  (agent (with-meta {} {
                        :snowball true
                        :sent-keys #{}}))

(defn base!
  [sb base-key base-val]
  (send sb
    #(assoc % base-key base-val)))

(defn step!
  [sb result needed-keys func]
  (when-not (@sb result)
    (let [
          uses (map @sb needed-keys)]
      (when (every? identity uses)
        (send sb
          #(assoc % result
            (apply func uses)))))))

;;TEST

(def sb (snowball))

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

(shutdown-agents)
