(defn snowball
  []
  (agent {} :meta {
                    :snowball true
                    :sent-keys #{}}))

(defn base!
  [var-quoted base-key base-val]
  (send @var-quoted
    #(assoc % base-key base-val)))

(defn step!
  [var-quoted result needed-keys func]
  (when
    (let [
          uses (map @@var-quoted needed-keys)]
      (when (and
        (every? identity uses)
        (:snowball (meta var-quoted))
        ((comp not result :sent-keys)
          (meta var-quoted)))
        (send @var-quoted
          #(assoc % result
            (apply func uses)))
        (alter-meta! var-quoted :sent-keys
          #(conj % result))))))

;;TEST

(def sb (snowball))

(while
    ((complement :result) @sb)
  (base! #'sb :a 10)
  (base! #'sb :b 42)
  (step! #'sb :sum
    [:a :b] +)
  (step! #'sb :result
    [:sum] dec))

(println @sb)

(assert (= @sb {
                :a 10 :b 42
                :sum 52
                :result 51}))

(shutdown-agents)
