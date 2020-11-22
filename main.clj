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

