(defmacro fn->
  [& args]
  `(fn [x#] (-> x# ~@args)))

(defmacro when-not->
  [basis pred & args]
  `(when-not (~pred ~basis) (-> ~basis ~@args)))

(defn snowball
  [start]
  (agent (with-meta start {
                            :snowball true
                            :sent-keys #{}})))

(defn key-sent?
  [target-key]
  (comp target-key :sent-keys meta deref))

(defn step!
  [sb result needed-keys func]
  (let [
        uses (map @sb needed-keys)]
    (when (every? identity uses)
      (when-not-> sb (key-sent? result)
        (send (fn-> (assoc result (apply func uses))
          (vary-meta update :sent-keys
            #(conj % result))))))))

(defn try!
  [sb pt]
  (map (partial apply step! sb) pt))

(defmacro path
  [paths*]
  (map (fn [[target _ func _ needed-keys]]
      (eval [target needed-keys func]))
    (partition 5 paths*)))

(defn returning
  [return proc]
  #(do (apply proc %&) return))

;;TEST

(def sb (snowball {
                    :a 10
                    :b 42}))

(def pt (path [
                :sum <- + <- [:a :b]
                :result <- dec <- [:sum]
                :done? <- (returning true println) <- [:result]]))

(while ((complement @sb) :result)
  (try! sb pt))

(assert (= @sb {
                 :a 10
                 :b 42
                 :sum 52
                 :result 51
                 :done? true}))

(shutdown-agents)
