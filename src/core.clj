(ns clotract.core)

;;; Commands

(defn create-account []
  (fn [state]
    (assert (= nil state) "Initial state must be nil")
    {:balance 0}))

(defn withdraw
  [{:keys [amount]}]
  (assert (> amount 0) "amount must be positive")
  (fn [{:keys [balance] :as state}]
    (assert (>= balance amount) "Amount too high")
    (assoc state :balance (- balance amount))))

(defn deposit
  [{:keys [amount]}]
  (assert (> amount 0) "amount must be positive")
  (fn [state]
    (update state :balance (partial + amount))))

;;; Ledger

(defn step [{:keys [state ledger] :as contract-state} command]
  (let [new-state
        (try ((eval command) state)
             (catch Error e
               (throw (ex-info "Unexpected event"
                               (assoc contract-state
                                      :command-exception e)))))]
    {:state new-state
     :ledger (vec (conj ledger command))}))

(comment
  (step (step {:state {:balance 0}
               :ledger []}
              `(deposit {:amount 100}))
        `(deposit {:amount 50}))

  (reduce step
          {:state {:balance 0} :ledger []}
          `[(deposit {:amount 100})
            (deposit {:amount 50})])

  (reduce step
          {:state {:balance 0} :ledger []}
          `[(deposit {:amount 100})
            (deposit {:amount 50})
            (withdraw {:amount 14000})
            (withdraw {:amount 1000})])
  )


(defn verify! [{:keys [state ledger]}]
  (let [derived-state (:state (reduce step {} ledger))]
    (if-not
        (= state
           derived-state)
      (throw (ex-info "Could not validate contract object"
                      {:expected-state state
                       :derived-state derived-state})))))

(comment
  (def good
    (reduce step {}
            `[(create-account)
              (deposit {:amount 60})
              (deposit {:amount 40})]))

  (def bad (update-in good [:state :balance] #(- % 1000)))

  (verify! good)
  (verify! bad)
  )
