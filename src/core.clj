(ns clotract.core)

;;; Commands

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

(defn step [{:keys [state ledger] :as contract-state} command]
  (let [new-state
        (try ((eval command) state)
             (catch Error e
               (throw (ex-info "Unexpected event"
                               (assoc contract-state
                                      :command-exception e)))))]
    {:state new-state
     :ledger (conj ledger command)}))

(comment
  (step (step {:state {:balance 0}
               :ledger []}
              `(deposit {:amount 100}))
        `(deposit {:amount 50}))

  (reduce step
          {:state {:balance 0} :ledger []}
          `[(deposit {:amount 100})
            (deposit {:amount 50})
            (withdraw {:amount 14000})
            (withdraw {:amount 1000})])
  )

(comment
  (def acc (atom {:state {:balance 0}
                  :ledger []}))

  (def command `(deposit-curry {:amount 100}))
  (let [{:keys [state history]} @acc
        new-state ((eval command) state)
        new-history (conj history command)]
    (reset! acc {:state new-state :history new-history}))
  )

