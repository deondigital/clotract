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

(defn step-n
  ([commands] (step-n nil commands))
  ([contract-state commands] (reduce step contract-state commands)))

(comment
  (step {:state {:balance 0}
         :ledger []}
        `(deposit {:amount 100}))

  (step-n
   `[(create-account)
     (deposit {:amount 100})
     (deposit {:amount 50})])

  (step-n
   `[(create-account)
     (deposit {:amount 100})
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

;;;; Bank

{"acc1" {:name "Johnny" :balance 100}
 "acc2" {:name "George" :balance 500}}

(defn add-account [account-number]
  (fn [state]
    (assoc state account-number ((create-account) nil))))

(defn assert-account-exists [state account-number]
  (assert (state account-number) "account does not exist"))

(defn cash-deposit [{:keys [account-number] :as args}]
  (fn [state]
    (assert-account-exists state account-number)
    (update state account-number (deposit args))))

(defn cash-withdraw [{:keys [account-number] :as args}]
  (fn [state]
    (assert-account-exists state account-number)
    (update state account-number (withdraw args))))

(defn transfer [{:keys [from-account to-account] :as args}]
  (fn [state]
    (assert-account-exists state from-account)
    (assert-account-exists state to-account)
    (-> state
        (update from-account (withdraw args))
        (update to-account (deposit args)))))

(comment
  (verify! (step nil `(add-account ~(name (gensym "acc")))))

  (step-n `[(add-account ~(name (gensym "acc")))])

  (step-n `[(add-account "acc1")
            (cash-deposit {:account-number "acc1"
                           :amount 100})
            (cash-withdraw {:account-number "acc1"
                            :amount 50})])

  (step-n `[(add-account "acc1")
            (cash-deposit {:account-number "acc1"
                           :amount 100})
            (cash-withdraw {:account-number "acc1"
                            :amount 150})])

  (step-n `[(add-account "acc1")
            (cash-deposit {:account-number "acc1"
                           :amount 100})
            (cash-withdraw {:account-number "acc2"
                            :amount 50})])

  (step-n `[(add-account "acc1")
            (add-account "acc2")
            (cash-deposit {:account-number "acc1"
                           :amount 100})
            (transfer {:from-account "acc1" :to-account "acc2" :amount 50})
            (transfer {:from-account "acc1" :to-account "acc2" :amount 60})
            ])
  )
