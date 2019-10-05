(ns clotract.core
  (:require
   [clojure.pprint :as pp]
   ))

;;; Commands

(defn create-account []
  (fn [state]
    (assert (= nil state) "Initial state must be nil")
    {:balance 0}))

(defn set-credit-limit [{:keys [credit-limit]}]
  (assert (> credit-limit 0) "credit limit must be a positive number")
  (fn [state]
    (assoc state :credit-limit credit-limit)))

(defn withdraw
  [{:keys [amount]}]
  (assert (> amount 0) "amount must be positive")
  (fn [{:keys [balance credit-limit] :as state}]
    (assert (>= (+ balance (or credit-limit 0)) amount) "Amount too high")
    (assoc state :balance (- balance amount))))

(defn deposit
  [{:keys [amount]}]
  (assert (> amount 0) "amount must be positive")
  (fn [state]
    (update state :balance (partial + amount))))

;;; Ledger

{:state {}
 :ledger []}

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


(defn verify [{:keys [state ledger] :as obj}]
  (let [derived-state (:state (reduce step {} ledger))]
    (if (= state
           derived-state)
      obj
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

  (verify good)
  (verify bad)
  )

;;;; Bank

{"acc1" {:name "Johnny" :balance 100}
 "acc2" {:name "George" :balance 500}}

(defn add-account [{:keys [account-number]}]
  (fn [state]
    (assert (not (get state account-number)) "account number in use")
    (assoc state account-number ((create-account) nil))))

(defn assert-account-exists [state account-number]
  (assert (state account-number) "account does not exist"))

(defn on-account [account-command]
  (fn [{:keys [account-number] :as args}]
    (fn [state]
      (assert-account-exists state account-number)
      (update state account-number (account-command args)))))

(defn set-account-credit-limit [{:keys [account-number] :as args}]
  (fn [state]
    (assert-account-exists state account-number)
    (update state account-number (set-credit-limit args))))

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
  (verify (step nil `(add-account {:account-number ~(name (gensym "acc"))})))

  (step-n `[(add-account {:account-number ~(name (gensym "acc"))})])

  (step-n `[(add-account {:account-number "acc1"})
            (cash-deposit {:account-number "acc1"
                           :amount 100})
            (cash-withdraw {:account-number "acc1"
                            :amount 50})])

  ;; should fail
  (step-n `[(add-account {:account-number "acc1"})
            (cash-deposit {:account-number "acc1"
                           :amount 100})
            (cash-withdraw {:account-number "acc1"
                            :amount 150})])

  ;; should fail
  (step-n `[(add-account {:account-number "acc1"})
            (cash-deposit {:account-number "acc1"
                           :amount 100})
            (cash-withdraw {:account-number "acc2"
                            :amount 50})])

  ;; should fail
  (step-n `[(add-account {:account-number "acc1"})
            (add-account {:account-number "acc2"})
            (cash-deposit {:account-number "acc1"
                           :amount 100})
            (transfer {:from-account "acc1" :to-account "acc2" :amount 50})
            (transfer {:from-account "acc1" :to-account "acc2" :amount 60})
            ])

  ;; should fail
  (step-n `[(add-account {:account-number "acc1"})
            (add-account {:account-number "acc1"})])


  (step-n `[(add-account {:account-number "acc1"})
            ((on-account set-credit-limit) {:account-number "acc1" :credit-limit 100})])

  (step-n `[(add-account {:account-number "acc1"})
            (set-account-credit-limit {:account-number "acc1" :credit-limit 100})
            (cash-withdraw {:account-number "acc1"
                            :amount 101})])
  )


;;; Analytics
(def bank
  (step-n `[(add-account {:account-number "acc1"})
            (add-account {:account-number "acc2"})
            ((on-account deposit) {:account-number "acc1" :amount 100})
            (transfer {:from-account "acc1" :to-account "acc2" :amount 70})]))

(defn total-balance [bank]
  (apply + (map (fn [[_ account]] (:balance account)) (:state bank))))

(defn pp-accounts [{:keys [state]}]
  (pp/print-table
   (map (fn [[account-number {:keys [balance credit-limit]}]]
          {"Account number" account-number
           "Credit limit" credit-limit
           "Balance" balance})
        state)))
