(ns clotract.core)

(defn withdraw
  [{:keys [balance] :as state}
   {:keys [amount]}]
  (assert (> amount 0) "amount must be positive")
  (assert (> balance amount) "Amount too high")
  (assoc state :balance (- balance amount)))


(defn deposit
  [{:keys [balance] :as state}
   {:keys [amount]}]
  (assert (> amount 0) "amount must be positive")
  (assoc state :balance (+ balance amount)))

(comment
  (withdraw {:balance 100 :name "Johnny"} {:amount 50})
  (withdraw {:balance 10} {:amount 20})
  (withdraw {:balance 10} {:amount -1})
  (deposit {:balance 100} {:amount 50})
  (deposit {:balance 100} {:amount -50})
  )


(defn withdraw-curry
  [{:keys [amount]}]
  (assert (> amount 0) "amount must be positive")
  (fn [{:keys [balance] :as state}]
    (assert (>= balance amount) "Amount too high")
    (assoc state :balance (- balance amount))))

(defn deposit-curry
  [{:keys [amount]}]
  (assert (> amount 0) "amount must be positive")
  (fn [state]
    (update state :balance (partial + amount))))

(comment
  ((withdraw-curry {:amount 10}) {:balance 10})


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

;; (def step eval)

;; (defn step [event-list command]
;;   (try (eval command)
;;        (catch Exception e (str ))))

;; (comment
;;   (step `(deposit {:balance 0} {:amount 100}))
;;   )

(comment
  (def johnny-account (atom {:name "Johnny B"
                             :balance 1000}))

  (def withdraw-a-hundred (withdraw-curry {:amount 100}))

  (swap! johnny-account withdraw-a-hundred)

  @johnny-account

  (swap! johnny-account #(withdraw % {:amount -900}))
  )
