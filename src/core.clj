(ns clotract.core)

(defn withdraw [{:keys [balance] :as state} amount]
  (assert (> amount 0) "amount must be positive")
  (if (> amount balance)
    (throw (ex-info "Amount too high" state))
    (assoc state :balance (- balance amount))))

(defn deposit [{:keys [balance] :as state} amount]
  (assert (> amount 0) "amount must be positive")
  (assoc state :balance (+ balance amount)))

(comment
  (withdraw {:balance 100 :name "Johnny"} 50)
  (withdraw {:balance 10} 20)
  (withdraw {:balance 10} -1)
  (deposit {:balance 100} 50)
  (deposit {:balance 100} -50)


  )
