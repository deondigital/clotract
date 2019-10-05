(ns clotract.core)

(defn withdraw
  [{:keys [balance] :as state}
   {:keys [amount] :as command}]
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

(comment
  (def johnny-account (atom {:name "Johnny B"
                             :balance 1000}))

  (swap! johnny-account #(withdraw % {:amount 100}))

  @johnny-account

  (swap! johnny-account #(withdraw % {:amount -900}))
  )
