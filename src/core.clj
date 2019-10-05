(ns clotract.core)

(defn account
  [balance]
  (fn [command]
    (assert (> (:amount command) 0) "Amount must be a positive number")
    (case (:type command)
      :withdraw
      (if (> (:amount command) balance)
        (throw (ex-info "Amount too high" {:balance balance}))
        (account (- balance (:amount command))))
      :deposit
      (account (+ balance (:amount command))))))
