; This is a Clojure implementation of the "Vending Machine Specification".
; See the "specification" link near the top of
; http://groups.google.com/group/lambda-lounge/web/language-shootout.

(ns com.ociweb.vending
  (:use [clojure.contrib.seq-utils :only (find-first)]))

;---------------------------------------------------------------------------

(defstruct item-struct :description :price :quantity)

;---------------------------------------------------------------------------
; Create ref objects for all mutable state.
;---------------------------------------------------------------------------

(def amount-inserted-ref (ref 0.0))

; key is item selector and value is item
(def item-map-ref (ref (sorted-map)))

; key is money denomination and value is quantity
(def money-map-ref (ref (sorted-map)))

;---------------------------------------------------------------------------

(defn currency
  "returns a currency formatted string for a given dollar amount"
  [dollars]
  (let [formatter (java.text.NumberFormat/getCurrencyInstance)]
    (.format formatter dollars)))

(def money-code
  {0.05 \n, 0.10 \d, 0.25 \q, 1.0 \1})
    
(def money-name
  {0.05 "nickel", 0.10 "dime", 0.25 "quarter", 1.0 "dollar"})
    
(defn money-string [value quantity]
  (str quantity " " (money-name value) (when (> quantity 1) "s")))

(defn remove-coin [money-map value]
  (let [quantity (money-map value)]
    (condp = quantity
      nil (throw (RuntimeException.
                   (str "can't remove coin with value " value)))
      1 (dissoc money-map value)
      (assoc money-map value (dec quantity)))))

(defn nearly-equal [v1 v2]
  (< (Math/abs (- v1 v2)) 1e-7))

(declare make-change)

(defn make-change-internal [amount money-map change value]
  (cond
    (nearly-equal value amount) (conj change value)
    (< value amount)
    (make-change
      (- amount value)
      (remove-coin money-map value)
      (conj change value))
    true nil))

(defn make-change
  ([amount money-map] (make-change amount money-map []))
  ([amount money-map change]
     (some ; returns the first non-nil result
       #(make-change-internal amount money-map change %)
       (reverse (keys money-map)))))
  
(defn add-item
  "adds an item to the vending machine"
  [selector description price quantity]
  (let [item (struct item-struct description price quantity)]
    (dosync (alter item-map-ref assoc selector item))))

(defn add-money
  "adds money to the vending machine to be used for change"
  [value quantity]
  (dosync (alter money-map-ref assoc value quantity)))

(defn show-change
  "shows the change the vending machine currently holds"
  []
  (println "machine holds:")
  ; TODO: Is there a way to loop through
  ; TODO: all the key/value pairs like in Ruby?
  ; TODO: Maybe loop through entry objects and destructure them.
  (doseq [[value quantity] @money-map-ref]
    (println (money-string value quantity))))
  
(defn help
  "outputs help instructions"
  []
  (println "Commands are:")
  (println "  help - show this help")
  (println "  exit or quit - exit the application")
  (println "  change - list change available")
  (println "  items - list items available")
  (println "  inserted - show amount inserted")
  (println "  return - coin return")
  (println "  n - enter a nickel")
  (println "  d - enter a dime")
  (println "  q - enter a quarter")
  (println "  1 - enter a dollar bill")
  (println "  uppercase letter - buy item with that selector"))

(defn coin-return
  "returns true if successful and false if it couldn't make correct change."
  []
  (when-let [change (make-change @amount-inserted-ref @money-map-ref)]
    (doseq [value change]
      (println (money-code value))
      (dosync
        (ref-set money-map-ref (remove-coin @money-map-ref value))
        (ref-set amount-inserted-ref 0)))))

(defn show-inserted
  "shows the amount of money that has been inserted and not yet spent"
  []
  (println "amount inserted is" (currency @amount-inserted-ref)))

(defn show-items
  "shows the items the vending machine currently holds"
  []
  (doseq [selector (keys @item-map-ref)]
    (let [item (@item-map-ref selector)]
      (println selector "-"
        (item :quantity)
        (item :description)
        (currency (item :price))))))
  
(defn insert-money
  "inserts money in the vending machine for purchasing an item"
  [value]
  (dosync
    (let [new-quantity (inc (get @money-map-ref value 0))]
      (alter money-map-ref assoc value new-quantity)
      (alter amount-inserted-ref + value))))

(defn- purchase [selector item]
  (let [new-quantity (dec (item :quantity))
        new-item (assoc item :quantity new-quantity)]
    (dosync
      (alter item-map-ref assoc selector new-item)
      (alter amount-inserted-ref - (item :price))))
  (println selector)
  (coin-return))

(defn- attempt-purchase [selector item]
  (if (zero? (item :quantity))
    (println "sold out")
    (let [price (item :price)
          inserted @amount-inserted-ref]
      (if (> price inserted)
        (println "insert" (currency (- price inserted)) "more")
        (purchase selector item)))))

(defn select
  "attempts to purchase the item with the given selector"
  [selector]
  (if-let [item (@item-map-ref selector)]
    (attempt-purchase selector item)
    (println "invalid command or selector" (str "\"" selector "\""))))
    
; TODO: Consider changing this to call functions with eval
; TODO: to be more like the Ruby version.
(defn command [cmd]
  "processes a command string"
  (condp = cmd
    "help" (help)
    "exit" nil
    "quit" nil
    "change" (show-change)
    "inserted" (show-inserted)
    "items" (show-items)
    "return" (coin-return)
    "n" (insert-money 0.05)
    "d" (insert-money 0.10)
    "q" (insert-money 0.25)
    "1" (insert-money 1.00)
    (select cmd)))

;---------------------------------------------------------------------------
; Code above this has been compared to Ruby version.
;---------------------------------------------------------------------------

(defn- not-more-and-available
  "determines if a given coin denomination is both
   not more than inserted and available in the machine"
  [denomination inserted]
  (and (<= denomination inserted) (@money-map-ref denomination)))

(defn quantity
  "gets the quantity remaining for a given item selector"
  [selector]
  (let [item (@item-map-ref selector)]
    (item :quantity)))

(defn reset-machine
  "resets the state of the vending machine"
  []
  (dosync
    (ref-set amount-inserted-ref 0)
    (ref-set item-map-ref (sorted-map))
    (ref-set money-map-ref (sorted-map))))

(defn fill-machine []
  (reset-machine)
  (add-item "A" "Juicy Fruit" 0.65 3)
  (add-item "B" "Baked Lays" 1.00 2)
  (add-item "C" "Pepsi" 1.50 4)
  (add-money 0.05 5)
  (add-money 0.10 3)
  (add-money 0.25 4)
  (add-money 1.00 2))

; Note that this is called by vending_main.clj.
(defn main []
  ; Add items to vending machine.
  (add-item "A" "Juicy Fruit" 0.65 3)
  (add-item "B" "Baked Lays" 1.00 2)
  (add-item "C" "Pepsi" 1.50 4)

  ; Add money to vending machine.
  (add-money 0.05 5)
  (add-money 0.10 3)
  (add-money 0.25 4)
  (add-money 1.00 2)

  ; Process input.
  (println "Enter commands such as \"help\".")
  (loop []
    (print "> ") (flush)
    (let [cmd (read-line)]
      (command cmd)
      (when-not (or (= cmd "exit") (= cmd "quit")) (recur)))))
