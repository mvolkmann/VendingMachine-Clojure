; This is an implementation of the Vending Machine specification
; from the St. Louis Lambda Lounge.  See the "specification" link at
; http://groups.google.com/group/lambda-lounge/web/language-shootout
; author: R. Mark Volkmann, Object Computing, Inc.

(ns com.ociweb.vending
  (:use [clojure.contrib.seq-utils :only (find-first)]))

(defstruct item-struct :description :price :quantity)

;---------------------------------------------------------------------------
; Create ref objects for all mutable state.
;---------------------------------------------------------------------------

(def amount-inserted-ref (ref 0.0))

; key is item selector and value is an item-struct
(def item-map-ref (ref (sorted-map)))

; key is money denomination and value is quantity
(def money-map-ref (ref (sorted-map)))

;---------------------------------------------------------------------------

(defn reset-machine
  "resets the state of the vending machine"
  []
  (dosync
    (ref-set amount-inserted-ref 0)
    (ref-set item-map-ref (sorted-map))
    (ref-set money-map-ref (sorted-map))))

(defn add-item
  "adds an item to the vending machine"
  [selector description price quantity]
  (let [item (struct item-struct description price quantity)]
    (dosync (alter item-map-ref assoc selector item))))

(defn add-money
  "adds money to the vending machine to be used for change"
  [value quantity]
  (dosync (alter money-map-ref assoc value quantity)))

(defn fill-machine
  "fills the vending machine with initial items and money"
  []
  (reset-machine)
  (add-item "A" "Juicy Fruit" 0.65 3)
  (add-item "B" "Baked Lays" 1.00 2)
  (add-item "C" "Pepsi" 1.50 4)
  (add-money 0.05 5)
  (add-money 0.10 3)
  (add-money 0.25 4)
  (add-money 1.00 2))

;---------------------------------------------------------------------------

(defn currency
  "returns a currency formatted string for a given dollar amount"
  [dollars]
  (let [formatter (java.text.NumberFormat/getCurrencyInstance)]
    (.format formatter dollars)))

(defn help
  "outputs help on commands"
  []
  (println "Commands are:
  help - show this help
  exit or quit - exit the application
  change - list change available
  items - list items available
  inserted - show amount inserted
  return - coin return
  n - enter a nickel
  d - enter a dime
  q - enter a quarter
  1 - enter a dollar bill
  uppercase letter - buy item with that selector"))

(defn insert-money
  "inserts money in the vending machine for purchasing an item"
  [value]
  (dosync
    ; 0 is the default value returned by get
    ; when the money map doesn't contain a key equal to value.
    (let [new-quantity (inc (get @money-map-ref value 0))]
      (alter money-map-ref assoc value new-quantity)
      (alter amount-inserted-ref + value))))

(def money-code
  {0.05 \n, 0.10 \d, 0.25 \q, 1.0 \1})
    
(def money-name
  {0.05 "nickel", 0.10 "dime", 0.25 "quarter", 1.0 "dollar"})
    
(defn money-string [value quantity]
  (str quantity " " (money-name value) (when (> quantity 1) "s")))

(defn show-change
  "shows the change the vending machine currently holds"
  []
  (println "machine holds:")
  (doseq [[value quantity] @money-map-ref]
    (println (money-string value quantity))))
  
(defn show-inserted
  "shows the amount of money that has been inserted and not yet spent"
  []
  (println "amount inserted is" (currency @amount-inserted-ref)))

(defn show-items
  "shows the items the vending machine currently holds"
  []
  (doseq [[selector, item] @item-map-ref]
    (println selector "-"
      (item :quantity)
      (item :description)
      (currency (item :price)))))
  
;---------------------------------------------------------------------------
 
(defn nearly-equal [v1 v2]
  (< (Math/abs (- v1 v2)) 1e-7))

(defn remove-coin
  "returns a new map with the given coin removed"
  [money-map value]
  (let [quantity (money-map value)]
    (condp = quantity
      nil (throw (RuntimeException.
                   (str "no coin with value " value " to remove")))
      1 (dissoc money-map value)
      (assoc money-map value (dec quantity)))))

(declare make-change) ; needed for mutually recursive functions

(defn make-change-internal
  "This attempts to make an amount of change
   using a given coin value
   and the coins in money-map.
   If successful, the coins needed to make the change
   are added to the change vector and that is returned.
   Otherwise nil is returned."
  [amount value money-map change]
  (cond
    (nearly-equal value amount) (conj change value) ; success
    (< value amount) ; subtract value and try to make remaining change
      (make-change
        (- amount value)
        (remove-coin money-map value)
        (conj change value))
    true nil)) ; failure

(defn make-change
  ([amount money-map]
    (make-change amount money-map []))
  ([amount money-map change]
     (some ; stop on first non-nil result and return it
       #(make-change-internal amount % money-map change)
       ; try highest value coins first
       (reverse (keys money-map)))))
  
(defn coin-return
  "ejects the unused money that has been inserted"
  []
  (when-let [change (make-change @amount-inserted-ref @money-map-ref)]
    (dosync
      (doseq [value change]
        (println (money-code value))
        (ref-set money-map-ref (remove-coin @money-map-ref value)))
      (ref-set amount-inserted-ref 0))))

;---------------------------------------------------------------------------

(defn purchase
  "makes a purchase assuming the item isn't sold out
   and enough money was inserted"
  [selector item]
  (let [new-quantity (dec (item :quantity))
        new-item (assoc item :quantity new-quantity)]
    (dosync
      (alter item-map-ref assoc selector new-item)
      (alter amount-inserted-ref - (item :price))))
  (println selector)
  (coin-return))

(defn attempt-purchase
  "checks for items begin sold out or insufficient money inserted
   before making a purchase"
  [selector item]
  (if (zero? (item :quantity))
    (println "sold out")
    (let [price (item :price)
          inserted @amount-inserted-ref]
      (if (> price inserted)
        (println "insert" (currency (- price inserted)) "more")
        (purchase selector item)))))

(defn select
  "verifies that a valid selector was entered and attempts a purchase"
  [selector]
  (if-let [item (@item-map-ref selector)]
    (attempt-purchase selector item)
    (println "invalid command or selector" (str "\"" selector "\""))))
    
;---------------------------------------------------------------------------
 
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

(defn main
  "called by vending_main.clj"
  []
  (fill-machine)

  (println "Enter commands such as \"help\".")
  (loop [] ; process input
    (print "> ") (flush)
    (let [cmd (read-line)]
      (command cmd)
      (when-not (#{"exit" "quit"} cmd)
        (recur)))))
