; These are tests for an implementation of the Vending Machine specification
; from the St. Louis Lambda Lounge.  See the "specification" link at
; http://groups.google.com/group/lambda-lounge/web/language-shootout
; author: R. Mark Volkmann, Object Computing, Inc.

(use 'clojure.contrib.test-is)
(use 'com.ociweb.vending)

(defn my-fixture
  "wraps the execution of each test function to provide setup and teardown"
  [test-function]
  (fill-machine) ; setup
  (test-function)
  ; no tear-down is needed
)

(use-fixtures :each my-fixture)

(defn commands
  "calls the vending machine command function
   on each command in a comma-delimited string"
  [s]
  (doseq [c (.split s ",")] (command c)))

(defn output-is
  "compares the output of a comma-delimited set of commands
   to an expected string"
  [cmds expected]
  (is (= expected (with-out-str (commands cmds)))))

(defn output-matches
  "compares the output of a comma-delimited set of commands
   to an expected regular expression"
  [cmds expected-re]
  (is (re-find expected-re (with-out-str (commands cmds)))))

;---------------------------------------------------------------------------

(deftest buy-with-exact-change
  (output-is "q,q,d,n,A" "A\n"))

(deftest buy-with-excess-change
  (output-is "1,1,A" "1
q
d
A
"))

(deftest buy-with-insufficient-change
  (output-is "q,q,n,A" "insert $0.10 more\n"))

(deftest change-test
  (output-is "change" "machine holds:
5 nickels
3 dimes
4 quarters
2 dollars
"))

(deftest help-test
  (output-matches "help" #"^Commands are:\n"))

(deftest inserted-test
  (output-is "inserted" "amount inserted is $0.00\n")
  (output-is "1,q,inserted" "amount inserted is $1.25\n"))

(deftest items-test
  (output-is "items" "A - 3 Juicy Fruit $0.65
B - 2 Baked Lays $1.00
C - 4 Pepsi $1.50
"))

(deftest return-test
  (output-is "n,d,d,q,1,return" "1
q
q
"))

(deftest reset-machine-test
  (reset-machine)
  (output-is "change" "machine holds:\n")
  (output-is "inserted" "amount inserted is $0.00\n")
  (output-is "items" ""))

(deftest add-item-test
  (reset-machine)
  (add-item "A" "Juicy Fruit" 65 1)
  (output-is "items" "A - 1 Juicy Fruit $0.65\n"))

(deftest add-moneytest
  (reset-machine)
  (add-money 25 1)
  (output-is "change" "machine holds:
1 quarter
"))

(deftest bad-change-test
  (reset-machine)
  (add-item "A" "Juicy Fruit" 65 1)
  (add-money 25 1)
  (let [expected (item-quantity "A")]
    (output-is "q,q,q,A" "use correct change\n")
    ; Verify that the quantity of the item didn't change.
    (is (= expected (item-quantity "A")))))

; Run all the tests in the current namespace.
(run-tests)
