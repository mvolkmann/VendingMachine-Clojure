This is an implementation of the Vending Machine specification
from the St. Louis Lambda Lounge.  See the "specification" link at
http://groups.google.com/group/lambda-lounge/web/language-shootout
author: R. Mark Volkmann, Object Computing, Inc.

In order to run this code you must:
- download and install Clojure,
- download and install Clojure Contrib,
- create a clj script that runs the Clojure interpreter
  and sets the classpath to include:
  * clojure.jar
  * clojure-contrib.jar
  * . (dot) so it can find com/ociweb/vending.clj
- run from the directory that contains the com directory

For help with these steps see
http://java.ociweb.com/mark/clojure/article.html#GettingStarted

To run the tests
  clj vending_test.clj

To run the application
  clj vending_main.clj
