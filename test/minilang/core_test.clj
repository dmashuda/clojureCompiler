(ns minilang.core-test
  (:require [clojure.test :refer :all]
            [minilang.core :refer :all]
            [minilang.lexer :as lexer]))

;(deftest a-test
;  (testing "FIXME, I fail."
;    (is (= 0 1))))

(defn join-lines [& string-list]
  (reduce str (map (fn [s] (str s "\n")) string-list)))

(def lines
  (join-lines
    "var a;"
    "var b;"
    "a := 4;"
    "b := 5;"
    "a * b;"))

(def expected-tokens
  [["var" :var]
   ["a" :identifier]
   [";" :semicolon]
   ["var" :var]
   ["b" :identifier]
   [";" :semicolon]
   ["a" :identifier]
   [":=" :op_assign]
   ["4" :int_literal]
   [";" :semicolon]
   ["b" :identifier]
   [":=" :op_assign]
   ["5" :int_literal]
   [";" :semicolon]
   ["a" :identifier]
   ["*" :op_mul]
   ["b" :identifier]
   [";" :semicolon]])

(defn make-raw-test-lexer []
  (lexer/create-raw-lexer (java.io.StringReader. lines)))

(defn make-test-lexer []
  (lexer/create-lexer (java.io.StringReader. lines)))

(deftest fill-line-test
  (testing "fill-line"
           (let [lexer (make-raw-test-lexer)
                 after-fill-line (lexer/fill-line lexer)]
             (is (= "var a;" (:line after-fill-line))))))

(defn nth-token [lexer n]
  (nth (lexer/token-sequence lexer) n))

;(deftest first-token-test
;  (testing "token 0"
;           (let [lexer (make-test-lexer)]
;             (is (= ["var" :var] (nth-token lexer 0))))))
;
;(deftest second-token-test
;  (testing "token 1"
;           (let [lexer (make-test-lexer)]
;             (is (= ["a" :identifier] (nth-token lexer 1))))))

; Check that the sequence of tokens produced by the lexer matches
; the expected sequence
;(deftest lexer-test
;  (testing "Expected tokens vs lexer token sequence"
;           (map
;             (fn [expected actual] (is (= expected actual)))
;             expected-tokens
;             (lexer/token-sequence (make-test-lexer)))))

(deftest lexer-test
  (testing "Expected tokens vs lexer token sequence"
           (is (= (nth expected-tokens 0) (nth (lexer/token-sequence (make-test-lexer)) 0)))
           (is (= (nth expected-tokens 1) (nth (lexer/token-sequence (make-test-lexer)) 1)))
           (is (= (nth expected-tokens 2) (nth (lexer/token-sequence (make-test-lexer)) 2)))
           (is (= (nth expected-tokens 3) (nth (lexer/token-sequence (make-test-lexer)) 3)))
           (is (= (nth expected-tokens 4) (nth (lexer/token-sequence (make-test-lexer)) 4)))
           (is (= (nth expected-tokens 5) (nth (lexer/token-sequence (make-test-lexer)) 5)))
           (is (= (nth expected-tokens 6) (nth (lexer/token-sequence (make-test-lexer)) 6)))
           (is (= (nth expected-tokens 7) (nth (lexer/token-sequence (make-test-lexer)) 7)))
           (is (= (nth expected-tokens 8) (nth (lexer/token-sequence (make-test-lexer)) 8)))
           (is (= (nth expected-tokens 9) (nth (lexer/token-sequence (make-test-lexer)) 9)))
           (is (= (nth expected-tokens 10) (nth (lexer/token-sequence (make-test-lexer)) 10)))
           (is (= (nth expected-tokens 11) (nth (lexer/token-sequence (make-test-lexer)) 11)))
           (is (= (nth expected-tokens 12) (nth (lexer/token-sequence (make-test-lexer)) 12)))
           (is (= (nth expected-tokens 13) (nth (lexer/token-sequence (make-test-lexer)) 13)))
           (is (= (nth expected-tokens 14) (nth (lexer/token-sequence (make-test-lexer)) 14)))
           (is (= (nth expected-tokens 15) (nth (lexer/token-sequence (make-test-lexer)) 15)))
           (is (= (nth expected-tokens 16) (nth (lexer/token-sequence (make-test-lexer)) 16)))
           (is (= (nth expected-tokens 17) (nth (lexer/token-sequence (make-test-lexer)) 17)))))

(def aplusb "a + b;")
(def aplusb-tokens (lexer/token-sequence (lexer/create-lexer (java.io.StringReader. aplusb))))

(deftest aplusb-test 
  (testing "a + b; tokens"
           (is (= ["a" :identifier] (nth aplusb-tokens 0)))
           (is (= ["+" :op_plus] (nth aplusb-tokens 1)))
           (is (= ["b" :identifier] (nth aplusb-tokens 2)))
           (is (= [";" :semicolon] (nth aplusb-tokens 3)))))