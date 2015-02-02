(ns minilang.parser2
  (:require [minilang.lexer :as lexer])
  (:require [minilang.prettyprint :as pp])
  (:require [minilang.node :as node]))

; ------------------------------------------------------------
; Data types
; ------------------------------------------------------------

; Record for parse tree nodes.
;
; symbol indicates the (terminal or nonterminal) symbol.
; value is the "value" of the node, which for terminal nodes
; is the lexeme of the token, and for nonterminal nodes
; is the list of child nodes.
;(defrecord Node [symbol value])

; Result of expanding a single right-hand-side symbol:
; A single parse node, and a sequence containing the remaining
; input tokens.
(defrecord SingleParseResult [node tokens])

; Result of partially or completely applying a production:
; A sequence of 0 or more parse nodes, and a sequence containing
; the remaining input tokens.
(defrecord ParseResult [nodes tokens])

; ------------------------------------------------------------
; Functions
; ------------------------------------------------------------

; Create an initial ParseResult.
; Useful for beginning a production (as part of a call to
; apply-production).
;
; Parameters:
;   token-seq - the input token sequence
;
; Returns: an initial (empty) ParseResult with the given input
; token sequence.
;
(defn initial-parse-result [token-seq]
  (ParseResult. [] token-seq))

; Create a symbol application function to expect and consume a particular
; type of token (i.e., a terminal symol).
;
; Parameters:
;   expected-token-type - a keyword specifying the expected token
;                         type (e.g., :identifier)
;
; Returns: a symbol application function which takes an input tokens
; sequence and returns a SingleParseResult with the resulting
; terminal parse node and the remaining input tokens.
;
(defn expect [expected-token-type]
  (fn [token-seq]
    ; Check to see if there are more tokens
    (if (empty? token-seq)
      ; No more tokens
      (throw (RuntimeException. "Unexpected end of input"))
      ; Check to see if the next token has the expected type
      (let [[lexeme token-type] (first token-seq)]
        (if (not (= token-type expected-token-type))
          ; Wrong token type seen
          (throw (RuntimeException. (str "Expected " expected-token-type ", saw " token-type)))
          ; Consume the token and return a SingleParseResult
          (SingleParseResult. (node/make-node token-type lexeme) (rest token-seq)))))))

; Apply a production (or part of a production) by expanding
; symbols on the right-hand side of a production.
;
; Parameters:
;   parse-result - the ParseResult to which the expanded symbols
;                  should be added; it also contains the current token sequence
;   rhs          - is a sequence of symbol application functions (corresponding
;                  to the symbols on the right hand side of the production being
;                  applied)
;
; Returns: a ParseResult containing the parse nodes resulting from
; applying the symbol application functions, and the remaining
; input tokens.
;
(defn apply-production [parse-result rhs]
  (loop [result parse-result
         symbol-application-functions rhs]
    (if (empty? symbol-application-functions)
      ; Done, no more symbol application functions to apply
      result
      ; Apply the next symbol application function and continue
      (let [symbol-application-function (first symbol-application-functions)
            single-parse-result (symbol-application-function (:tokens result))]
        (recur (ParseResult. (conj (:nodes result) (:node single-parse-result)) (:tokens single-parse-result))
               (rest symbol-application-functions))))))

; Complete a production by creating a SingleParseResult from a
; ParseResult, labeling it with a specified nonterminal symbol.
;
; Parameters:
;    nonterminal  - a keyword specifying the nonterminal symbol (e.g., :statement_list) 
;    parse-result - a ParseResult containing the results of applying zero or
;                   more symbol application functions
;
; Returns: a SingleParseResult
;
(defn complete-production [nonterminal parse-result]
  (SingleParseResult. (node/make-node nonterminal (:nodes parse-result)) (:tokens parse-result)))

; Perform a complete production, returning a SingleParseResult
; as a result.
;
; Parameters:
;   nonterminal - the left-hand nonterminal symbol of the production
;   rhs         - a sequence of symbol application functions (i.e., the right-hand
;                 side of the production)
;   token-seq   - the token sequence to parse
;
; Returns: a SingleParseResult
;
(defn do-production [nonterminal rhs token-seq]
  (complete-production nonterminal (apply-production (initial-parse-result token-seq) rhs)))

; Get the next token from a token sequence, throwing
; a RuntimeException if the token sequence is empty.
;
; Parameters:
;   token-seq - the input token sequence
;
; Returns: the next token, which is a vector containing the lexeme
; and token type, (e.g., ["foobar" :identifier])
;
(defn next-token [token-seq]
  (if (empty? token-seq)
    (throw (RuntimeException. "Unexpected end of input"))
    (first token-seq)))

; ------------------------------------------------------------
; Precedence climbing (for parsing infix expressions)
; ------------------------------------------------------------

(def precedence
  {:op_assign 0,
   :op_plus 1,
   :op_minus 1,
   :op_mul 2,
   :op_div 2,
   :op_exp 3})

(def associativity
  {:op_assign :right,
   :op_plus :left,
   :op_minus :left,
   :op_mul :left,
   :op_div :left,
   :op_exp :right})

(defn is-operator? [[lexeme token-type]]
  (contains? precedence token-type))

(def primary-expression-types #{:identifier :int_literal :str_literal})

(declare parse-expression)

; Parse a primary expression
; TODO: handle parenthesized expressions
;
; Parameters:
;    token-seq - the input token sequence
;
; Returns: a SingleParseResult containing the primary expression
; and the remaining input token sequence.
;
(defn parse-primary [token-seq]
  (let [[lexeme token-type] (next-token token-seq)]
    ; Handle other kinds of primary expressions (identifiers, literals)
    (case token-type
      :lparen (do-production :primary
                             [(expect :lparen) parse-expression (expect :rparen)] token-seq)
      (if (contains? primary-expression-types token-type)
        (do-production :primary [(expect token-type)] token-seq)
        (throw (RuntimeException. (str "Invalid token " token-type " looking for primary expression")))))
    )
  )

; This is adapted more or less directly from the wikipedia pseudo code:
;   http://en.wikipedia.org/wiki/Operator-precedence_parser

(defn need-recursive-parse? [[op-lexeme op-token-type] token-seq]
  ; Check whether
  ;   "the next token is a binary operator whose precedence is greater
  ;   than op's, or a right-associative operator whose precedence is equal to op's"
  (if (or (empty? token-seq)
          (not (is-operator? (first token-seq))))
    ; Either we've reached the end of the input token sequence,
    ; or the next token isn't an operator.
    false
    ; Check the precedence and associativity of the next token.
    (let [[next-lexeme next-token-type] (first token-seq)
          op-prec (op-token-type precedence)
          next-prec (next-token-type precedence)
          next-assoc (next-token-type associativity)]
      (or (> next-prec op-prec)
          (and (>= next-prec op-prec) (= :right next-assoc))))))

(declare parse-expression-1)

(defn parse-rhs [op rhs-result token-seq]
  (let [token-seq (:tokens rhs-result)]
    (if (not (need-recursive-parse? op token-seq))
      ; We can continue at the same precedence level,
      ; so rhs is fine as-is
      rhs-result
      ; Recursive parsing is needed at a higher precedence level
      ; (or we encountered a right-associative operator).
      (let [[lookahead-lexeme lookahead-token-type] (first token-seq)
            lookahead-prec (lookahead-token-type precedence)]
        (parse-expression-1 rhs-result lookahead-prec token-seq)))))

(defn parse-expression-1 [init-lhs-result min-precedence init-token-seq]
  (loop [lhs-result init-lhs-result
         token-seq init-token-seq]
    (if (or (empty? token-seq) (not (is-operator? (first token-seq))))
      ; Done, return lhs SingleParseResult
      lhs-result
      ; Get op and parse next primary expression
      (let [op (first token-seq)
            next-primary-result (parse-primary (rest token-seq))
            ; Parsing the rhs is done in a separate function,
            ; which may involve recursive calls to parse-expression-1.
            rhs-result (parse-rhs op next-primary-result (:tokens next-primary-result))]
        ; Combine lhs and rhs and continue parsing at the same precedence level
        (recur (SingleParseResult. (node/make-node (get op 1) [(:node lhs-result) (:node rhs-result)]) (:tokens rhs-result))
               (:tokens rhs-result))))))

(defn parse-expression [token-seq]
  (let [lhs-result (parse-primary token-seq)]
    (parse-expression-1 lhs-result 0 (:tokens lhs-result))))

; ------------------------------------------------------------
; Parse functions
; ------------------------------------------------------------

; Forward declaration of parse-statement-list.
; (This will be helpful when implementing parse functions for
; if_statement and while_statement nonterminals,
; which need to parse a statement_list recursively.)
(declare parse-statement-list)

; Parse a variable declaration statement.
;
; Parameters:
;   token-seq - the input token sequence
;
; Returns: a SingleParseResult with the :var_decl_statement parse node
; and the remaining input token sequence.
;
(defn parse-var-decl-statement [token-seq]
  ; var_decl_statement -> ^ var identifier ;
  (do-production :var_decl_statement
                 [(expect :var) (expect :identifier) (expect :semicolon)]
                 token-seq))

(defn parse-if-statement [token-seq]
 (do-production :if_statement [(expect :if) (expect :lparen) parse-expression (expect :rparen) (expect :lbrace) parse-statement-list (expect :rbrace)] token-seq
 ))
(defn parse-while-statement [token-seq]
 (do-production :while_statement [(expect :while) (expect :lparen) parse-expression (expect :rparen) (expect :lbrace) parse-statement-list (expect :rbrace)] token-seq
 ))


; Parse an expression statement.
;
; Parameters:
;   token-seq - the input token sequence
;
; Returns: a SingleParseResult with the :expression_statement parse node
; and the remaining input token sequence.
;
(defn parse-expression-statement [token-seq]
  (do-production :expression_statement [parse-expression (expect :semicolon)] token-seq))

; Parse a statement.
;
; Parameters:
;   token-seq - the input token sequence
;
; Returns: a SingleParseResult with the :statement parse node
; and the remaining input token sequence.
;
(defn parse-statement [token-seq]
  (let [[lexeme token-type] (next-token token-seq)]
    (case token-type
      ;statement -> if(expression){statement_list}
      :if (do-production :statement [parse-if-statement] token-seq)
     
      ;statement -> while(expression){statement_list}
      :while (do-production :statement [parse-while-statement] token-seq) 
      ; statement -> ^ var_decl_statement
      :var (do-production :statement [parse-var-decl-statement] token-seq)
      ; statement -> ^ expression_statement
      (do-production :statement [parse-expression-statement] token-seq))))

; Parse a statement list.
;
; Parameters:
;   token-seq - the input token sequence
;
; Returns: a SingleParseResult with the :statement_list parse node
; and the remaining input token sequence.
;
(defn parse-statement-list [token-seq]
  ; The possibilities are:
  ;   statement_list -> ^ statement
  ;   statement_list -> ^ statement statement_list
  ; We'll start by just parsing a statement.
  (let [statement-result (apply-production (initial-parse-result token-seq) [parse-statement])
        remaining-tokens (:tokens statement-result)]
    ; Did we reach the end of the input token sequence?
    (if (empty? (:tokens statement-result))
      ; End of input
      ; statement_list -> statement ^
      (complete-production :statement_list statement-result)
      (let [[lexeme token-type] (first remaining-tokens)]
        ; If the next token is a right brace, then end the production
        (if (= :rbrace token-type)
          ; statement_list -> statement ^
          (complete-production :statement_list statement-result)
          ; There is more input, so recursively parse another statement list
          ; statement_list -> statement ^ statement_list
          (complete-production :statement_list (apply-production statement-result [parse-statement-list])))))))

; Parse a unit.
;
; Parameters:
;    token-seq - the input token sequence
;
; Returns: a SingleParseResult containing the :unit parse node and the
; remaining input token sequence.
;
(defn parse-unit [token-seq]
  ; unit -> ^ statement_list
  (do-production :unit [parse-statement-list] token-seq))

; Top-level parse function. Returns a single node resulting from
; parsing the given token sequence.
(defn parse [token-seq]
  (:node (parse-unit token-seq)))

; ------------------------------------------------------------
; Testing
; ------------------------------------------------------------

; This is a handy way to test your parser: define a string containing
; input, create a token sequence using a lexer, and then call the
; parse function to parse the token sequence.
;
; Just uncomment the two lines below (def testprog ...) and (def prog ...)
;
; Suggestion: pretty print the result of the parse using
;
;    (pp/pretty-print prog)

(def testprog "if (x) { y := z*3; }")
(def tok-seq (lexer/token-sequence (lexer/create-lexer (java.io.StringReader. testprog))))
(def prog (parse tok-seq))
