(ns minilang.astbuilder
  (:require [minilang.lexer :as lexer])
  (:require [minilang.parser2 :as p])
  (:require [minilang.prettyprint :as pp])
  (:require [minilang.node :as node]))

; Make a new AST node.
;
; Parameters:
;   symbol - the AST node type
;   nodes  - the value of the AST node, which must be a sequence
;            containing child nodes
;
; Returns: the AST node.
;
(defn make-node [symbol nodes]
  (if (not (sequential? nodes))
    (throw (RuntimeException. "non-sequence passed as second param of make-node"))
    (node/make-node symbol nodes)))

; Get a sequence containing the child nodes of given parse node.
;
; Parameters:
;    node - a parse node
;
; Returns: sequence containing the children of the parse node.
;
(defn children [node]
  (if (not (vector? (:value node)))
    (throw (RuntimeException. "Getting children of terminal node"))
    (:value node)))

; Get the nth child node of given parse node.
;
; Parameters:
;    node - a parse node
;    n     - the index of the child to get (0 for the first child, 1 for the
;            second child, etc.)
;
; Returns: the nth child
;
(defn get-child [node n]
  (nth (children node) n))

; Forward declaration for the build-ast function, useful if you
; define any helper functions that need to call build-ast.
;
(declare build-ast)


(defn has-one-child [node]
  ( = (count (children node)) 1))


; Collect all statements from given statement list and add them to
; the accumulator vector, returning the accumulator when there are
; no more statements to collect.
(defn get-grandchildren [node acc]
  (let [statement (get-child node 0)
        updated-acc (conj acc statement)]
    (if (has-one-child node)
      updated-acc
      (recur  (get-child node 1)  updated-acc))))
;      we're not done, continue recursively in child statement list
  

; Flatten a :statement_list node.
; The resulting AST node should have the ASTs for the statements
; that are part of the statement list as immediate children.
;
; Parameters:
;   node - a :statement_list parse node
;
; Returns: the statement list as an AST, with child statements
; as immediate children
;
(defn flatten-statement-list [node]
   ;(make-node (:symbol node) (map build-ast (get-grandchildren node))))
   (make-node (:symbol node) (map build-ast (get-grandchildren node []))))

; Returns an AST node whose symbol is the same as the parse node,
; and whose children are ASTs constructed from the children of the
; parse node.
;
; Parameters:
;    node - a parse node
;
; Returns: an AST node (as described above)
;
(defn recur-on-children [node]
  (make-node (:symbol node) (map build-ast (children node))))

; Build an Abstract Syntax Tree (AST) from the specified
; parse tree.
;
; Parameters:
;    node - a parse tree
;
; Returns: the abstract syntax tree representation of the parse tree.
;
(defn build-ast [node]
  (case (:symbol node)
    :unit (recur-on-children node)
    :statement_list (flatten-statement-list node)
    :statement  (build-ast (get-child node 0))
    :while_statement (make-node (:symbol node) (map build-ast [(get-child node 2)(get-child node 5)]))
    :if_statement (make-node (:symbol node) (map build-ast [(get-child node 2)(get-child node 5)]))
    :expression_statement (make-node (:symbol node)[(build-ast (get-child node 0))])
    :op_mul (recur-on-children node)
    :op_plus (recur-on-children node)
    :op_div (recur-on-children node)
    :op_sub (recur-on-children node)
    :op_assign (recur-on-children node)
    :var_decl_statement (make-node (:symbol node) (build-ast [(get-child node 1)]))
    :primary (if (has-one-child node)
               (build-ast (get-child node 0))
               (build-ast (get-child node 1)))
    
     
    ; The default case just leaves the parse node unchanged.
    ; This is the correct behavior for identifiers, int literals,
    ; and string literals
    node))

; ----------------------------------------------------------------------
; Testing
; ----------------------------------------------------------------------

;(def testprog "var a; a := 3*4;")
(def testprog "a * (b + 3);")
;(def testprog "while (a + b) { c; d*e*4; }")
;(def testprog "if (x+b) { y := z*3; }")

(def parse-tree (p/parse (lexer/token-sequence (lexer/create-lexer (java.io.StringReader. testprog)))))
(def ast (build-ast parse-tree))
;(pp/pretty-print ast)