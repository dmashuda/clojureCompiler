(ns minilang.analyzer
  (:require [minilang.lexer :as lexer])
  (:require [minilang.node :as node])
  (:require [minilang.parser2 :as p])
  (:require [minilang.prettyprint :as pp])
  (:require [minilang.astbuilder :as ast])
  (:require [minilang.scope :as s]))

; Convert a regular Node into an augmented node,
; adding the properties specified in the given map.
(defn to-augmented-node-with-props [node props]
  (node/make-node-with-props (:symbol node) (:value node) (merge (:props node) props)))

; Create an augmented node with a specified register number.
(defn add-regnum [node regnum]
  (to-augmented-node-with-props node {:regnum regnum}))

(declare augment-ast)

; Get a variable name from a var_decl_statement AST node
(defn get-varname [var-decl]
  (:value (node/get-child var-decl 0)))

(defn augment-statement-list [ast scope]
  ;;(println "; Entering scope " (s/scope->string scope))
  (loop [cur-scope scope
         nodes (:value ast)
         augmented-nodes []
         ; Keep track of the maximum number of locals defined in any nested scope
         max-nested-locals 0]
    (if (empty? nodes)
      ; All AST nodes in the statement list have been augmented,
      ; so we're done.  Mark the augmented statement_list with an
      ; :nlocals property that counts the number of locals defined
      ; directly in the statement list, as well as the maximum
      ; number of locals defined in any nested scope.
      (node/make-node-with-props :statement_list
                                 augmented-nodes
                                 (merge (:props ast) {:nlocals (+ (s/get-num-locals cur-scope) max-nested-locals)}))
      
      ; There is at least one remaining AST node.  Augment it and
      ; continue recursively.
      (let [stmt (first nodes)
            stmt-symbol (:symbol stmt)
            ; We add a special marker property, :last => true, to the last
            ; statement in a statement list, but only for the top-level scope.
            last-stmt-marker (if (and (= (:depth cur-scope) 0) (empty? (rest nodes)))
                               {:last true}
                               {})]
        ;;(println "; last-stmt-marker is " last-stmt-marker)
        (cond
          ; For variable declaration statements, add the variable to the
          ; current scope, augment the AST node with the register number
          ; of the variable, and continue in the updated scope.
          (= stmt-symbol :var_decl_statement)
            (let [varname (get-varname stmt)
                  updated-scope (s/add-var cur-scope varname)
                  regnum (s/lookup-regnum updated-scope varname)
                  augmented-stmt (node/add-props (add-regnum stmt regnum) last-stmt-marker)]
              (recur updated-scope (rest nodes) (conj augmented-nodes augmented-stmt) max-nested-locals))
          
          ; For if and while statements, recursively augment the condition
          ; expression in the current scope, create a nested scope (for
          ; the statement list inside the body of the if or while
          ; statement), recursively augment the body statement list
          ; in the nested scope, and continue in the current scope.
          (or (= stmt-symbol :if_statement) (= stmt-symbol :while_statement))
            (let [condition (node/get-child stmt 0)
                  augmented-condition (augment-ast condition cur-scope)
                  nested-scope (s/push-scope cur-scope)
                  child-statement-list (node/get-child stmt 1)
                  augmented-child-statement-list (augment-statement-list child-statement-list nested-scope)
                  num-locals-child-statement-list (node/get-prop augmented-child-statement-list :nlocals)
                  augmented-stmt-1 (node/make-node stmt-symbol [augmented-condition augmented-child-statement-list])
                  augmented-stmt (node/add-props augmented-stmt-1 last-stmt-marker)]
              (recur cur-scope
                     (rest nodes)
                     (conj augmented-nodes augmented-stmt)
                     ; Determine whether the nested statement list defines
                     ; more local variables than the current maximum.
                     (max max-nested-locals num-locals-child-statement-list)))
          
          ; For expression statements, recursively augment and continue in
          ; the current scope.
          :else
            (let [augmented-stmt (node/add-props (augment-ast stmt cur-scope) last-stmt-marker)]
              (recur cur-scope (rest nodes) (conj augmented-nodes augmented-stmt) max-nested-locals)))))))

(defn augment-ast [ast-node scope]
  (case (:symbol ast-node)
    ; Statement lists: use augment-statement-list
    :statement_list (augment-statement-list ast-node scope)
    ; Literals don't require any modification
    :int_literal ast-node
    :str_literal ast-node
    ; Identifiers are variable references! Annotate with register number.
    :identifier (add-regnum ast-node (s/lookup-regnum scope (:value ast-node)))
    ; All other nodes: recursively augment children.
    (let [augmented-children (doall (map (fn [n] (augment-ast n scope)) (node/children ast-node)))]
      (node/make-node-with-props (:symbol ast-node) augmented-children (:props ast-node)))))
                 
;(def testprog "var a; var b; var c; a := 3; b := 4;  c := a*b;")
(def testprog "var a; a := 4 * 5; a;")
;(def testprog
;"var max;
;var count;
;var sum;
;max := 5;
;count := 1;
;sum := 0;
;while (count <= max) {
;    var t;
;    t := sum + count;
;    sum := t;
;    count := count + 1;
;}
;sum;"
;)
(def parse-tree (p/parse (lexer/token-sequence (lexer/create-lexer (java.io.StringReader. testprog)))))
(def ast (ast/build-ast parse-tree))
(def aast (augment-ast ast (s/create-scope)))

