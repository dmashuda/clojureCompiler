(ns minilang.scope)

; Scope data structure:
; - locals is a map of variable names to register numbers
; - depth is the depth of the scope: 0 for the outermost scope, etc.
; - parent is the parent Scope.
;
(defrecord Scope [locals depth parent])

; Create a new outermost scope.
;
; Returns: a Scope
;
(defn create-scope []
  (Scope. {} 0 nil))

; Push a nested scope.
;
; Parameters:
;   parent - the parent Scope
;
; Returns: a nested Scope (with the given parent as its parent)
;
(defn push-scope [parent]
  (let [new-scope (Scope. {} (+ (:depth parent) 1) parent)]
    ;(println (str "new scope: " (scope->string new-scope)))
    new-scope))

; Determine whether a named variable is defined locally in the
; given scope.
;
; Parameters:
;   scope   - a Scope
;   varname - the name of a variable
;
; Returns: true if the variable is defined locally in the scope,
; false otherwise
;
(defn is-defined-in-current-scope? [scope varname]
  (contains? (:locals scope) varname))

; Look up the register number of a variable.
; The search starts at the named scope, and (if necessary)
; continues recursively in the parent scope.
;
; Parameters:
;   scope - the Scope where the search should start (i.e., the current scope)
;   varname - the name of the variable to look up
;
; Returns: the register number of the variable (a RuntimeException
; is thrown if the variable is not found in any scope)
;
(defn lookup-regnum [scope varname]
  ;(println (str "Look up " varname " in " (scope->string scope)))
  (cond
    ; If the search has gone beyond the outermost scope,
    ; then the variable isn't defined anywhere.
    (nil? scope) (throw (RuntimeException. (str "Unknown variable: " varname)))
    ; If the variable is defined in this scope, then we're done.
    (is-defined-in-current-scope? scope varname) (get (:locals scope) varname)
    ; Variable isn't defined in this scope: try looking it up in the parent scope.
    :else (recur (:parent scope) varname)))

; Get the number of local variables declared in the given scope.
; Does not count variables declared in outer scopes.
;
; Parameters:
;   scope - a scope
;
; Returns: the number of local variables defined in the scope.
;
(defn get-num-locals [scope]
  (count (:locals scope)))

; Get the total number of variables declared in this scope and
; all outer scopes.
;
; Parameters:
;   scope - a scope
;
; Returns: the total number of variables declared in this scope
; and all outer scopes.
;
(defn get-num-variables-all-scopes [scope]
  (loop [s scope
         total 0]
   (if (nil? s)
     total
     (recur (:parent s) (+ total (get-num-locals s)))) ))
    
; Define a new variable in the current scope.
;
; Parameters:
;   scope   - the current scope
;   varname - name of the variable to define
;
; Returns: a modified Scope with the same depth and parent scope,
; but with the new variable defined (a RuntimeException is thrown
; if there is already a variable with the same name defined in the
; scope)
;
(defn add-var [scope varname]
  (if (is-defined-in-current-scope? scope varname)
    (throw (RuntimeException. (str "Attempt to redefine local variable " varname)))
    ; The new register number is the count of previously-defined variables.
    ; This ensures that each variable gets a distinct variable number,
    ; starting at 0.  It also naturally allows registers to be reused
    ; when a scope ends.
    (let [regnum (get-num-variables-all-scopes scope)]
      (Scope. (assoc (:locals scope) varname regnum) (:depth scope) (:parent scope))))) 

(defn scope->string [scope]
  (if (nil? scope)
    "nil"
    (str "{ depth=" (:depth scope) ", locals=" (:locals scope) ", parent=" (scope->string (:parent scope)) " }") ))
