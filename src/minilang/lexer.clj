(ns minilang.lexer)

(defrecord Lexer [lineseq line tok])

; Regular expressions defining the token types
(def token-patterns
  [; Keywords
   [#"^var" :var]
   [#"^func" :func]
   [#"^if" :if]
   [#"^while" :while]

   ; Identifiers   
   [#"^[A-Za-z_][A-Za-z0-9_]*" :identifier]

   ; Literals
   [#"^\"(\\\"|\\\\|[^\\\\\"])*?\"" :str_literal]
   [#"^[0-9]+" :int_literal]

   ; Operators
   [#"^:=" :op_assign]
   [#"^\+" :op_plus]
   [#"^-" :op_minus]
   [#"^\*" :op_mul]
   [#"^/" :op_div]
   [#"^\^" :op_exp]
   
   ; Punctuation
   [#"^;" :semicolon]
   [#"^\(" :lparen]
   [#"^\)" :rparen]
   [#"^\{" :lbrace]
   [#"^\}" :rbrace]
   ])

; Attempt to ensure that the lexer has a line available
(defn fill-line [lexer]
  (cond
    ; If there is already a line, there's nothing to do
    (not (empty? (:line lexer))) lexer
    ; If there are no more input lines, then set line to nil (to signal EOF)
    (empty? (:lineseq lexer)) (assoc lexer :line nil)
    ; Get the next line
    :else (Lexer. (rest (:lineseq lexer)) (first (:lineseq lexer)) (:tok lexer))))

(defn recognize-token [lexer]
  ; Trim leading whitespace
  (let [line (clojure.string/triml (:line lexer))]
    ; Attempt to match each token pattern in sequence
    (loop [patterns token-patterns]
      ; If there are no more patterns, then the input contains an illegal token
      (if (empty? patterns)
        (throw (RuntimeException. (str "Illegal token at " line)))
        (let [[token-regexp token-type] (first patterns)
              match (re-find token-regexp line)]
          (if match
            ; Current pattern is a match: update lexer's line and token
            (let [match-text (if (vector? match) (get match 0) match)
                  rest-of-line (subs line (count match-text))]
              (assoc lexer :line rest-of-line :tok [match-text token-type]))
            ; Current pattern is not a match: try next pattern
            (recur (rest patterns))))))))

; Attempt to ensure that the lexer has the next token available
(defn fill-token [lexer]
  (cond
    ; If there is already a token, there's nothing to do
    (not (nil? (:tok lexer))) lexer
    ; If there are no more lines, then leave tok nil (to signal end of stream)
    (nil? (:line lexer)) lexer
    ; If current line is empty, read another line and recur
    (empty? (clojure.string/trim (:line lexer))) (recur (fill-line lexer))
    ; A nonempty line is available: call recognize-token
    :else (recognize-token lexer)))

; Create a lexer to read from given source of input
(defn create-lexer [in]
  (let [the-lexer (Lexer. (line-seq (java.io.BufferedReader. in)) "" nil)]
    (fill-token the-lexer)))

; Create a "raw" lexer (which is positioned at the beginning on input,
; but hasn't had fill-token called on it.)
(defn create-raw-lexer [in]
  (let [the-lexer (Lexer. (line-seq (java.io.BufferedReader. in)) "" nil)]
    the-lexer))

; Determine whether lexer is at end of file.
(defn at-eof [lexer]
  (nil? (:tok lexer)))

; Get the current token.
(defn get-current-token [lexer]
  (:tok lexer))

; Consume current token, returning updated lexer.
(defn consume-token [lexer]
  (fill-token (assoc lexer :tok nil)))

; Use given lexer to create a lazy sequence of tokens.
; This is the recommended way of using a lexer.
(defn token-sequence [lexer]
  (if (at-eof lexer)
    []
    (let [first-token (get-current-token lexer)
          advanced-lexer (consume-token lexer)]
      (cons first-token (lazy-seq (token-sequence advanced-lexer))))))

;;;(def thingy "\"Hello, \\\"Cruel\\\" world.\\\\Yeah\"")
;;;(def thingy2 "\"Hello, \\\"Cruel\\\" world. Yeah\"")

;;;(def aplusb "a + b;")
;;;(def aplusb-tokens (token-sequence (create-lexer (java.io.StringReader. aplusb))))
