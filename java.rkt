#lang racket

(require "read.rkt"
         racket/pretty)

;; java expand ast
;;
;; statement = 
;; (class name superclass (interface ...)
;;   (field name type expr)
;;   (method name (arg ...) body:statement ...))
;; 
;; (var name type expr)
;; expr
;;
;; expr = ...
;; (call expr name arg:expr ...)
;; (store expr expr)
;; (if expr expr expr)

(define (java-read-syntax input)
  (with-input-from-string input
    (lambda ()
      (honu-read-syntax))))

(struct parsed (data) #:transparent)
(struct operator (precedence association binary unary postfix?))
(struct java-class (inside))
(struct java-macro (function))

(define-syntax-rule (define-java-macro (name arg ...) body ...)
                    (define name (java-macro (lambda (arg ...) body ...))))

(define (environment-value environment name)
  (hash-ref environment name (lambda () #f)))

(define (is-operator? what environment)
  (operator? (environment-value environment what)))

(struct pattern-output [pattern-var value rest next])

(define (combine-output output1 output2)
  (cond
    [(not output1) output2]
    [(not output2) output1]
    [else 
      (when (not (null? (pattern-output-rest output1)))
        (error 'combine-output "pattern 1 output should be empty" (pattern-output-rest output1)))
      (pattern-output (pattern-output-pattern-var output2)
                      (pattern-output-value output2)
                      (pattern-output-rest output2)
                      output1)]))

(define (parse-type input environment)
  (match input
    [(list (and type (? (lambda (i) (type? i environment)))) more ...)
     (values type more)]))

(define (interpret-pattern pattern input environment)
  (debug "Interpret pattern ~a\n" pattern)
  (match pattern
    [(list) #f]
    [(list (and (? symbol?) x) '%colon 'id rest ...)
     (match input
       [(list (and id (? id?)) input-rest ...)
        (combine-output (pattern-output x id '() #f)
                        (interpret-pattern rest input-rest environment))])]
    [(list (and (? symbol?) x) '%colon 'type rest ...)
     (define-values (parsed unparsed)
                    (parse-type input environment))
     (combine-output (pattern-output x parsed '() #f)
                     (interpret-pattern rest unparsed environment))]
    [(list (list '#%parens pattern-inside ...) pattern-more ...)
     (match input
       [(list (list '#%parens input-inside ...) input-more ...)
        (combine-output
          (interpret-pattern pattern-inside input-inside environment)
          (interpret-pattern pattern-more input-more environment))])]
    [(list (and (? symbol?) x) '%colon 'expression rest ...)
     (define-values (parsed unparsed) (enforest-java input environment))
     (combine-output
       (pattern-output x parsed '() #f)
       (interpret-pattern rest unparsed environment))]
    [(list (and (? symbol?) id) rest ...)
     (define-values (parsed unparsed) (values (car input) (cdr input)))
     (combine-output
       (pattern-output id parsed '() #f)
       (interpret-pattern rest unparsed environment))]
    ))

(define (replace input output)
  (if output
    (let ()
      (define var (pattern-output-pattern-var output))
      (define value (pattern-output-value output))
      (define new-input
        (match input
          [(? (lambda (in) (eq? in var))) value]
          [(struct parsed (inside))
           (parsed (replace inside output))]
          [(or (? symbol?) (? number?)) input]
          [(list x ...)
           (for/list ([x x])
             (replace x output))]))
      (replace new-input (pattern-output-next output)))
    input))

(define (project-output body output)
  (match body
    [(list 'syntax (list '#%parens template ...))
     (replace template
              output)]))

(define-java-macro (create-java-macro stx environment)
  (match stx
    [(list id (list '#%parens literal ...)
           (list '#%braces pattern ...)
           (list '#%braces body ...)
     rest ...)
     (define macro (java-macro (lambda (stx environment)
                                 (debug "Java macro with ~a\n" stx)
                                 (define output (interpret-pattern pattern stx environment))
                                 (define template (project-output body output))
                                 (values template (pattern-output-rest output)))))
     (values (parsed `(macro ,id ,macro))
             rest)]))
      
(define (is-java-macro? name environment)
  (debug "Java macro ~a in ~a? ~a\n" name environment (java-macro? (environment-value environment name)))
  (java-macro? (environment-value environment name)))

(define (id? x)
  (and (symbol? x)
       (not (or (operator? x)
                (member x '(= #%parens #%braces #%brackets < >))
                (member x '(public private protected))))))

(define (type? x environment)
  (and (id? x)
       (eq? (environment-value environment x) 'type)))

(define (binary-operator precedence association binary [unary #f] [postfix? #f])
  (operator precedence association binary unary postfix?))

(define (unary-operator precedence unary [postfix? #f])
  (operator precedence 'left #f unary postfix?))

(define java-! (unary-operator 5 (lambda (what)
                                   (parsed `(unary-op ! ,@what))))) 

(define java-+ (binary-operator 1 'left (lambda (left right)
				   (parsed `(op + ,@left ,@right)))))
(define java-- (binary-operator 1 'left (lambda (left right)
				   (parsed `(op - ,@left ,@right)))))
(define java-* (binary-operator 2 'left (lambda (left right)
				   (parsed `(op * ,@left ,@right)))))
(define java-/ (binary-operator 2 'left (lambda (left right)
				   (parsed `(op / ,@left ,@right)))))

(define java-= (binary-operator 0.1 'left (lambda (left right)
                                            (parsed `(assign ,@left ,@right)))))

(define java-+= (binary-operator 0.1 'left (lambda (left right)
                                            (parsed `(assign+ ,@left ,@right)))))

(define java-dot (binary-operator 100 'left (lambda (left right)
                                              (parsed `(dot ,@left ,@right)))))

(define java-== (binary-operator 0.5 'left (lambda (left right)
                                             (parsed `(op == ,@left ,@right)))))

(define java-< (binary-operator 0.2 'left (lambda (left right)
                                            (parsed `(op < ,@left ,@right)))))

(define java-++ (unary-operator 3 (lambda (what)
                                    (parsed `(postfix-op ++ ,@what)))
                                #t))

#;
(define (operator-binary-transformer operator)
  (case operator
    [(=) (lambda (left right)
           (parsed `(assign ,@left ,@right)))]
    [(+=) (lambda (left right)
            (parsed `(assign+ ,@left ,@right)))]
    [(!= == < - + *) (lambda (left right)
            (parsed `(op ,operator ,@left ,@right)))]
    [(%dot) (lambda (left right)
              (parsed `(dot ,@left ,@right)))]
    [else #f]))

#;
(define (operator-unary-transformer operator)
  (case operator
    [(! -) (lambda (left)
           (parsed `(unary-op ,operator ,@left)))]
    [else #f]))

(define cast-precedence 10)
(define function-call-precedence 99)
#;
(define (operator-precedence operator)
  (case operator
    [(= +=) 0.1]
    [(== !=) 0.2]
    [(<) 0.15]
    [(%dot) 100]
    [(+ -) 1]
    [(* /) 2]
    [(!) 5]
    [else (error 'precedence "unknown precedence for operator ~a" operator)]
    ))

(define (parse-all stuff environment)
  #;
  (enforest-java stuff)
  (let loop ([all '()]
             [more stuff])
    (if (or (null? more) (not more))
      (reverse all)
      (let ()
        (define-values (parsed unparsed)
                       (enforest-java more environment))
        (loop (cons parsed all) unparsed)))))

(define debug printf)
; (define-syntax-rule (debug x ...) (void))

(define (parse-args args environment)
  ;; parse an expression, then maybe a comma, then repeat
  (let loop ([all '()]
             [rest args])
    (define-values (expr1 more) (enforest-java rest environment))
    (if expr1
      (loop (cons expr1 all)
            (match more
              [(list 'honu-comma rest ...) rest]
              [else more]))
      (reverse all))))

(define (enforest-java-class-body body environment)
  (debug "Enforest class body ~a\n" (pretty-format body))
  (match body
    [(list (list '%semicolon (or 'public 'private 'protected)
                 (and (? symbol?) type) (and (? symbol?) var))
           more ...)
     (values `(var ,var ,type) more)]
    [(list (or 'public 'private) (and (? id?) id) (list '#%parens args ...)
           (list '#%braces body ...)
           more ...)
     #;
     (define code (parse-body body environment))
     #;
     (debug "Constructor body ~a\n" (pretty-format code))
     (values `(constructor ,id (java-unparsed-method ,@body)) more)]
    [(list (or 'public 'private) (and (? id?) type)
           (and (? id?) id) (list '#%parens args ...)
           (list '#%braces body ...)
           more ...)
     (values `(method ,id ,type (java-unparsed-method ,@body)) more)]
    [(list (or 'public 'private)
           (and (? symbol?) type) '< (not '>) ... '>
           (and (? symbol?) id) (list '#%parens args ...)
           (list '#%braces body ...)
           more ...)
     (values `(method ,id ,type (java-unparsed-method ,@body)) more)]
    [else (error 'parse-class-body "can't parse ~a" body)]))

(define (enforest-java input environment)
  (define (parse stream precedence left current)
    (debug "Parsing ~a current ~a\n" stream current)
    (match stream
      [#f (values (left current) #f)]
      [(and (? parsed?) x)
       (values (left x) #f)]
      [(list (and (? parsed?) x) rest ...)
       (if current
         (values (left current) stream)
         (parse rest precedence left x))]
      [(list (and (? (lambda (x) (is-java-macro? x environment)) macro)) rest ...)
       (if current
         (values (left current) stream)
         (let ()
           (define function (java-macro-function (environment-value environment macro)))
           (define-values (parsed unparsed) (function rest environment))
           (values parsed unparsed)))]
      [(list) (values (left current) #f)]
      [(list 'honu-comma rest ...)
       (if current
         (values (left current) stream)
         (values (left #f) stream))]
      [(list (list '%semicolon inner ...) more ...)
       ;; there should only be one statement here so just take it out
       (values (first (parse-all inner environment)) more)]

      [(list (and (? (lambda (i)
                       (is-operator? i environment)))
                  operator-symbol) rest ...)
       (define operator (environment-value environment operator-symbol))
       (define new-precedence (operator-precedence operator))
       (define association (operator-association operator))
       (define binary-transformer (operator-binary operator))
       (define unary-transformer (operator-unary operator))
       (define postfix? (operator-postfix? operator))
       (define higher
         (case association
           [(left) >]
           [(right) >=]))

       (if (higher new-precedence precedence)
         (let-values ([(parsed unparsed)
                       (parse rest new-precedence
                              (lambda (stuff)
                                (define right (parse-all stuff environment))
                                (define output
                                  (if current
                                    (if binary-transformer
                                      (binary-transformer (parse-all current environment) right)
                                      (if (and postfix? unary-transformer)
                                        (unary-transformer (list current))
                                        (error 'binary "cannot be used as a binary operator in ~a" operator)))
                                    (if unary-transformer
                                      (unary-transformer right)
                                      (error 'unary "cannot be used as a unary operator in ~a" operator))))

                                output)
                              #f)])
           (parse unparsed precedence left parsed))
         (if unary-transformer
           (if current
             (values (left current) stream)
             (error 'low-precedence-unary "implement"))
           (values (left current) stream)))]

      ;; bug, for should have '#%parens immediately after it
      [(list 'for (list (list '%semicolon '#%parens init-code ...) (list '%semicolon until-code ...) update-code ...) body-code rest ...)
       (if current
         (error 'parse "cannot have anything next to a for loop ~a" current)
         (let ()
           (define init (parse-all init-code environment))
           (define until (parse-all until-code environment))
           (define update (parse-all update-code environment))
           (define body (parse-all body-code environment))
           (values (parsed `(for ,init ,until ,update ,@body)) rest)))]

      [(list 'while (list '#%parens inside ...) (list '#%braces body ...) rest ...)
       (define condition (parse-all inside environment))
       (values (parsed `(while ,condition (java-unparsed-method ,@body)))
               rest)]

      [(list '#%braces code ...)
       (define inner (parse-all code environment))
       (values (parsed `(body ,@inner)) #f)]

      [(list 'if (list '#%parens condition-code ...) (list '#%braces then-code ...) 'else (list '#%braces else-code ...) more ...)
       (debug "If with an else\n")
       (define condition (first (parse-all condition-code environment)))
       (define then (parse-all then-code environment))
       (define else (parse-all else-code environment))
       (values (parsed `(if ,condition (body ,@then) (body ,@else))) more)]

      [(list 'if (list '#%parens condition-code ...) (list '#%braces then-code ...) more ...)
       (debug "If by itself\n")
       (define condition (first (parse-all condition-code environment)))
       (define then (parse-all then-code environment))
       (values (parsed `(if ,condition (body ,@then))) more)]

      [(list (list '#%braces inside ...) rest ...)
       (values (parsed `(java-unparsed-method ,@inside)) rest)]

      [(list 'return rest ...)
       (define returned (first (parse-all rest environment)))
       (values (parsed `(return ,returned)) #f)]

      ;; Type name
      [(list (and (? (lambda (i) (type? i environment))) type) (and (? id?) name))
       (if current
         (error 'enforest-java "cannot have anything next to a variable definition ~a" current)
         (values (parsed `(var ,type ,name)) #f))]

      [(list (and (? (lambda (i) (type? i environment))) type) (list #%brackets)
             (and (? id?) name) '= right-side ...)
       (if current
         (error 'enforest-java "cannot have anything next to a variable definition ~a" current)
         (let ()
           (define right (first (parse-all right-side environment)))
           (values (parsed `(var ,type ,name ,right)) #f)))]

      ;; List<Foo> x = ...
      [(list (and (? (lambda (i) (type? i environment))) type) '< (and (? id?) generic) '> (and (? id?) name) '= right-side ...)
       (if current
         (error 'enforest-java "cannot have anything next to a variable definition ~a" current)
         (let ()
           (define right (first (parse-all right-side environment)))
           (values (parsed `(var ,type ,name ,right)) #f)))]

      ;; List x = ...
      [(list (and (? (lambda (i) (type? i environment))) type)
             (and (? id?) name) '= right-side ...)
       (if current
         (error 'enforest-java "cannot have anything next to a variable definition ~a" current)
         (let ()
           (define right (first (parse-all right-side environment)))
           (values (parsed `(var ,type ,name ,right)) #f)))]

      [(list (list '#%brackets expr ...) more ...)
       (if (not current)
         (error 'enforest-java "must have something next to brackets")
         (let ()
           (define inside (first (parse-all expr environment)))
           (parse more precedence left (parsed `(lookup ,current ,inside)))))]

      [(list (list '#%parens (and (? (lambda (id)
                                       (type? id environment)))
                                  type)) rest ...)
       (debug "Parse cast id ~a current ~a\n" type current)
       (if current
         (let ()
           (values (left current) stream)
           #;
           (parse rest precedence (lambda (right) (left (parsed `(cast ,current ,right ,x)))) #f)

           #;
           (define-values (right more) (enforest-java stream environment))
           #;
           (parse more precedence (lambda (x) (parsed `(cast ,current ,right ,x))) #f))
         (parse rest cast-precedence (lambda (x) (left (parsed `(cast ,type ,x)))) #f)
         #;
         (parse rest precedence left (parsed x)))

       #;
       (if current
         (values (left current) stream)
         (parse rest precedence left (parsed x)))]

      ;; new foo()
      [(list 'new (and (? id?) constructor)
             (list '#%parens args ...)
             rest ...)
       (if current
         (values (left current) stream)
         (let ()
           (define parsed-args (parse-args args environment))
           (define output (parsed `(new ,constructor ,@parsed-args)))
           (parse rest precedence left output)))]

       [(list (and (? id?) x) rest ...)
        (if current
          (values (left current) stream)
          (parse rest precedence left (parsed x)))]

      [(list (list '#%parens args ...) more ...)
       (debug "Function call with ~a at ~a\n" current precedence)
       (if current
         (if (> precedence function-call-precedence)
           (let ()
             (define function (left current))
             (define parsed-args (parse-args args environment))
             (parse more function-call-precedence (lambda (x) x)
                    (parsed `(call ,function ,@parsed-args))))
           (let ()
             (define parsed-args (parse-args args environment))
             (parse more precedence left (parsed `(call ,current ,@parsed-args)))))
         ;; not a function call, just parenthesizing an expression
         (let ()
           (define inner (parse-all args environment))
           (parse more precedence left inner)))]

      [(list (and (? string?) x) rest ...)
       (if current
         (values (left current) rest)
         (parse rest precedence left (parsed x)))]

      [(list (and (? number?) x) rest ...)
       (if current
         (values (left current) rest)
         (parse rest precedence left (parsed x)))]
      [else (error 'parse "don't know how to parse ~a" stream)]))

  (parse input 0 (lambda (x) x) #f))

(define (parse-body body environment)
  (parse-all body environment))

(define (parse-class-body body environment)
  #f
  )

(define (parse-class class class-body environment)
  `(class ,class (java-unparsed-body ,@class-body))
  #;
  (define class-stuff
    (let loop ([all '()]
               [more class-body])
      (if (null? more)
        (reverse all)
        (let ()
          (define-values (parsed unparsed)
                         (parse-class-body more environment))
          (loop (cons parsed all) unparsed)))))
  #;
  `(parsed-class ,class ,@class-stuff))

(define (parse-top-level input environment)
  (match input
    [(list (list '%semicolon 'package rest ...)
           more ...)
     (values `(package ,@rest) more)]
    [(list (list '%semicolon 'import rest ...)
           more ...)
     (values `(import ,@rest) more)]
    [(list 'public 'class (and (? symbol?) class)
           'implements (and (? symbol?) interface)
           (list '#%braces class-body ...)
           more ...)
     ;; FIXME: handle interface
     (values (parse-class class class-body environment)
             more)]
    [(list 'public 'class (and (? symbol?) class) (list '#%braces class-body ...)
           more ...)
     (values (parse-class class class-body environment)
             more)]
    ))

(define (make-string what)
  (format "~a" what)
  #;
  (cond
    [(symbol? what) (symbol->string what)]
    [(number? what) (number->string what)]
    [(string what) what]))

(define (add-tab tab)
  (string-append tab "   "))

(define (unparse-java input tabs [so-far ""])
  (define (package-name what)
    (define strings
      (reverse 
        (for/fold ([name '()])
                  ([component what])
                  (cons (match component
                          ['%dot "."]
                          [x (symbol->string x)])
                        name))))
    (apply string-append strings))
  (debug "Unparse ~a\n" (pretty-format input))
  (match input
    [(list (list)) so-far]
    [(list (list 'package name ...) more)
     (unparse-java more tabs (string-append so-far (format "package ~a;" (package-name name))))]
    [(list (list 'import name ...) more)
     (unparse-java more tabs (string-append so-far (format "\nimport ~a;" (package-name name))))]
    [(list (list 'class name body ...) more ...)
     (unparse-java more tabs (string-append so-far (format "\nclass ~a{\n~a\n~a}" name (unparse-java body (add-tab tabs)) tabs)))]
    [(list (list 'var name type stuff ...) more ...)
     (unparse-java more tabs (string-append so-far (format "\n~aprivate ~a ~a;" tabs type name)))]
    [(list (list 'constructor name body) more ...)
     (unparse-java more tabs
                   (string-append so-far
                                  (format "\n~apublic ~a(){\n~a\n~a}"
                                          tabs name
                                          (unparse-java `(body ,@body) (add-tab tabs))
                                          tabs)))]
    [(list (list 'method name type body) more ...)
     (unparse-java more tabs
                   (string-append so-far
                                  (format "\n~apublic ~a ~a(){\n~a\n~a}\n"
                                          tabs type name
                                          (unparse-java `(body ,@body) (add-tab tabs)) tabs)))]

    [(list 'body (list)) so-far]

    [(list 'body 'begin inside more ...)
     (unparse-java `(body ,inside ,@more)
                   tabs
                   so-far)]

    [(list 'body (list 'begin inside ...))
     (unparse-java `(body ,@inside)
                   tabs
                   so-far)]

    [(list 'body (list 'call obj args ...) more ...)
     (unparse-java `(body ,@more)
                   tabs
                   (string-append so-far
                                  (format "~a~a;\n"
                                          tabs
                                          (unparse-java `(expression (call ,obj ,@args)) tabs))))]
    [(list 'body (list 'assign name expr) more ...)
     (unparse-java `(body ,@more) tabs
                   (string-append so-far (format "~a~a = ~a;\n" tabs
                                                 (unparse-java `(expression ,name) tabs)
                                                 (unparse-java `(expression ,expr) tabs))))]
    [(list 'body (list 'assign+ name expr) more ...)
     (unparse-java `(body ,@more) tabs
                   (string-append so-far (format "~a~a += ~a;\n"
                                                 tabs
                                                 (unparse-java `(expression ,name) tabs)
                                                 (unparse-java `(expression ,expr) tabs))))]
    [(list 'body (list 'return expr) more ...)
     (unparse-java `(body ,@more)
                   tabs
                   (string-append so-far (format "~areturn ~a;"
                                                 tabs
                                                 (unparse-java `(expression ,expr) tabs))))]
    [(list 'expression (list 'new type args ...))
     (string-append so-far (format "new ~a(~a)" type
                                   (apply string-append
                                          (add-between
                                            (for/list ([arg args]) (unparse-java `(expression ,arg) tabs))
                                            ",")))
                                            )]

    [(list 'body (list 'var type name expr) more ...)
     (unparse-java `(body ,@more)
                   tabs
                   (string-append so-far
                                  (format "~a~a ~a = ~a;\n"
                                          tabs
                                          type name
                                          (unparse-java `(expression ,expr) tabs))))]

    [(list 'expression (list 'var type name expr))
     (string-append so-far
                    (format "~a~a ~a = ~a"
                            tabs
                            type name
                            (unparse-java `(expression ,expr) tabs)))]

    [(list 'expression (list 'assign+ name expr))
     (string-append so-far (format "~a += ~a" name (unparse-java `(expression ,expr) tabs)))]

    [(list 'expression (list 'lookup obj expr))
     (string-append so-far (format "~a[~a]"
                                   (unparse-java `(expression ,obj) tabs)
                                   (unparse-java `(expression ,expr) tabs)))]

    [(list 'expression (and (or (? symbol?)
                                (? number?)
                                (? string?))
                            id))
     (string-append so-far (format "~a" id))]
    
    [(list 'body (list 'if condition then) more ...)
     (unparse-java `(body ,@more) tabs (string-append
                                         (format "\n~aif (~a){\n~a\n~a}\n"
                                                 tabs
                                                 (unparse-java `(expression ,condition) tabs)
                                                 (unparse-java then (add-tab tabs))
                                                 tabs)))]

    [(list 'body (list 'if condition then else) more ...)
     (unparse-java `(body ,@more) tabs
                   (string-append (format "\n~aif (~a){\n~a\n~a} else {\n~a\n~a}\n"
                                          tabs
                                          (unparse-java `(expression ,condition) tabs)
                                          (unparse-java then (add-tab tabs))
                                          tabs
                                          (unparse-java else (add-tab tabs))
                                          tabs
                                          )))]

    [(list 'body (list 'while condition body) more ...)
     (unparse-java `(body ,@more) tabs
                   (string-append so-far
                                  (format "\n~awhile (~a){\n~a\n~a}\n"
                                          tabs
                                          (unparse-java `(expression ,@condition) "")
                                          (unparse-java `(body ,@body) (add-tab tabs))
                                          tabs)))]

    [(list 'body (list 'for init condition rest body) more ...)
     (unparse-java `(body ,@more) tabs 
                   (string-append so-far
                                  (format "\n~afor (~a; ~a; ~a){\n~a\n~a}\n"
                                          tabs
                                          (unparse-java `(expression ,@init) "")
                                          (unparse-java `(expression ,@condition) "")
                                          (unparse-java `(expression ,@rest) "")
                                          (unparse-java body (add-tab tabs))
                                          tabs)))]

    [(list 'expression (list 'not more))
     (string-append so-far (format "!(~a)" (unparse-java `(expression ,@more) tabs)))]
    [(list 'expression (list 'unary-op op x))
     (string-append so-far (format "(~a~a)" op (unparse-java `(expression ,x) tabs)))]
    
    [(list 'expression (list 'postfix-op op x))
     (string-append so-far (format "(~a~a)" (unparse-java `(expression ,x) tabs) op))]

    [(list 'body (and x (list 'op blah ...)) more ...)
     (unparse-java `(body ,@more)
                   tabs
                   (string-append so-far (format "~a~a;" tabs (unparse-java `(expression ,x) tabs))))]

    [(list 'body first rest ...)
     (unparse-java `(body ,@rest)
                   tabs
                   (string-append so-far (format "~a~a;" tabs (unparse-java `(expression ,first) tabs))))]

    [(list 'expression (list 'op op a b))
     (string-append so-far (format "(~a ~a ~a)"
                                   (unparse-java `(expression ,a) tabs)
                                   op
                                   (unparse-java `(expression ,b) tabs)))]
    [(list 'expression (list 'cast class expr))
     (string-append so-far (format "(~a) ~a" class (unparse-java `(expression ,expr) tabs)))]

    [(list 'expression (list 'call function args ...))
     (string-append so-far (format "~a(~a)"
                                   (unparse-java `(expression ,function) tabs)
                                   (apply string-append
                                          (add-between (for/list ([arg args])
                                                         (unparse-java `(expression ,arg) tabs))
                                                       ","
                                                       ))))]

    [(list 'expression (list 'dot what field))
     (string-append so-far (format "~a.~a" (unparse-java `(expression ,what) tabs) field))]

    #;
    [(list 'expression (list '#%brackets expression ...))
     (string-append so-far (format "[~a]" (unparse-java `(expression ,@expression))))]

    #;
    [(list 'expression (list 'call (and (? symbol?) obj) method))
     (string-append so-far (format "~a.~a()" obj method))]
    #;
    [(list 'expression (list 'call (list 'dot (list what) (list expr)) args ...))
     (string-append so-far (format "~a.~a(~a)"
                                   what
                                   (unparse-java `(expression ,expr))
                                   (apply string-append
                                          (add-between (for/list ([arg args])
                                                         (unparse-java `(expression ,arg)))
                                                       ","
                                                       ))))]
    [(list 'expression) so-far]
    [(list 'body) so-far]
    [(list) so-far]
    [else (error 'unparse "Could not unparse\n~a\n" (pretty-format input))]
    ;; [else so-far]
    ))

(define (make-environment)
  (make-hash))

(define (copy-environment environment)
  (hash-copy environment))

(define-syntax-rule (add-binding! environment symbol value)
                    (hash-set! environment 'symbol value))

(define (add-binding-name! environment symbol value)
  (hash-set! environment symbol value))

(struct macro (function))
(define-syntax-rule (define-macro (name arg ...) body ...)
                    (define name (macro (lambda (arg ...) body ...))))

(define-macro (java-unparsed-body stx environment)
  (match stx
    [(list) '()]
    [else (let-values ([(parsed unparsed) (enforest-java-class-body stx environment)])
            `(begin ,parsed (java-unparsed-body ,@unparsed)))]))

(define (remove-parsed input)
  (match input
    [(struct parsed (data))
     (remove-parsed data)]
    [(list x ...)
     (for/list ([x x])
       (remove-parsed x))]
    [x x]))

(define-macro (java-unparsed-method stx environment)
  (match stx
    [(list) '()]
    [else (let-values ([(parse unparsed) (enforest-java stx environment)])
            (when (not unparsed)
              (set! unparsed '()))
            (if (parsed? parse)
              (let ([inside (remove-parsed parse)])
                (if (null? unparsed)
                  `(begin ,inside)
                  `(begin ,inside (java-unparsed-method ,@unparsed))))
              (if (null? unparsed)
                `(java-unparsed-method ,@parse)
                `(begin (java-unparsed-method ,@parse)
                        (java-unparsed-method ,@unparsed))))
              )]))

(define-macro (java-unparsed-top stx environment)
  (match stx
    [(list) '()]
    [else (let-values ([(parsed unparsed) (parse-top-level stx environment)])
            `(,parsed (java-unparsed-top ,@unparsed)))]))

(define (is-macro? id environment)
  (define value (environment-value environment id))
  (debug "Macro? ~a in ~a ~a\n" id environment (macro? value))
  (and value (macro? value)))

(define (base-java-environment)
  (define environment (make-environment))
  (add-binding! environment + java-+)
  (add-binding! environment - java-+)
  (add-binding! environment * java-*)
  (add-binding! environment / java-/)
  (add-binding! environment = java-=)
  (add-binding! environment < java-<)
  (add-binding! environment == java-==)
  (add-binding! environment += java-+=)
  (add-binding! environment ! java-!)
  (add-binding! environment ++ java-++)
  (add-binding! environment %dot java-dot)
  (add-binding! environment macro create-java-macro)
  (add-binding! environment java-unparsed-top java-unparsed-top)
  (add-binding! environment java-unparsed-body java-unparsed-body)
  (add-binding! environment java-unparsed-method java-unparsed-method)

  (add-binding! environment int 'type)
  (add-binding! environment void 'type)
  (add-binding! environment List 'type)
  (add-binding! environment Iterator 'type)

  environment)

(define (update-bindings environment name value)
  (define new (copy-environment environment))
  (hash-set! new name value)
  new)

(define (linearize input)
  (match input
    [(list 'begin a)
     (linearize a)]
    [(list 'begin a x)
     (cons (linearize a) (linearize x))]
    [else input]))

(define (expand-java environment code)
  (debug "Expand ~a\n" (pretty-format code))
  (match code
    [(? number?) code]
    [(? string?) code]
    ;; should check that the symbol is bound
    [(? symbol?) code]
     [(list (and symbol? (? (lambda (x) (is-macro? x environment))) id) rest ...)
      (debug "Expand macro ~a\n" id)
      (define macro (environment-value environment id))
      (define output ((macro-function macro) rest environment))
      (expand-java environment output)]

     [(list 'begin x ...)
      (debug "Begin ~a\n" x)
      (define stuff
        (filter values
                (for/list ([x x])
                  (expand-java environment x))))
      `(begin ,@stuff)]

     [(list 'class name body)
      (define new-environment (update-bindings environment name 'type))
      (define body* (linearize (expand-java new-environment body)))
      `(class ,name ,@body*)]

     #;
     [(list (list (and symbol? (? (lambda (x) (is-macro? x environment))) id) rest ...)
            more ...)
      (define macro (environment-value environment id))
      (define output ((macro-function macro) rest environment))
      (define front (expand-java environment output))
      (expand-java environment (cons front more))]

     #;
     [(list (struct parsed (inside)) rest ...)
      (define output (expand-java environment rest))
      (cons inside output)]

     #;
     [(list (list 'class name class-stuff ...) rest)

      (define (expand-class environment stuff)
        (define class-environment (copy-environment environment))
        (for/list ([stuff stuff])
          (define (parse what)
            (match what
              [(list 'java-unparsed-body) '()]
              [(list 'java-unparsed-body stuff ...)
               (define-values (parsed unparsed)
                              (enforest-java-class-body stuff class-environment))
               (define parsed* (parse parsed))
               `(,parsed* (java-unparsed-body ,@unparsed))]

              [(list 'var id type)
               ;; TODO add binding for var
               `(var ,id ,type)]
              [(struct parsed (inside)) (parse inside)]
              [(list 'constructor name body)
               (define body* (expand-java (copy-environment class-environment)
                                          body))
               `(constructor ,name ,@body*)]
              [else (error 'class "unknown form ~a" stuff)]))
          (parse stuff)))

      (define stuff* (expand-class environment class-stuff))
      (define new-environment (update-bindings environment name (java-class stuff*)))
      (define rest* (expand-java new-environment rest))
      `((class ,name ,stuff*) ,@rest*)]

     [(list 'constructor name stuff)
      (define stuff* (expand-java environment stuff))
      `(constructor ,name ,stuff*)]

     [(list 'package stuff ...)
      `(package ,@stuff)]

     [(list 'import stuff ...)
      (define item (last stuff))
      (add-binding-name! environment item 'type)
      `(import ,@stuff)]

     [(list 'method name type body)
      (define body* (expand-java (copy-environment environment) body))
      `(method ,name ,type ,body*)]

     [(list 'assign left right)
      (define left* (expand-java environment left))
      (define right* (expand-java environment right))
      `(assign ,left* ,right*)]

     [(list 'op op left right)
      (define left* (expand-java environment left))
      (define right* (expand-java environment right))
      `(op ,op ,left* ,right*)]

     [(list 'new what args ...) `(new ,what ,@args)]
     [(list 'dot left right)
      (define left* (expand-java environment left))
      (define right* (expand-java environment right))
      `(dot ,left* ,right*)]

     [(list 'var name type stuff ...)
      (define stuff* (expand-java environment stuff))
      (add-binding-name! environment name 'lexical)
      `(var ,name ,type ,@stuff*)]

     [(and x (list 'macro name macro))
      (debug "Add macro binding for ~a\n" name)
      (add-binding-name! environment name macro)
      #f]

     [(and x (list 'call stuff ...)) x]

     [(list first rest ...)
      (define first* (expand-java environment first))
      (define rest* (expand-java environment rest))

      ;; first could be a macro definition which goes away completely
      (if first*
        `(,first* ,@rest*)
        rest*)]
     [(list) '()]
     [(list (list)) '()]
     [else (error 'expand-java "don't know how to expand ~a" code)]
     ))

(define (start-expand-java input)
  (define environment (base-java-environment))
  (define code `(java-unparsed-top ,@input))
  (expand-java environment code))

(define (parse-java input)
  #|
  (define-values (parsed unparsed)
                 (enforest-java (syntax->datum input)))
  (when unparsed
    (error 'parse-java "unparsed ~a" unparsed))
  parsed
  |#

  #;
  (define out
    (let loop ([all '()]
               [more (syntax->datum input)])
      (if (null? more)
        (reverse all)
        (let ()
          (define-values (parsed unparsed) (parse-top-level more))
          (loop (cons parsed all) unparsed)))))

  (define out (start-expand-java input))

  out
    
  )

#;
(define (remove-parsed input)
  (cond
    [(parsed? input) (remove-parsed (parsed-data input))]
    [(pair? input) (map remove-parsed input)]
    [else input]))

#|
(parse-java (java-read-syntax "1 + 1 * 4"))
(parse-java (java-read-syntax "new foo()"))
(parse-java (java-read-syntax "1 + new foo()"))
(parse-java (java-read-syntax "1 + new foo() * 4"))
|#

#;
(parse-java (with-input-from-file "tests/Token.java"
                                  (lambda () (honu-read-syntax))))

(define file (command-line
               #:args (file) file))

(printf
  "~a\n"
  (unparse-java 
    (remove-parsed
      (parse-java (with-input-from-file file
                                       ; "tests/t1.java"
                                        (lambda () (honu-read)))))
    ""))

;; the honu framework is
;; 1. read into s-expressions
;; 2. do enforestation parsing
;; 3. convert to regular language, do normal parsing process
