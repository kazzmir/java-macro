#lang racket

(require honu/core/read
         racket/pretty)

(define (java-read-syntax input)
  (with-input-from-string input
    (lambda ()
      (honu-read-syntax))))

(struct parsed (data) #:transparent)
(struct operator (precedence association binary unary postfix?))

(define (id? x)
  (and (symbol? x)
       (not (or (operator? x)
                (member x '(= #%parens #%braces #%brackets < >))
                (member x '(public private protected))))))

(define (binary-operator precedence association binary [unary #f] [postfix? #f])
  (operator precedence association binary unary postfix?))

(define java-+ (binary-operator 1 'left (lambda (left right)
				   `(op ,+ ,@left ,@right))))
(define java-- (binary-operator 1 'left (lambda (left right)
				   `(op ,- ,@left ,@right))))
(define java-* (binary-operator 2 'left (lambda (left right)
				   `(op ,* ,@left ,@right))))
(define java-/ (binary-operator 2 'left (lambda (left right)
				   `(op ,/ ,@left ,@right))))

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

(define (operator-unary-transformer operator)
  (case operator
    [(! -) (lambda (left)
           (parsed `(unary-op ,operator ,@left)))]
    [else #f]))

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

;; (define debug printf)
(define-syntax-rule (debug x ...) (void))

(define (parse-args args)
  ;; parse an expression, then maybe a comma, then repeat
  (let loop ([all '()]
             [rest args])
    (define-values (expr1 more) (enforest-java rest))
    (if expr1
      (loop (cons expr1 all)
            (match more
              [(list 'honu-comma rest ...) rest]
              [else more]))
      (reverse all))))

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
      [(list) (values (left current) #f)]
      [(list 'honu-comma rest ...)
       (if current
         (values (left current) stream)
         (values (left #f) stream))]
      [(list (list '%semicolon inner ...) more ...)
       ;; there should only be one statement here so just take it out
       (values (first (parse-all inner environment)) more)]

      [(list (and (? operator?) operator) rest ...)
       (define new-precedence (operator-precedence operator))
       (define association (operator-association operator))
       (define binary-transformer (operator-binary operator))
       (define unary-transformer (operator-unary operator))
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
                                      (error 'binary "cannot be used as a binary operator in ~a" operator))
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

      [(list '#%braces code ...)
       (define inner (parse-all code environment))
       (values (parsed `(body ,@inner)) #f)]

      [(list 'if (list '#%parens condition-code ...) (list '#%braces then-code ...) 'else (list '#%braces else-code ...) more ...)
       (debug "If with an else\n")
       (define condition (first (parse-all condition-code environment)))
       (define then (parse-all then-code environment))
       (define else (parse-all else-code environment))
       (values `(if ,condition (body ,@then) (body ,@else)) more)]

      [(list 'if (list '#%parens condition-code ...) (list '#%braces then-code ...) more ...)
       (debug "If by itself\n")
       (define condition (first (parse-all condition-code environment)))
       (define then (parse-all then-code environment))
       (values `(if ,condition (body ,@then)) more)]

      [(list 'return rest ...)
       (define returned (first (parse-all rest environment)))
       (values `(return ,returned) #f)]

      ;; Type name
      [(list (and (? id?) type) (and (? id?) name))
       (if current
         (error 'enforest-java "cannot have anything next to a variable definition ~a" current)
         (values `(var ,type ,name) #f))]

      ;; List<Foo> x = ...
      [(list (and (? id?) type) '< (and (? id?) generic) '> (and (? id?) name) '= right-side ...)
       (if current
         (error 'enforest-java "cannot have anything next to a variable definition ~a" current)
         (let ()
           (define right (first (parse-all right-side environment)))
           (values `(var ,type ,name ,right) #f)))]

      ;; List x = ...
      [(list (and (? id?) type) (and (? id?) name) '= right-side ...)
       (if current
         (error 'enforest-java "cannot have anything next to a variable definition ~a" current)
         (let ()
           (define right (first (parse-all right-side environment)))
           (values `(var ,type ,name ,right) #f)))]

      [(list (list '#%brackets expr ...) more ...)
       (if (not current)
         (error 'enforest-java "must have something next to brackets")
         (let ()
           (define inside (first (parse-all expr environment)))
           (parse more precedence left (parsed `(lookup ,current ,inside)))))]

      ;; new foo()
      [(list 'new (and (? id?) constructor) (list '#%parens args ...)
             rest ...)
       (if current
         (values (left current) stream)
         (let ()
           (define parsed-args (parse-args args))
           (define output (parsed `(make ,constructor ,@parsed-args)))
           (parse rest precedence left output)))]

      [(list (list '#%parens args ...) more ...)
       (debug "Function call with ~a at ~a\n" current precedence)
       (if current
         (if (> precedence function-call-precedence)
           (let ()
             (define function (left current))
             (define parsed-args (parse-args args))
             (parse more function-call-precedence (lambda (x) x) (parsed `(call ,function ,@parsed-args))))
           (let ()
             (define parsed-args (parse-args args))
             (parse more precedence left (parsed `(call ,current ,@parsed-args)))))
         ;; not a function call, just parenthesizing an expression
         (let ()
           (define inner (parse-all args environment))
           (parse more precedence left inner)))]

      [(list (and (? id?) x) rest ...)
       (debug "Parse id ~a\n" x)
       (if current
         (let ()
           (define-values (right more) (enforest-java stream environment))
           (parse more precedence (lambda (x) (parsed `(cast ,current ,right ,x))) #f))
         (parse rest precedence left (parsed x)))
       #;
       (if current
         (values (left current) stream)
         (parse rest precedence left (parsed x)))]

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
  (match body
    [(list (list '%semicolon (or 'public 'private 'protected)
                 (and (? symbol?) type) (and (? symbol?) var))
           more ...)
     (values `(var ,var ,type) more)]
    [(list (or 'public 'private) (and (? id?) id) (list '#%parens args ...)
           (list '#%braces body ...)
           more ...)
     (define code (parse-body body environment))
     (debug "Constructor body ~a\n" (pretty-format code))
     (values `(constructor ,id (body ,@code)) more)]
    [(list (or 'public 'private) (and (? id?) type)
           (and (? id?) id) (list '#%parens args ...)
           (list '#%braces body ...)
           more ...)
     (define code (parse-body body environment))
     (values `(method ,id ,type (body ,@code)) more)]
    [(list (or 'public 'private)
           (and (? symbol?) type) '< (not '>) ... '>
           (and (? symbol?) id) (list '#%parens args ...)
           (list '#%braces body ...)
           more ...)
     (define code (parse-body body environment))
     (values `(method ,id ,type (body ,@code)) more)]
    [else (error 'parse-class-body "can't parse ~a" body)]
    ))

(define (parse-class class class-body environment)
  (define class-stuff
    (let loop ([all '()]
               [more class-body])
      (if (null? more)
        (reverse all)
        (let ()
          (define-values (parsed unparsed)
                         (parse-class-body more environment))
          (loop (cons parsed all) unparsed)))))
  `(class ,class ,@class-stuff))

(define (parse-top-level input environment)
  (match input
    [(list (list '%semicolon 'package rest ...)
           more ...)
     (values `(package ,@rest) more)]
    [(list (list '%semicolon 'import rest ...)
           more ...)
     (values `(import ,@rest) more)]
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
    [(list (list 'package name ...) more ...)
     (unparse-java more tabs (string-append so-far (format "package ~a;" (package-name name))))]
    [(list (list 'import name ...) more ...)
     (unparse-java more tabs (string-append so-far (format "\nimport ~a;" (package-name name))))]
    [(list (list 'class name body ...) more ...)
     (unparse-java more tabs (string-append so-far (format "\nclass ~a{\n~a\n~a}" name (unparse-java body (add-tab tabs)) tabs)))]
    [(list (list 'var name type) more ...)
     (unparse-java more tabs (string-append so-far (format "\n~aprivate ~a ~a;" tabs type name)))]
    [(list (list 'constructor name body) more ...)
     (unparse-java more tabs
                   (string-append so-far
                                  (format "\n~apublic ~a(){\n~a\n~a}"
                                          tabs name
                                          (unparse-java body (add-tab tabs))
                                          tabs)))]
    [(list (list 'method name type body) more ...)
     (unparse-java more tabs (string-append so-far (format "\n~apublic ~a ~a(){\n~a\n~a}" tabs type name (unparse-java body (add-tab tabs)) tabs)))]
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
    [(list 'expression (list 'make type args ...))
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

    [(list 'body (list 'for init condition rest body) more ...)
     (unparse-java `(body ,@more) tabs 
                   (string-append (format "\n~afor (~a; ~a; ~a){\n~a\n~a}\n"
                                          tabs
                                          (unparse-java `(expression ,@init) "")
                                          (unparse-java `(expression ,@condition) "")
                                          (unparse-java `(expression ,@rest) "")
                                          (unparse-java body (add-tab tabs))
                                          tabs)))]

    [(list 'expression (list 'not more))
     (string-append so-far (format "!(~a)" (unparse-java `(expression ,@more) tabs)))]
    [(list 'expression (list 'unary-op op x))
     (string-append so-far (format "~a~a" op (unparse-java `(expression ,x) tabs)))]
    [(list 'expression (list 'op op a b))
     (string-append so-far (format "~a ~a ~a"
                                   (unparse-java `(expression ,a) tabs)
                                   op
                                   (unparse-java `(expression ,b) tabs)))]
    [(list 'expression (list 'cast (list class) expr ignore))
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
  (hash))

(define-syntax-rule (add-binding! environment symbol value)
		    (set! environment (hash-set environment 'symbol value)))

(struct macro (function))
(define-syntax-rule (define-macro (name arg) body ...)
		    (define name (macro (lambda (arg) body ...))))

(define (environment-value environment name)
  (hash-ref environment name (lambda () #f)))

(define-macro (java-unparsed-top stx environment)
  (parse-top-level stx environment))

(define (is-macro? id environment)
  (define value (environment-value environment id))
  (and value (macro? value)))

(define (base-java-environment)
  (define environment (make-environment))
  (add-binding! environment + java-+)
  (add-binding! environment - java-+)
  (add-binding! environment * java-*)
  (add-binding! environment / java-/)
  (add-binding! environment java-unparsed-top java-unparsed-top)
  environment)

(define (update-bindings environment code)
  environment)

(define (expand-java environment code)
  (match code
     [(list (and symbol? (? (lambda (x) (is-macro? x environment))) id) rest ...)
      (define macro (environment-value environment id))
      (define output ((macro-function macro) environment rest))
      (expand-java (update-bindings environment output) output)]
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

  (define out
    (let loop ([all '()]
               [more (syntax->datum input)])
      (if (null? more)
        (reverse all)
        (let ()
          (define-values (parsed unparsed) (parse-top-level more))
          (loop (cons parsed all) unparsed)))))

  out
    
  )

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

(printf "~a\n"
        (unparse-java 
          (remove-parsed
            (parse-java (with-input-from-file "tests/Token.java"
                                              (lambda () (honu-read-syntax)))))
          ""))

;; the honu framework is
;; 1. read into s-expressions
;; 2. do enforestation parsing
;; 3. convert to regular language, do normal parsing process
