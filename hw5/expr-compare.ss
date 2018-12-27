(define (expr-compare x y) (expr-compare-helper x y empty))


; all keywords which can't be split on (if, etc)
; returns true if a and b are either both one of the keywords or both not one of them
(define (notKeywordMismatch? a b)
  (cond
    ; if
    [(xor (eq? 'if a) (eq? 'if b)) #f]
    ; cond
    [(xor (eq? 'cond a) (eq? 'cond b)) #f]
    ; let
    [(xor (eq? 'let a) (eq? 'let b)) #f]
    ; lambda
    [(xor (eq? 'lambda a) (eq? 'lambda b)) #f]

    ; default
    [#t #t]
  )
)

; defs is a list of lists which records which variable bindings have been created
; Format: '((a b a!b) (c d c!d) (f f f))
; defs stores matched variables (var is f in both x and y) too, allowing variables to match to their most recent definition - implements scope
(define (expr-compare-helper x y defs)
  (cond
    ; If x,y are both empty lists (base case for recursion)
    [(and (null? x) (null? y))
      empty
    ]
    ; Check that x and y are lists, and that they have the same length and neither is quoted (if dif lengths or first element of both is quote, we treat them like atoms) (if exactly one of them starts with any specific keyword (like if), then we treat the whole list like an atom)
    [(and (pair? x) (pair? y) (= (length x) (length y)) (not (or (eq? 'quote (car x)) (eq? 'quote (car y)))) (notKeywordMismatch? (car x) (car y)))
      (cond

        ; Check if 1st element is let for both x and y and each has the same amount of definitions
        [(and (eq? 'let (car x)) (= (length (cadr x)) (length (cadr y))))
	  (let ((result (expr-compare-def (cadr x) (cadr y) defs)))
            (cons 'let (cons (car result) (expr-compare-helper (cddr x) (cddr y) (cadr result))))
	  )
	]

	; 1st element of both is lambda, and each has same amount of variables
	[(and (eq? 'lambda (car x)) (= (length (cadr x)) (length (cadr y))))
	  (let ((result (expr-compare-lambda (cadr x) (cadr y) defs)))
	    (cons 'lambda (cons (car result) (cons (expr-compare-helper (caddr x) (caddr y) (cadr result)) (expr-compare-helper (cdddr x) (cdddr y) defs)))) ; only use the updated defs for the lambda function body. Afterwards, go back to current defs list
	  )
	]

	; both start with let or both start with lambda, and they have different-sized definition/var lists
	[(or (eq? 'let (car x)) (eq? 'lambda (car x)))
	  (let ((x_new (applyDefs x defs 0)) (y_new (applyDefs y defs 1)))
	    (list 'if '% x_new y_new)
	  )
	]

        ; neither starts with let, simply apply expr-compare-helper to each list element recursively
	[#t
	  (cons (expr-compare-helper (car x) (car y) defs) (expr-compare-helper (cdr x) (cdr y) defs))
	]
      )
    ]
    
    ; If atoms, different length lists, or quoted lists
    [#t
      (let ((x_new (applyDefs x defs 0)) (y_new (applyDefs y defs 1))) ; apply any variable name changes, so that we don't accidentally match 2 variables with different bindings
	(if (equal? x_new y_new)
	  x_new
	    
	  ; if x_new != y_new, check for special cases where x,y = #t,#f,etc or we have to use a new variable binding
	  (cond
	    [(equal? x_new y_new)
	      x_new
	    ]
	    [(and (eq? x #t) (eq? y #f))
	      '%
	    ]
	    [(and (eq? x #f) (eq? y #t))
	      (list 'not '%)
	    ]
	    [#t
	      (list 'if '% x_new y_new)
	    ]
	  )
	)
      )
    ]
  )
)

; checks whether the symbols s matches with a new variable binding (ex: a!b)
; Return new binding if possible, otherwise return s
; sel = 1 -> s is from y
; sel = 0 -> s is from x
(define (applyDefs-helper s defs sel)
  (cond
    [(empty? defs)
      s
    ]
    [(or (and (= sel 0) (eq? s (caar defs))) (and (= sel 1) (eq? s (cadar defs))))
      (caddar defs)
    ]
    [#t
      (applyDefs-helper s (cdr defs) sel)
    ]
  )
)

; applies all new variable bindings to the given list/atom. If any variable match is found, it replaces the variable name with it's new name, ex: a!b
; sel = 1 -> s is from y
; sel = 0 -> s is from x
(define (applyDefs x defs sel)
  (cond
    ; if x is list, recursively run applyDefs on each element of list
    [(pair? x)
      (cons (applyDefs (car x) defs sel) (applyDefs (cdr x) defs sel))
    ]
    ; if x is atom (or empty list), attempt to convert it to new variable name
    [#t
      (applyDefs-helper x defs sel)
    ]
  )
)

; expr-compare helper function which takes in definition list of let statement
; returns (formatted definition list, updated defs)
(define (expr-compare-def x y defs)
  (if (and (null? x) (null? y)) ; if expr-compare-def has gone through all the defs of the let statement
    (list empty defs)
   
    ; if not done with definitions
    (let
      (
        (x_name (caar x))
        (y_name (caar y))
	(def (expr-compare-helper (cadar x) (cadar y) defs))
	(result (expr-compare-def (cdr x) (cdr y) defs))
      )
      (cond
        
        ; matching var names
        [(equal? x_name y_name)
          (list (cons (list x_name def) (car result)) (cons (list x_name x_name x_name) (cadr result)))
        ]
        
	; dif var names
        [#t
          (let ((newDefsEntry (list x_name y_name (string->symbol (string-append (symbol->string x_name) "!" (symbol->string y_name))))))
	    (list (cons (list (caddr newDefsEntry) def) (car result)) (cons newDefsEntry (cadr result)))
          )
        ]
      )
    )
  )
)

; unifies variables in lambda expressions
; x,y are the variable lists for the lambda expression
; returns (formatted var list, updated defs)
(define (expr-compare-lambda x y defs)
  (cond

    ; if var lists are empty
    [(and (null? x) (null? y))
      (list x defs)
    ]

    ; 1st var names match
    [(eq? (car x) (car y))
      (let 
	(
	  (result (expr-compare-lambda (cdr x) (cdr y) defs))
	  (newDefsEntry (list (car x) (car x) (car x)))
	)
	(list (cons (car x) (car result)) (cons newDefsEntry (cadr result)))
      )
    ]

    ; 1st var names don't match
    [#t
      (let 
	(
	  (result (expr-compare-lambda (cdr x) (cdr y) defs))
	  (newDefsEntry (list (car x) (car y) (string->symbol (string-append (symbol->string (car x)) "!" (symbol->string (car y))))))
	)
	(list (cons (caddr newDefsEntry) (car result)) (cons newDefsEntry (cadr result)))
      )
    ]
  )
)







;-------------------------------------------- TEST FUNCTIONS -----------------------------------------------------

(define (test-expr-compare x y)
  (and
    (equal?
      (eval x)
      (eval (list 'let '((% #t)) (expr-compare x y)))
    )
    (equal?
      (eval y)
      (eval (list 'let '((% #f)) (expr-compare x y)))
    )
  )
)


(define test-expr-x '(let ((a 2) (b 3) (c 'h))
		       ((lambda (a b) (list a b)) (if a a c) '(list 3 2))))

(define test-expr-y '(let ((a 2) (c 3) (d 'f))
		       ((lambda (b d) (list a c)) (list a a c) '(cons 3 2))))
