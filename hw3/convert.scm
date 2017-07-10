;;Bowei Li
;;CS 450
;;2013/9/22
;;Assignment 3


;;look-up is a helper procedure which will return the base unit of the input
;;unit. This procedure also takes care about power of the unit. For example, if
;;one inch equal 0.0254 m, then (in 2) will be 0.0254*0.0254 (m 2).

(define (look-up unit)
  (define (power x n)    ;;procedure for calculating the power of number
    (cond ((= n 0) 1)
          ((> n 0) (* x (power x (- n 1))))
          ((< n 0) (/ 1 (power x (abs n))))))
  (define (power-unit u n) ;;procedure for calculating the power of unit
    (if (null? u)
        '()
        (cons (list (caar u) (* (cadar u) n))
              (power-unit (cdr u) n))))
  (let ((p (cadar unit))
        (base-unit (assoc (caar unit) source)))
    (if (assoc (caar unit) source)
        (cons (power (caadr base-unit) p)
              (power-unit (cdadr base-unit) p))
        (cons 1 unit))))

;;combine-unit-list is a helper procedure which take two base unit-lists and
;;return the combination of these. For example, '((k 2)(m 2)) combine with '((m
;;1)(sec 2) will be '((k 2)(m 3)(sec 2)).

(define (combine-unit-list x y)
  (define (add-unit first second) ;;procedure for adding two single unit
    (if (equal? (car first) (car second))
        (list (car first) (+ (cadr first) (cadr second)))
        (list first second)))
  (cond ((null? x) y)
        ((null? y) x)
        (else
         (if (>= (length x) (length y))
             (if (assoc (caar x) y)
                 (cons (add-unit (car x) (assoc (caar x) y)) 
                       (combine-unit-list (cdr x) (cdr y)))
                 (cons (car x) (combine-unit-list (cdr x) y)))
             (combine-unit-list y x)))))

;;compare? procedure will determine are two unit-list comparable or not.

(define (compare? u v)
  (if (null? u)
      #t
      (if (assoc (caar u) v)
          (if (= (cadar u) (cadr (assoc (caar u) v)))
              (compare? (cdr u) v)
              #f)
          #f)))

;;convert-unit is a helper procedure which convert the input unit-list to a
;;base-unit-list.

(define (convert-unit u)
  (if (null? (cdr u))
      (look-up u)
      (let ((crt-unit (look-up (list (car u))))
            (nxt-unit (convert-unit (cdr u))))
        (cons (* (car crt-unit) (car nxt-unit))
              (combine-unit-list (cdr crt-unit) (cdr nxt-unit))))))


(define (convert quantity unit)
  (let ((u (convert-unit (cdr quantity)))
        (v (convert-unit unit)))
    (if (compare? (cdr u) (cdr v))
        (cons (/ (* (car quantity) (car u)) (car v)) unit)
        (error "units are not comparable" (cdr u) (cdr v)))))
            
  


;;read-file produces a list whose elements are the expressions in the fill.

(define (read-file)
  (let ((expr (read)))
    (if (eof-object? expr)
        '()
        (cons expr (read-file)))))

;;Here we go: read in the database.

(define source (with-input-from-file "units.dat" read-file))
