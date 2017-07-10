;;Bowei Li
;;CS 450
;;09/16/2013

#lang Scheme

;;Problem 1

(define (is-list? x)
  (cond ((null? x) #t)                 ;;if x is null, means empty list
        ((pair? x) (is-list? (cdr x))) ;;if x is a pair, keep checking
        (else #f)))                    ;;otherwise, x is not a list.

;;Problem 2

(define (my-reverse x)
  (if (null? (cdr x))
      x               ;;if (cdr x) is null, means it is the end of list.
      (append (my-reverse (cdr x)) (list (car x)))))

;;There is my idea, I separate every element which in the list and make them to
;;be a list which just contains themselves, then I use the append procedure,
;;which will combine two list, to regroup these list in reverse order. Finally,
;;I will get a list which will be reverse order of the origin one.


;;Problem 3

(define (even-parity x)
  (cond ((null? x) x)
        ((even? (car x)) (append (list (car x)) (even-parity (cdr x))))
        (else (even-parity (cdr x)))))


(define (odd-parity x)
  (cond ((null? x) x)
        ((odd? (car x)) (append (list (car x)) (odd-parity (cdr x))))
        (else (odd-parity (cdr x)))))

(define (same-parity x . y)
  (if (even? x)
      (append (list x) (even-parity y))
      (append (list x) (odd-parity y))))

;;For this case, I first define two help procedures which will collect even or
;;odd numbers from a list. And then, in same-parity procedure, it will determine
;;is first element even or odd number, and call the procedure to get the list.


;;Problem 4

(define (square-list items)
  (if (null? items)
      '()    ;;if current item is null, just return empty list.
      (cons (* (car items) (car items))
            (square-list (cdr items))))) ;;combine everything together

(define (square-list-map items)
  (map (lambda (x) (* x x)) items))


;;Problem 5

(define (my-for-each x)
  (for-each (lambda (x) (newline) (display (even? x))) x))



;;Problem 7

(define (my-equal? x y)
  (if (and (pair? x) (pair? y)) ;;if both x and y are list, check recursively.
      (and (my-equal? (car x) (car y)) (my-equal? (cdr x) (cdr y)))
      (eqv? x y)))              ;;otherwise just compare two symbols


;;Problem 8

;;a

(define (every? pred seq)
  (if (null? seq)
      (pred seq)
      (if (null? (cdr seq))
          (pred (car seq))
          (and (pred (car seq)) (every? pred (cdr seq))))))

;;b

;;If the list is empty, the result will be false except predicates are null?
;;and list?. Empty list means there is no element can be tested by the
;;predicate, so it will return #f. The only predicates that empty list can
;;satisfy are null? and list? predicate because empty list means nothing, and it
;;is a list. It is true that empty list is a list which contains nothing. And
;;also, the symbol of list is '() which is also means null in Scheme. When null
;;value is passed to every predicate except null? and list?, the result is
;;false.



;;Problem 9

;;help procedure from book P.152 which determine does set contains x.
(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))


(define (unordered-union-set x y)
  (cond ((null? x) y)
        ((null? y) x)
        ((element-of-set? (car x) y)
         (unordered-union-set (cdr x) y))
        (else (cons (car x) (unordered-union-set (cdr x) y)))))


;;Problem 10

(define (ordered-union-set x y)
  (cond ((null? x) y)
        ((null? y) x)
        (else
         (let ((x1 (car x)) (x2 (car y)))
           (cond ((= x1 x2) (cons x1 (ordered-union-set (cdr x) (cdr y))))
                 ((< x1 x2) (cons x1 (ordered-union-set (cdr x) y)))
                 ((< x2 x1) (cons x2 (ordered-union-set x (cdr y)))))))))


;;Problem 11


(define (remove-val x y)
  (cond ((null? x) y)
        ((null? y) '())
        ((element-of-set? x y)
         (if (eq? x (car y))
             (remove-val x (cdr y))
             (cons (car y) (remove-val x (cdr y)))))
        (else y)))
