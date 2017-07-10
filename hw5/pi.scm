;;; file: pi_header.scm
;;;
;;; This should be included (textually) at the top of pi.scm.  All
;;; these definitions are from the textbook.

;;; cons-stream is already defined (by a macro, as a special form) in
;;; UMB Scheme

(define stream-car car)
(define (stream-cdr stream) (force (cdr stream)))
(define stream-null? null?)
(define the-empty-stream '())

(define (stream-foreach f x)
  (if (stream-null? x)
      'done
      (begin (f (stream-car x))
             (stream-foreach f (stream-cdr x)))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))


;; Problem 1

(define (display-n stream n)
  (cond ((<= n 0) '())
        ((stream-null? stream) '())
        ((= n 1) (display (stream-car stream)))
        (else (display (stream-car stream))
              (newline)
              (display-n (stream-cdr stream) (- n 1)))))


;; Problem 2

(define (stream-map proc . argstreams)
  (if (null? (car argstreams))
      the-empty-stream
      (cons-stream
        (apply proc (map stream-car argstreams))
        (apply stream-map
               (cons proc (map stream-cdr argstreams))))))

;; Problem 3

(define ones (cons-stream 1 ones))

(define integers (cons-stream 1 (add-streams ones integers)))

(define (add-streams s1 s2) (stream-map + s1 s2))

;;helper procedure which check is x divisible by 2, 3, or 5.

(define (divisible-235? x)
  (or (= (remainder x 2) 0)
      (= (remainder x 3) 0)
      (= (remainder x 5) 0)))

;;a stream which does not contain integers which is divisible by 2, 3, 5.

(define notdiv-235
  (stream-filter (lambda (x) (not (divisible-235? x)))
                 integers))

;; Problem 4

;;helper procedure which takes a list as the argument and return the n power of
;;10 which n is the number elements in the list.

(define (pow arg)
  (define (pow-10 n)
    (if (<= n 0)
        1
        (* 10 (pow-10 (- n 1)))))
  (pow-10 (- (length arg) 1)))

;;helper procedure which takes a number as the argument and return a list which
;;contains every digits in the number.

(define (number->list-of-digits arg)
  (define (change-to-number ls)
    (if (null? ls)
        '()
        (let ((current (- (char->integer (car ls)) (char->integer #\0))))
          (cons current (change-to-number (cdr ls))))))
  (let ((current (string->list (number->string arg))))
    (change-to-number current)))

;;helper procedure which takes a list as the argument and return a stream that
;;contains same elements in the list.

(define (list->stream ls)
  (if (null? ls)
      '()
      (cons-stream (car ls) (list->stream (cdr ls)))))

;;mult-stream will takes two arguments, m is the multiplier and strm is a stream
;;of digits. This procedure will return a stream which is the decimal
;;representation of the product of m with the input stream.

(define (mult-stream m input-strm)
  (define (produce a a-list strm)
    (let ((num (car a-list))
          (power (pow a-list)))
      (cons-stream num (action (remainder a power) 
                               (cdr a-list)
                               strm))))
  (define (consume a a-list strm)
    (let ((num (+ (* 10 a) (* m (stream-car strm)))))
      (let ((list (number->list-of-digits num)))
        (if (<= (length list) (length a-list))
            (action num (cons '0 list) (stream-cdr strm))
            (action num list (stream-cdr strm))))))
  (define (action a a-list strm)
    (if (stream-null? strm)
        (list->stream a-list)
        (let ((power (pow a-list)))
          (if (and (not (null? a-list))
                   (> power (+ m (remainder a power))))
              (produce a a-list strm)
              (consume a a-list strm)))))
  (action 0 '() input-strm))

(define (mult-stream m strm)
  (define a-list '())
  (define a 0)
  (define (produce)
    (let ((num (car a-list))
          (power (pow a-list)))
      (begin (set! a (remainder a power))
             (set! a-list (cdr a-list))
             (cons-stream num (action)))))
  (define (consume)
    (let ((new-num (+ (* 10 a) (* m (stream-car strm)))))
      (let ((new-list (number->list-of-digits new-num)))
        (begin (set! a new-num)
               (if (<= (length new-list) (length a-list))
                   (set! a-list (cons '0 new-list))
                   (set! a-list new-list))
               (set! strm (stream-cdr strm))
               (action)))))
  (define (action)
    (if (stream-null? strm)
        (list->stream a-list)
        (let ((power (pow a-list)))
          (if (and (not (null? a-list))
                   (> power (+ m (remainder a power))))
              (produce)
              (consume)))))
  (action))

;; Problem 5

;;constructor of matrix data structure.

(define (cons-matrix a b c d)
  (list a b c d))

;;helper procedure which return the correspond part of matrix 

(define (selector m matrix)
  (cond ((eq? m 'a) (car matrix))
        ((eq? m 'b) (cadr matrix))
        ((eq? m 'c) (caddr matrix))
        ((eq? m 'd) (cadddr matrix))
        (else "wrong operation")))

;;helper procedure which multiply matrix

(define (compose m1 m2)
  (let ((a (+ (* (selector 'a m1) (selector 'a m2))
              (* (selector 'b m1) (selector 'c m2))))
        (b (+ (* (selector 'a m1) (selector 'b m2))
              (* (selector 'b m1) (selector 'd m2))))
        (c (+ (* (selector 'c m1) (selector 'a m2))
              (* (selector 'd m1) (selector 'c m2))))
        (d (+ (* (selector 'c m1) (selector 'b m2))
              (* (selector 'd m1) (selector 'd m2)))))
    (cons-matrix a b c d)))


;;helper procedure which add two matrix stream up

(define (add-matrix-stream s1 s2)
  (define (add-matrix m1 m2)
    (let ((a (+ (selector 'a m1) (selector 'a m2)))
          (b (+ (selector 'b m1) (selector 'b m2)))
          (c (+ (selector 'c m1) (selector 'c m2)))
          (d (+ (selector 'd m1) (selector 'd m2))))
      (cons-matrix a b c d)))
  (stream-map add-matrix s1 s2))

;;the input stream;
  
(define original (cons-stream (cons-matrix 1 4 0 2) original)) 
(define sequence (cons-stream (cons-matrix 1 6 0 3)
                              (add-matrix-stream original sequence)))

;;helper procedure which return the matrix to taken away n

(define (lost-value n)
  (cons-matrix 10 (* -10 n) 0 1))

;;helper procedure which apply n in to the matrix

(define (compute matrix n)
  (let ((a (selector 'a matrix))
        (b (selector 'b matrix))
        (c (selector 'c matrix))
        (d (selector 'd matrix)))
    (quotient (+ (* a n) b) (+ c d))))

;;help procedure which checks the matrix applied to 3 and 4 yield numbers having
;;same floor.

(define (ready? m)
  (let ((x (compute m 3))
        (y (compute m 4)))
    (= x y)))

(define (pi)
  (define (consume a strm)
    (let ((new-a (compose a (stream-car strm)))
          (new-strm (stream-cdr strm)))
      (action new-a new-strm)))
  (define (produce a strm)
    (let ((num (compute a 3)))
      (let ((new-a (compose (lost-value num) a)))
        (cons-stream num (action new-a strm)))))
  (define (action a strm)
    (if (ready? a)
        (produce a strm)
        (consume a strm)))
  (action (cons-matrix 1 0 0 1) sequence))
