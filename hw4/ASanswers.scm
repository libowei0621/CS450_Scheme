;;Problem 1

;;a
(define make-account-lambda
  (lambda (balance)
    (define withdraw
      (lambda (amount)
        (if (>= balance amount)
            (begin (set! balance (- balance amount))
                   balance)
            "Insufficient funds")))
    (define deposit
      (lambda (amount)
        (set! balance (+ balance amount))
        balance))
    (lambda (m)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            (else (error "Unknow request -- MAKE-ACCOUNT"
                         m))))))


;;b
(define make-account-inline
  (lambda (balance)
    (lambda (m)
      (cond ((eq? m 'withdraw) 
                (lambda (amount)
                  (if (>= balance amount)
                      (begin (set! balance (- balance amount))
                             balance)
                      "Insufficient funds")))
            ((eq? m 'deposit) 
                (lambda (amount)
                  (set! balance (+ balance amount))
                  balance))
            (else (error "Unknow request -- MAKE-ACCOUNT"
                         m))))))


;;Problem 3

(define (make-monitored f)
  (let ((count 0))
    (define (mf m)
      (cond ((eq? m 'how-many-calls?) count)
            ((eq? m 'reset-count) (set! count 0))
            (else (begin (set! count (+ count 1))
                         (f m)))))
    mf))


;;Problem 4
(define (make-pw-account amount pw)
  (define make-account
    (lambda (balance)
      (define withdraw
        (lambda (amount)
          (if (>= balance amount)
              (begin (set! balance (- balance amount))
                     balance)
              "Insufficient funds")))
      (define deposit
        (lambda (amount)
          (set! balance (+ balance amount))
          balance))
      (lambda (m)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else (error "Unknow request -- MAKE-ACCOUNT"
                           m))))))
  (let ((account (make-account amount)))
    (lambda (input-pw m)
      (if (eq? input-pw pw)
          (account m)
          (lambda (x) (display "Incorrect password"))))))
