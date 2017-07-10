;; read-file produces a list whose elements are the expressions in the file.

(define (read-file)
  (let ((expr (read)))
    (if (eof-object? expr)
        '()
        (cons expr (read-file)))))

;; Here we go: read in the file that defines the graph

(define data (with-input-from-file "dist.dat" read-file))


;;Lookup proedure will check does the table contains variables. Return the
;;cost if the table contains, otherwise return false.

(define (lookup key-1 key-2 table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
        (if (null? key-2)
            (cdr subtable)
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  #f)))
        #f)))

;;Insert! procedure will put the value into the table with the two keys.

(define (insert! key-1 key-2 value table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
        (let ((record (assoc key-2 (cdr subtable))))
          (if record
              (set-cdr! record value)
              (set-cdr! subtable
                        (cons (cons key-2 value)
                              (cdr subtable)))))
        (set-cdr! table
                  (cons (list key-1
                              (cons key-2 value))
                        (cdr table))))))

;;Constructor for a new table

(define (make-table) (list '*table*))

;;Create two tables, one to save the cost between two single nodes, other one
;;to save the cost of path between two nodes.

(define node-table (make-table))
(define path-table (make-table))

;;Define a variable infinity for there is no edge between two nodes.

(define infinity 100000)

;;Put-node procedure will take two nodes from the list and save their cost in
;;to the table.

(define (put-node line)
  (let ((first (car line))
        (second (cadr line))
        (cost (caddr line)))
    (insert! first second cost node-table)))

;;Take-node procedure will take every simgle element from the data and put them
;;into the node table

(define (take-node data-list)
  (let ((current (car data-list))
        (left (cdr data-list)))
    (cond ((null? left) (put-node current))
          (else (put-node current)
                (take-node left)))))


(define (compute children node)
  (cond ((null? children) (list infinity))
        ((equal? 'end (caar children))
         (let ((num (lookup node (caar children) node-table)))
           (cons num (list 'end))))
        (else
         (let ((co (cost (caar children))))
           (let ((x (+ (lookup node (caar children) node-table) (caar co)))
                 (y (compute (cdr children) node)))
             (if (> x (car y))
                 y
                 (cons x (cons (caar children) (cdar co)))))))))


(define (cost node)
  (let ((result (lookup node '() path-table)))
    (if result
        result
        (let ((children (lookup node '() node-table)))
          (cond (children
                 (let ((x (compute children node)))
                   (insert! node (car x) (cdr x) path-table))
                 (lookup node '() path-table))
                (else (list (list infinity))))))))

(define (display-result)
  (let ((num (caar (cost 'start)))
        (path (cdar (cost 'start))))
    (cons num (cons 'start path))))


(begin (take-node data)
       (display-result))
