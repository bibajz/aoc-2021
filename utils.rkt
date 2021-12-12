#lang racket

(provide load-and-split
         accumulate
         accumulate-right
         sum
         prod
         sliding-window
         average
         transpose
         tumbling-window
         in-matrix-2d
         zip
         flatten-1
         group-by-hash
         accumulate-by-hash
         min-list
         max-list
         string-intersect
         inverse-map
         power-range
         scalar-product
         without-last
	 nested-map
	 2d-ref
	 create-matrix
	 2d-list-set
	 2d-list-update
	 batch-2d-list-update)

(define (load-and-split path)
  (file->lines path))

(define (accumulate fn lst init)
  (define (inner l acc)
    (cond
      [(null? l) acc]
      [else (inner (cdr l) (fn acc (car l)))]))
  (inner lst init))

(define (accumulate-right fn lst init)
  (define (inner l acc)
    (cond
      [(null? l) acc]
      [else (inner (cdr l) (fn (car l) acc))]))
  (inner lst init))

(define (sum lst)
  (accumulate + lst 0))
(define (prod lst)
  (accumulate * lst 1))

(define (sliding-window n lst)
  (define (inner old new buffer)
    (if (null? old)
        (append new (list buffer))
        (if (= (length buffer) n)
            (inner (cdr old) (append new (list buffer)) (append (cdr buffer) (list (car old))))
            (inner (cdr old) new (append buffer (list (car old)))))))
  (inner lst '() '()))

(define (average lst)
  (/ (* 1.0 (sum lst)) (length lst)))

(define (pairwise-list lst1 lst2)
  (if (null? lst1) (map list lst2) (map (lambda (l1 l2) (append l1 (list l2))) lst1 lst2)))

(define (transpose list-of-lists)
  (accumulate pairwise-list list-of-lists '()))

(define (tumbling-window n lst)
  (define (inner old new buffer)
    (if (null? old)
        (append new (list buffer))
        (if (= (length buffer) n)
            (inner old (append new (list buffer)) '())
            (inner (cdr old) new (append buffer (list (car old)))))))
  (inner lst '() '()))

; Returns #t if a number is not in a matrix, its position otherwise
(define (in-matrix-2d matrix num)
  (let ([row (index-where (map (lambda (l) (index-of l num)) matrix) number?)])
    (cons num (if row (cons row (index-of (list-ref matrix row) num)) row))))

(define (zip lst1 lst2)
  (if (or (null? lst1) (null? lst2))
      '()
      (append (list (list (car lst1) (car lst2))) (zip (cdr lst1) (cdr lst2)))))

; Unfolds only the first level of nesting, unline flatten, which does flatten arbitralily deep
(define (flatten-1 list-of-lists)
  (if (null? list-of-lists) '() (append (car list-of-lists) (flatten-1 (cdr list-of-lists)))))

(define (group-by-hash grouping-fn lst)

  (for/hash ([group (group-by grouping-fn lst)])
    (values (car group) group)))

; TODO: How can I destructure a tuple, thus avoiding the `key+group` ???
(define (accumulate-by-hash grouping-fn accumulate-fn lst)

  (for/hash ([key+group (hash->list (group-by-hash grouping-fn lst))])
    (values (car key+group) (accumulate-fn (cdr key+group)))))

(define (max-list lst)
  (accumulate max (cdr lst) (car lst)))
(define (min-list lst)
  (accumulate min (cdr lst) (car lst)))

(define (string-contains-char? str c)
  (string-contains? str (string c)))

(define (string-intersect s1 s2)
  (list->string (filter (lambda (c) (string-contains-char? s2 c)) (string->list s1))))

(define (inverse-map h)
  (make-immutable-hash (for/list ([(k-v) (hash->list h)])
                         (cons (cdr k-v) (car k-v)))))

(define (power-range base start stop step)
  (for/list ([i (inclusive-range start stop step)])
    (expt base i)))

(define (scalar-product list1 list2)
  (sum (map prod (zip list1 list2))))

(define (without-last lst)
  (reverse (cdr (reverse lst))))

(define (nested-map fn list-of-lists)
  (map (lambda (lst) (map fn lst)) list-of-lists))

(define (2d-ref list-of-lists row-column)
  (list-ref (list-ref list-of-lists (car row-column)) (cdr row-column)))

(define (create-matrix num-rows num-cols)
  (for/list ([two-list (cartesian-product (range num-rows) (range num-cols))])
    (cons (car two-list) (cadr two-list))))

(define (2d-list-set list-of-lists pos value)
  (list-set list-of-lists (car pos) (list-set (list-ref list-of-lists (car pos)) (cdr pos) value)))

(define (2d-list-update list-of-lists pos updater)
  (list-set list-of-lists
            (car pos)
            (list-update (list-ref list-of-lists (car pos)) (cdr pos) updater)))

(define (batch-2d-list-update list-of-lists positions updater)
  (if (null? positions)
      list-of-lists
      (batch-2d-list-update (2d-list-update list-of-lists (car positions) updater)
                            (cdr positions)
                            updater)))
