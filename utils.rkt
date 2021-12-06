#lang racket

(provide load-and-split
         accumulate
         accumulate-right
         sum
         sliding-window
         average
         transpose
         tumbling-window
         in-matrix-2d
         zip
         flatten-1
         group-by-hash
         accumulate-by-hash)

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
