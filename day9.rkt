#lang racket

(require "utils.rkt")

(define (parse-line line)
  (map string->number (filter-not (lambda (s) (equal? s "")) (string-split line ""))))

(define input (map parse-line (load-and-split "input/day9.txt")))

(define num-col (length (car input)))
(define num-row (length input))

(define (2d-ref list-of-lists row-column)
  (list-ref (list-ref list-of-lists (car row-column)) (cdr row-column)))

(define (adjacent row-column)
  (list (cons (- (car row-column) 1) (cdr row-column))
        (cons (+ (car row-column) 1) (cdr row-column))
        (cons (car row-column) (- (cdr row-column) 1))
        (cons (car row-column) (+ (cdr row-column) 1))))

(define (adjacent-bounded row-column num-rows num-columns)
  (filter (lambda (pos)
            (and (and (>= (car pos) 0) (< (car pos) num-rows))
                 (and (>= (cdr pos) 0) (< (cdr pos) num-columns))))
          (adjacent row-column)))

(define (create-matrix num-rows num-cols)
  (for/list ([two-list (cartesian-product (range num-rows) (range num-cols))]) (cons (car two-list) (cadr two-list))))


(define (pred-all lst el pred-fn)
  (andmap (lambda (x) (pred-fn el x)) lst))


(define (is-low-point? row-column num-rows num-columns base-grid)
  (pred-all (map (lambda (p) (2d-ref base-grid p)) (adjacent-bounded row-column num-rows num-columns)) (2d-ref base-grid row-column) <))

(define low-points (filter (lambda (x) (is-low-point? x num-row num-col input)) (create-matrix num-row num-col)))

(display "Solution 1: ")
(sum (map (lambda (row-col) (+ (2d-ref input row-col) 1)) low-points))


(define (create-basin row-column num-rows num-columns base-grid)
  (define (inner basin to-scout already-visited)
    (if (null? to-scout) basin
      (inner
	(append basin (if (or (= (2d-ref base-grid (car to-scout)) 9) (member (car to-scout) basin)) '() (list (car to-scout))))
	(append (cdr to-scout) (filter-not (lambda (p) (or (= (2d-ref base-grid p) 9) (member p already-visited))) (adjacent-bounded (car to-scout) num-rows num-columns)))
	(append already-visited (list (car to-scout)))
	)
      )
    )
  (inner (list row-column) (filter-not (lambda (p) (= (2d-ref base-grid p) 9)) (adjacent-bounded row-column num-rows num-columns)) (list row-column)))


(display "Solution 2: ")
(prod (take (sort (map length (map (lambda (p) (create-basin p num-row num-col input)) low-points)) >) 3))
