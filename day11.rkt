#lang racket

(require "utils.rkt")

(define (parse-line line)
  (map (compose string->number string) (string->list line)))

(define input (map parse-line (load-and-split "input/day11.txt")))

(define (nested-map fn list-of-lists)
  (map (lambda (lst) (map fn lst)) list-of-lists))

(define (2d-ref list-of-lists row-column)
  (list-ref (list-ref list-of-lists (car row-column)) (cdr row-column)))

(define (adjacent-with-diag row-column)
  (filter (lambda (x) (not (and (= (car x) (car row-column)) (= (cdr x) (cdr row-column)))))
          (map (lambda (two-list) (cons (car two-list) (cadr two-list)))
               (cartesian-product
                (inclusive-range (- (car row-column) 1) (+ (car row-column) 1) 1)
                (inclusive-range (- (cdr row-column) 1) (+ (cdr row-column) 1) 1)))))

(define (adjacent-with-diag-bounded row-column num-rows num-columns)
  (filter (lambda (pos)
            (and (and (>= (car pos) 0) (< (car pos) num-rows))
                 (and (>= (cdr pos) 0) (< (cdr pos) num-columns))))
          (adjacent-with-diag row-column)))

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

(define (dumbo-update x)
  (if (= x 0) 0 (+ x 1)))

(define (step list-of-lists)
  (define (inner new-list positions-to-update)
    (if (null? positions-to-update)
        new-list
        (if (<= (2d-ref new-list (car positions-to-update)) 9)
            (inner new-list (cdr positions-to-update))
            (inner (batch-2d-list-update (2d-list-set new-list (car positions-to-update) 0)
                                         (adjacent-with-diag-bounded (car positions-to-update)
                                                                     (length list-of-lists)
                                                                     (length (car list-of-lists)))
                                         dumbo-update)
                   (create-matrix (length list-of-lists) (length (car list-of-lists)))))))
  (inner (nested-map (lambda (x) (+ x 1)) list-of-lists)
         (create-matrix (length list-of-lists) (length (car list-of-lists)))))

(step (step input))

(define (evolution fn init num-steps)
  (define (inner evolved-so-far num)
    (if (= num num-steps) evolved-so-far
	(inner (append evolved-so-far (list (fn (car (reverse evolved-so-far))))) (+ 1 num)))
    )
  (inner (list init) 0))

(newline)
(evolution step input 2)


(display "Solution 1: ")
(sum (map (lambda (l) (length (filter (lambda (x) (= x 0)) l))) (map flatten (evolution step input 100))))


(display "Solution 2: ")
(index-where (evolution step input 1000) (lambda (lol) (= (sum (flatten lol)) 0)))
