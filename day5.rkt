#lang racket

(require "utils.rkt")

; TODO: only 2d for now
(define (string->coords-2d str sep)
  (cons (car (map string->number (string-split str sep)))
        (cadr (map string->number (string-split str sep)))))

(define (parse-line line)
  (cons (string->coords-2d (car (map string-trim (string-split line "->"))) ",")
        (string->coords-2d (cadr (map string-trim (string-split line "->"))) ",")))

(define input (map parse-line (load-and-split "input/day5.txt")))

(define (is-vertical-or-horizontal? p1 p2)
  (or (= (car p1) (car p2)) (= (cdr p1) (cdr p2))))

(define (generate-line p1 p2 fn)
  (fn (inclusive-range (car p1) (car p2) (if (< (car p1) (car p2)) 1 -1))
                     (inclusive-range (cdr p1) (cdr p2) (if (< (cdr p1) (cdr p2)) 1 -1))))

(display "Solution 1: ")
(length (filter (lambda (x) (> x 1))
                (map (lambda (l) (length l))
                     (group-by (lambda (x) x)
                               (flatten-1
                                (map (lambda (p) (generate-line (car p) (cdr p) cartesian-product))
                                     (filter (lambda (p) (is-vertical-or-horizontal? (car p) (cdr p)))
                                             input)))))))

(display "Solution 2: ")
(length
 (filter (lambda (x) (> x 1))
         (map (lambda (l) (length l))
              (group-by
               (lambda (x) x)
               (flatten-1
                (map (lambda (p)
                       (generate-line
                        (car p)
                        (cdr p)
                        (if (is-vertical-or-horizontal? (car p) (cdr p)) cartesian-product zip)))
                     input))))))
