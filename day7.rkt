#lang racket

(require "utils.rkt")

(define (parse-line line)
  (map string->number (string-split line ",")))

(define input (car (map parse-line (load-and-split "input/day7.txt"))))

(define (fuel-consumption lst position dist-fn)
  (sum (map (lambda (x) (dist-fn (abs (- x position)))) lst)))

(define (arithmetic-sum n)
  (sum (inclusive-range 0 n)))

(display "Solution 1: ")
(car (sort (map (lambda (x) (cons x (fuel-consumption input x (lambda (x) x))))
                (inclusive-range (min-list input) (max-list input)))
           <
           #:key cdr))

(display "Solution 2: ")
(car (sort (map (lambda (x) (cons x (fuel-consumption input x arithmetic-sum)))
                (inclusive-range (min-list input) (max-list input)))
           <
           #:key cdr))
