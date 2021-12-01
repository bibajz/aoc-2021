#lang racket

(require "utils.rkt")

(define (increased? a b)
  (if (> b a) 1 0))

; Seems a little clumsy, revisit later
(define (solution lst)
  (if (null? (cdr lst)) 0 (+ (increased? (car lst) (cadr lst)) (solution (cdr lst)))))

(display "Solution 1: ")
(solution (map string->number (load-and-split "input/day1.txt")))

(display "Solution 2: ")
(solution (map sum (sliding-window 3 (map string->number (load-and-split "input/day1.txt")))))
