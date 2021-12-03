#lang racket

(require "utils.rkt")

(define (parse-line line)
  (map (compose string->number string) (string->list line)))

(define input (map parse-line (load-and-split "input/day3.txt")))

; For some reason, Racket treats (exact-round 0.5) as 0 ???
(define (round-between-0-1 x)
  (if (>= x 0.5) 1 0))

(define (gamma lst)
  (string->number (string-append* (map (compose number->string round-between-0-1 average)
                                       (transpose lst)))
                  2))

(define (epsilon lst)
  (string->number
   (string-append* (map (compose number->string (lambda (x) (- 1 x)) round-between-0-1 average)
                        (transpose lst)))
   2))

(display "Solution 1: ")
(* (gamma input) (epsilon input))

(define (rating fn list-of-lists)
  (define (inner ll n)
    (if (or (= (length ll) 1) (>= n (length (car ll))))
        (car ll)
        (let ([rounded-avg (fn (average (list-ref (transpose ll) n)))])
          (inner (filter (lambda (l) (= (list-ref l n) rounded-avg)) ll) (+ n 1)))))
  (inner list-of-lists 0))

(define (oxygen lst)
  (string->number (string-append* (map number->string (rating round-between-0-1 lst))) 2))
(define (co2 lst)
  (string->number
   (string-append* (map number->string (rating (lambda (x) (- 1 (round-between-0-1 x))) lst)))
   2))

(display "Solution 2: ")
(* (oxygen input) (co2 input))
