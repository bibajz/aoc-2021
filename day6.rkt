#lang racket

(require racket/hash)
(require "utils.rkt")

(define (parse-line line)
  (map string->number (string-split line ",")))

(define input (car (map parse-line (load-and-split "input/day6.txt"))))

(define (evolve-lanternfish n)
  (if (= n 0) (list 6 8) (- n 1)))

; Propagates 8's to the end, otherwise keeps the list xD
(define (8-to-the-end-sort x1 x2)
  (if (= x2 8) #t #f))

(define (solution-1 init days)
  (if (= days 0)
      init
      (solution-1 (sort (flatten (map evolve-lanternfish init)) 8-to-the-end-sort) (- days 1))))

(display "Solution 1: ")
(length (solution-1 input 80))

(define (evolve-lanternfish-count h)
  (make-immutable-hash (list (cons 0 (hash-ref h 1))
                             (cons 1 (hash-ref h 2))
                             (cons 2 (hash-ref h 3))
                             (cons 3 (hash-ref h 4))
                             (cons 4 (hash-ref h 5))
                             (cons 5 (hash-ref h 6))
                             (cons 6 (+ (hash-ref h 7) (hash-ref h 0)))
                             (cons 7 (hash-ref h 8))
                             (cons 8 (hash-ref h 0)))))

(define (solution-2 init-hash days)
  (if (= days 0)
      (sum (hash-values init-hash))
      (solution-2 (evolve-lanternfish-count init-hash) (- days 1))))

(define init-count
  (hash-union (for/hash ([i (inclusive-range 0 8)])
                (values i 0))
              (accumulate-by-hash (lambda (x) x) length input)
              #:combine/key (lambda (k v1 v2) (+ v1 v2))))

(display "Solution 2: ")
(solution-2 init-count 256)
