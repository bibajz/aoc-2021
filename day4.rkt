#lang racket

(require "utils.rkt")

(define input (load-and-split "input/day4.txt"))

(define num-draws (map string->number (string-split (car input) ",")))

(define boards
  (tumbling-window 5
                   (map (compose (lambda (l) (map string->number l)) string-split)
                        (filter (lambda (line) (not (equal? line ""))) (cdr input)))))

(define (is-winning? positions board-size)
  (or (ormap (lambda (l) (= (length l) board-size)) (group-by car positions))
      (ormap (lambda (l) (= (length l) board-size)) (group-by cdr positions))))

(define (sum-unmarked board marked)
  (sum (filter (lambda (x) (not (index-of marked x))) (flatten board))))

(define (solve board draws)
  (define (inner hits ds)
    (if (null? ds)
        #f
        (if (is-winning? (filter (lambda (x) x)
                                 (map cdr (append hits (list (in-matrix-2d board (car ds))))))
                         (length board))
            (cons (length (append hits (list (in-matrix-2d board (car ds)))))
                  (* (sum-unmarked board (map car (append hits (list (in-matrix-2d board (car ds))))))
                     (car ds)))
            (inner (append hits (list (in-matrix-2d board (car ds)))) (cdr ds)))))
  (inner '() draws))

(define board-scores (sort (map (lambda (b) (solve b num-draws)) boards) < #:key car))

(display "Solution 1: ")
(car board-scores)

(display "Solution 2: ")
(last board-scores)
