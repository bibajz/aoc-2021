#lang racket

(require "utils.rkt")

(define (fold-halfplane-by-y y pos)
  (if (>= y (cdr pos)) pos (cons (car pos) (- (* 2 y) (cdr pos)))))

(define (fold-halfplane-by-x x pos)
  (if (>= x (car pos)) pos (cons (- (* 2 x) (car pos)) (cdr pos))))

(define (parse-point-line line)
  (cons (string->number (car (string-split line ",")))
        (string->number (cadr (string-split line ",")))))

(define (parse-fold-instruction-line line)
  (if (equal? (car (string-split (last (string-split line " ")) "=")) "x")
      (lambda (pos)
        (fold-halfplane-by-x (string->number (cadr (string-split (last (string-split line " ")) "=")))
                             pos))
      (lambda (pos)
        (fold-halfplane-by-y (string->number (cadr (string-split (last (string-split line " ")) "=")))
                             pos))))

(define folds
  (map parse-fold-instruction-line
       (cdr (dropf (load-and-split "input/day13.txt") (lambda (line) (not (equal? "" line)))))))

(define points
  (map parse-point-line
       (takef (load-and-split "input/day13.txt") (lambda (line) (not (equal? "" line))))))

(define (fold-paper fold-fn points)
  (remove-duplicates (map fold-fn points)))

(display "Solution 1: ")
(length (evolve (list (car (map (lambda (fn) (lambda (ps) (fold-paper fn ps))) folds))) points))

(define (swap-pair pair)
  (cons (cdr pair) (car pair)))

(define (transpose-tuples list-of-tuples)
  (map swap-pair list-of-tuples))

(define last-fold (evolve (map (lambda (fn) (lambda (ps) (fold-paper fn ps))) folds) points))

; Not really readable though. lol
(display "Solution 2: ")
(newline)
(display (string-join (map (lambda (line) (string-join line ""))
                           (batch-2d-list-update (make-2d-list (+ (max-list (map cdr last-fold)) 1)
                                                               (+ (max-list (map car last-fold)) 1)
                                                               ".")
                                                 (transpose-tuples last-fold)
                                                 (lambda (x) "#")))
                      "\n"))
(newline)
