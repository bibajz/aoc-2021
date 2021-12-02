#lang racket

(require "utils.rkt")

(define (parse-line line)
  (cons (car (string-split line)) (string->number (cadr (string-split line)))))

(define (advance-1 instruction position)
  (case (car instruction)
    [("forward") (cons (+ (car position) (cdr instruction)) (cdr position))]
    [("up") (cons (car position) (- (cdr position) (cdr instruction)))]
    [("down") (cons (car position) (+ (cdr position) (cdr instruction)))]))

(define solution-1
  (* (car (accumulate-right advance-1 (map parse-line (load-and-split "input/day2.txt")) (cons 0 0)))
     (cdr
      (accumulate-right advance-1 (map parse-line (load-and-split "input/day2.txt")) (cons 0 0)))))

(display solution-1)
(newline)

(define (advance-2 instruction position)
  (case (car instruction)
    [("forward") (cons (+ (car position) (cdr instruction))
                       (cons (+ (cadr position) (* (cddr position) (cdr instruction)))
                             (cddr position)))]
    [("up") (cons (car position) (cons (cadr position) (- (cddr position) (cdr instruction))))]
    [("down") (cons (car position) (cons (cadr position) (+ (cddr position) (cdr instruction))))]))

(define solution-2
  (* (car (accumulate-right advance-2
                            (map parse-line (load-and-split "input/day2.txt"))
                            (cons 0 (cons 0 0))))
     (cadr (accumulate-right advance-2
                             (map parse-line (load-and-split "input/day2.txt"))
                             (cons 0 (cons 0 0))))))

(display solution-2)
(newline)
