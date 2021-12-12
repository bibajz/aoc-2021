#lang racket

(require "utils.rkt")

(define (parse-line line)
  (cons (car (string-split line "-")) (cadr (string-split line "-"))))

(define (adjacency-map list-of-tuples)
  (define (inner remaining group)
    (if (null? remaining)
        group
        (inner
         (cdr remaining)
         (hash-update
          (hash-update group (caar remaining) (lambda (x) (append (list (cdar remaining)) x)) '())
          (cdar remaining)
          (lambda (x) (append (list (caar remaining)) x))
          '()))))
  (inner list-of-tuples (hash)))

(define input (adjacency-map (map parse-line (load-and-split "input/day12.txt"))))

(define (is-upcase? str)
  (equal? (string-upcase str) str))

(define (find-paths path-hash visit-criteria-fn)
  (define (inner path-so-far already-visited)
    (if (equal? (last path-so-far) "end")
        (list path-so-far)
        (flatten-1 (for/list ([next (visit-criteria-fn already-visited
                                                       (hash-ref path-hash (last path-so-far)))])
                     (inner (append path-so-far (list next))
                            (append already-visited (if (is-upcase? next) '() (list next))))))))
  (inner (list "start") (list "start")))

(define (visit-at-most-once already-visited potential-next)
  (filter (lambda (next) (boolean? (member next already-visited)))
          (filter-not (lambda (str) (equal? str "start")) potential-next)))

(define (visit-one-at-most-twice already-visited potential-next)
  (filter (lambda (next)
            (or (boolean? (member next already-visited))
                (andmap (lambda (l) (= (length l) 1)) (group-by (lambda (x) x) already-visited))))
          (filter-not (lambda (str) (equal? str "start")) potential-next)))

(display "Solution 1: ")
(length (find-paths input visit-at-most-once))

(display "Solution 2: ")
(length (find-paths input visit-one-at-most-twice))
