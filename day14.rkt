#lang racket

(require racket/hash)
(require "utils.rkt")

(define (parse-insertion-rule line)
  (cons (string-trim (car (string-split line "->"))) (string-trim (cadr (string-split line "->")))))

(define polymer
  (car (takef (load-and-split "input/day14.txt") (lambda (line) (not (equal? "" line))))))

(define instruction-map
  (for/hash ([rule (map parse-insertion-rule
                        (cdr (dropf (load-and-split "input/day14.txt")
                                    (lambda (line) (not (equal? "" line))))))])
    (values (car rule) (cdr rule))))

(define (insert-by-rule str rule-map)
  (string-join
   (interleave (map string (string->list str))
               (map (lambda (rule) (hash-ref rule-map rule))
                    (map (lambda (l) (list->string l)) (sliding-window 2 (string->list str)))))
   ""))

(define (insert-mutable-hash h key v)
  (hash-set! h key v)
  (hash-ref h key))
(define (cache fn)
  (define h (make-hash '()))
  (define (inner arg1 arg2)
    (if (hash-has-key? h (cons arg1 arg2))
        (hash-ref h (cons arg1 arg2))
        (insert-mutable-hash h (cons arg1 arg2) (fn arg1 arg2))))
  inner)

; TODO: Revisit later, this implementaion is trashy - each call to `count-after-cached` builds its own cache`
(define (count-after-cached str steps instr-map)
  (define h (make-hash '()))
  (define (cache fn)
    (define (inner arg1 arg2)
      (if (hash-has-key? h (cons arg1 arg2))
          (hash-ref h (cons arg1 arg2))
          (insert-mutable-hash h (cons arg1 arg2) (fn arg1 arg2))))
    inner)

  (define (count-after s st)
    (if (= st 0)
        (counter (map string (string->list s)))
        (hash-union
         ((cache count-after) (string-join (list (string (string-ref s 0)) (hash-ref instr-map s)) "")
                              (- st 1))
         ((cache count-after) (string-join (list (hash-ref instr-map s) (string (string-ref s 1))) "")
                              (- st 1))
         (hash (hash-ref instr-map s) -1)
         #:combine/key (lambda (k v1 v2) (+ v1 v2)))))
  ((cache count-after) str steps))

(define letter-number-part1
  (sort (hash-values
         (hash-union
          (accumulate (lambda (h1 h2) (hash-union h1 h2 #:combine/key (lambda (k v1 v2) (+ v1 v2))))
                      (map (lambda (x) (count-after-cached x 10 instruction-map))
                           (map (lambda (str-list) (string-join str-list ""))
                                (sliding-window 2 (map string (string->list polymer)))))
                      (hash))
          (counter (map string (string->list (substring polymer 1 (- (string-length polymer) 1)))))
          #:combine/key (lambda (k v1 v2) (- v1 v2))))
        <))
(display "Solution 1: ")
(- (max-list letter-number-part1) (min-list letter-number-part1))

(define letter-number-part2
  (sort (hash-values
         (hash-union
          (accumulate (lambda (h1 h2) (hash-union h1 h2 #:combine/key (lambda (k v1 v2) (+ v1 v2))))
                      (map (lambda (x) (count-after-cached x 40 instruction-map))
                           (map (lambda (str-list) (string-join str-list ""))
                                (sliding-window 2 (map string (string->list polymer)))))
                      (hash))
          (counter (map string (string->list (substring polymer 1 (- (string-length polymer) 1)))))
          #:combine/key (lambda (k v1 v2) (- v1 v2))))
        <))
(display "Solution 2: ")
(- (max-list letter-number-part2) (min-list letter-number-part2))
