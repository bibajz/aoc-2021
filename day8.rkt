#lang racket

(require "utils.rkt")

(define (sort-str str comp-fn)
  (list->string (sort (string->list str) comp-fn)))

(define (parse-line line)
  (map (lambda (x) (string-split x " ")) (string-split line "|")))

(define input (map parse-line (load-and-split "input/day8.txt")))

(define (first-of-length list-of-str n)
  (car (filter (lambda (s) (= (string-length s) n)) list-of-str)))

(define (init-hash codes)
  (make-immutable-hash (list (cons 1 (first-of-length codes 2))
                             (cons 7 (first-of-length codes 3))
                             (cons 4 (first-of-length codes 4))
                             (cons 8 (first-of-length codes 7)))))

(define (filter-obvious codes)
  (filter-not (lambda (x)
                (or (= (string-length x) 2)
                    (= (string-length x) 3)
                    (= (string-length x) 4)
                    (= (string-length x) 7)))
              codes))

(define (init-solution codes)
  (list (filter-obvious codes) (init-hash codes)))

(define (solve-9 codes code-map)
  (define sol-9
    (car (filter (lambda (s) (= (string-length (string-intersect s (hash-ref code-map 4))) 4))
                 codes)))
  (list (filter-not (lambda (s) (eq? s sol-9)) codes)
        (make-immutable-hash (append (hash->list code-map) (list (cons 9 sol-9))))))

(define (solve-6 codes code-map)
  (define sol-6
    (car (filter (lambda (s)
                   (and (= (string-length (string-intersect s (hash-ref code-map 1))) 1)
                        (= (string-length s) 6)))
                 codes)))
  (list (filter-not (lambda (s) (eq? s sol-6)) codes)
        (make-immutable-hash (append (hash->list code-map) (list (cons 6 sol-6))))))

(define (solve-0 codes code-map)
  (define sol-0 (car (filter (lambda (s) (= (string-length s) 6)) codes)))
  (list (filter-not (lambda (s) (eq? s sol-0)) codes)
        (make-immutable-hash (append (hash->list code-map) (list (cons 0 sol-0))))))

(define (solve-3 codes code-map)
  (define sol-3
    (car (filter (lambda (s)
                   (and (= (string-length (string-intersect s (hash-ref code-map 7))) 3)
                        (= (string-length (string-intersect s (hash-ref code-map 9))) 5)))
                 codes)))
  (list (filter-not (lambda (s) (eq? s sol-3)) codes)
        (make-immutable-hash (append (hash->list code-map) (list (cons 3 sol-3))))))

(define (solve-5 codes code-map)
  (define sol-5
    (car (filter (lambda (s) (= (string-length (string-intersect s (hash-ref code-map 6))) 5))
                 codes)))
  (list (filter-not (lambda (s) (eq? s sol-5)) codes)
        (make-immutable-hash (append (hash->list code-map) (list (cons 5 sol-5))))))

(define (solve-2 codes code-map)
  (make-immutable-hash (append (hash->list code-map) (list (cons 2 (car codes))))))

(define (code-to-number-mapping codes)
  (inverse-map
   (apply solve-2
          (apply solve-5
                 (apply solve-3
                        (apply solve-0 (apply solve-6 (apply solve-9 (init-solution codes)))))))))


(define (solve-line codes-and-input)
  (define mapping
    (code-to-number-mapping (map (lambda (s) (sort-str s char<?)) (car codes-and-input))))
  (scalar-product (map (lambda (s) (hash-ref mapping s))
                       (map (lambda (s) (sort-str s char<?)) (cadr codes-and-input)))
                  (power-range 10 3 0 -1)))


(define input-codes (map cadr input))
(display "Solution 1: ")
(length (filter (lambda (x)
                  (or (= (string-length x) 2)
                      (= (string-length x) 3)
                      (= (string-length x) 4)
                      (= (string-length x) 7)))
                (flatten input-codes)))


(display "Solution 2: ")
(sum (map solve-line input))
