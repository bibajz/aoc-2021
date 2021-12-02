#lang racket

(provide load-and-split
         accumulate
	 accumulate-right
         sum
         sliding-window)

(define (load-and-split path)
  (file->lines path))

(define (accumulate fn lst init)
  (define (inner l acc)
    (cond
      [(null? l) acc]
      [else (inner (cdr l) (fn acc (car l)))]))
  (inner lst init))

(define (accumulate-right fn lst init)
  (define (inner l acc)
    (cond
      [(null? l) acc]
      [else (inner (cdr l) (fn (car l) acc))]))
  (inner lst init))


(define (sum lst)
  (accumulate + lst 0))
(define (prod lst)
  (accumulate * lst 1))

(define (sliding-window n lst)
  (define (inner old new buffer)
    (if (null? old)
        (append new (list buffer))
        (if (= (length buffer) n)
            (inner (cdr old) (append new (list buffer)) (append (cdr buffer) (list (car old))))
            (inner (cdr old) new (append buffer (list (car old)))))))
  (inner lst '() '()))
