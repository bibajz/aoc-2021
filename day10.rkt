#lang racket

(require "utils.rkt")

(define (string-reverse str)
  (list->string (reverse (string->list str))))

(define (matching-paren? paren1 paren2)
  (case paren1
    [(#\() (equal? paren2 #\))]
    [(#\)) (equal? paren2 #\()]
    [(#\[) (equal? paren2 #\])]
    [(#\]) (equal? paren2 #\[)]
    [(#\{) (equal? paren2 #\})]
    [(#\}) (equal? paren2 #\{)]
    [(#\<) (equal? paren2 #\>)]
    [(#\>) (equal? paren2 #\<)]))

(define (opening-param? c)
  (or (eq? c #\() (eq? c #\{) (eq? c #\[) (eq? c #\<)))

(define (closing-param? c)
  (or (eq? c #\)) (eq? c #\}) (eq? c #\]) (eq? c #\>)))

(define (closes-ok? back-bracket reverse-str-list)
  (define (inner back-brackets reversed)
    (if (or (null? reversed) (null? back-brackets))
        'ok
        (if (closing-param? (car reversed))
            (inner (append back-brackets (list (car reversed))) (cdr reversed))
            (if (matching-paren? (last back-brackets) (car reversed))
                (inner (without-last back-brackets) (cdr reversed))
                (car back-brackets)))))
  (inner (list back-bracket) reverse-str-list))

(define (paren-check parenstring)
  (define (inner pos)
    (if (= pos (string-length parenstring))
        'incomplete
        (if (opening-param? (string-ref parenstring pos))
            (inner (+ pos 1))
            (if (equal? (closes-ok? (string-ref parenstring pos)
                                    (string->list (string-reverse (substring parenstring 0 pos))))
                        'ok)
                (inner (+ pos 1))
                (string-ref parenstring pos)))))
  (inner 1))

(define input (load-and-split "input/day10.txt"))

(display "Solution 1: ")
(sum
 (map (lambda (c)
        (hash-ref (make-hash (list (cons #\) 3) (cons #\] 57) (cons #\} 1197) (cons #\> 25137))) c))
      (filter-not (lambda (x) (eq? x 'incomplete)) (map paren-check input))))

(define (paren-group paren)
  (case paren
    [(#\( #\)) "()"]
    [(#\{ #\}) "{}"]
    [(#\[ #\]) "[]"]
    [(#\< #\>) "<>"]))

(define (even-parens? paren-list)
  (let ([grouped (group-by (lambda (x) x) paren-list)])
    (if (= (length grouped) 1) #f (= (length (car grouped)) (length (cadr grouped))))))

(define (is-balanced? paren-str)
  (andmap even-parens? (group-by paren-group (string->list paren-str))))

(define (matching-paren paren)
  (case paren
    [(#\() #\)]
    [(#\)) #\(]
    [(#\{) #\}]
    [(#\}) #\{]
    [(#\[) #\]]
    [(#\]) #\[]
    [(#\<) #\>]
    [(#\>) #\<]))

(define (next-completion paren-str)
  (define (inner rev-list stat-hash)
    (if (closing-param? (car rev-list))
        (inner (cdr rev-list) (hash-update stat-hash (car rev-list) (lambda (v) (+ v 1))))
        (if (>= (hash-ref stat-hash (car rev-list))
                (hash-ref stat-hash (matching-paren (car rev-list))))
            (matching-paren (car rev-list))
            (inner (cdr rev-list) (hash-update stat-hash (car rev-list) (lambda (v) (+ v 1)))))))
  (inner (reverse (string->list paren-str)) (hash #\( 0 #\) 0 #\{ 0 #\} 0 #\[ 0 #\] 0 #\< 0 #\> 0)))

(define (complete-parens paren-str)
  (define (inner needed-to-complete)
    (if (is-balanced? (string-append-immutable paren-str needed-to-complete))
        needed-to-complete
        (inner (string-append needed-to-complete
                              (string (next-completion
                                       (string-append-immutable paren-str needed-to-complete)))))))
  (inner ""))

(define (weird-plus a b)
  (+ (* 5 a) b))
(define (completion-score str)
  (accumulate weird-plus
              (map (lambda (c) (hash-ref (hash #\) 1 #\] 2 #\} 3 #\> 4) c)) (string->list str))
              0))

(display "Solution 2: ")
(list-ref
 (sort (map completion-score
            (map complete-parens (filter (lambda (x) (eq? (paren-check x) 'incomplete)) input)))
       <)
 (/ (- (length (filter (lambda (x) (eq? (paren-check x) 'incomplete)) input)) 1) 2))
