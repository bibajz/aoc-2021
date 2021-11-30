#lang racket

(provide load-and-split)

(define (load-and-split path) (file->lines path))
