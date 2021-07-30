#lang racket/base
; main.rkt
; Develop a similar set of Ramda JS functions in Racket
; Andrew 2018-06-23 

(require racket/list
         racket/string
         racket/function
         "compose.rkt")

(provide (all-defined-out))

; define/curry → a pre-curried function definition
(define-syntax-rule (define/curry (fn args ...) body)
  (define fn (curry (λ (args ...) body))))

(define r/curry (curry (λ (f a b) (f a b))))
(define r/curryN (curry (λ (f . args) (apply f args))))
#;(define r/c (curry (λ (f x . y) (apply f (cons x y)))))

; Threading function
; e.g. (>> 5 (r/add 2)) => 7
; Can also use ~> and ~>> from the theading package
(define (>> it . fs)
  ((apply compose (reverse fs)) it))

(define (true? x) (if x #t #f))
(define (false? x) (if x #f #t))
(define r/eq? (r/curry eq?))

; Reverse two arguments
(define/curry (r/flip f a b)
  (apply f (list b a)))
(define r/head first)
(define r/last last)
(define r/nth (r/flip list-ref))
(define r/index-of (r/flip index-of))
(define r/index-where (r/flip index-where))

; Direct mapping
(define r/map (r/curry map))
(define r/filter (r/curry filter))
(define r/group-by (r/curry group-by))
(define r/reduce (r/curryN foldl))
(define r/reduce-right (r/curryN foldr))
(define/curry (r/juxt fs x) (map (λ (f) (f x)) fs))

; Zip functions
(define r/zip (curry map list))
(define/curry (r/zip-with fn lst1 lst2)
  (map (curry apply fn) (r/zip lst1 lst2)))
(define r/flatzip (compose flatten r/zip))

; Invert arguements
(define r/take (r/curryN r/flip take))
(define r/drop (r/curryN r/flip drop))
(define r/sort (r/curryN r/flip sort))

; Various list functions
; 1-argument
(define r/flatten flatten)
(define r/reverse reverse)
(define r/uniq remove-duplicates)
(define r/join (r/flip string-join))

; 2-argument
(define r/append (r/curry append))
(define/curry (r/prepend x lst) (append (list x) lst))
(define r/count (r/curry count))
(define r/all (r/curry andmap))
(define r/any (r/curry ormap))
(define r/argmin (r/curry argmin))
(define r/argmax (r/curry argmax))

(define/curry (r/in? x lst)
  (if (findf (r/eq? x) lst) #t #f))

(define/curry (r/find-in x lst)
  (if (member x lst) x #f))

; List union and intersections
(define r/union (compose r/uniq append))
(define/curry (r/intersection lst1 lst2)
  (r/uniq (r/filter (r/curryN r/flip r/in? lst1) lst2)))

; List rotations
(define (r/rotate-left lst)
  (r/append (cdr lst)
            (list (car lst))))
(define (r/rotate-right lst)
  (r/append (list (r/last lst))
            (r/take (sub1 (length lst)) lst)))

(define (r/iterate fn n x)
;; Iterate a function over an argument
;; (r/iterate fn n x) → '(x (fn x) (fn (fn x)) ... (fn^n x))
  (for/fold ([acc (list x)])
            ([i (range n)])
            (let ([y (fn (last acc))])
              (append acc (list y)))))

; Curried basic math functions
(define r/+       (r/curry +))
(define r/-       (r/curry -))
(define r/*       (r/curry *))
(define r/div     (r/curry /))
(define r/expt    (r/curry expt))
(define r/modulo  (r/curry modulo))

; The End
