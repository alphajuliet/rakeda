#lang racket
; main.rkt
; Develop a similar set of Ramda JS functions in Racket
; Andrew 2018-06-23 

(provide (all-defined-out))

; define/c → a pre-curried function definition
(define-syntax-rule
  (define/c (fn args ...) body)
  (define fn
    (curry (λ (args ...) body))))

(define r:curry (curry (λ (f a b) (f a b))))
(define r:curryN (curry (λ (f . args) (apply f args))))
#;(define r:c (curry (λ (f x . y) (apply f (cons x y)))))

; Threading function
; e.g. (>> 5 (r:add 2)) => 7
; Can also use ~> and ~>> from the theading package
(define (>> it . fs)
  ((apply compose (reverse fs)) it))

; Reverse two arguments
(define/c (r:flip f a b)
  (apply f (list b a)))
(define r:head first)
(define r:last last)
(define r:nth (r:flip list-ref))
(define r:index-of (r:flip index-of))

; Direct mapping
(define r:map (r:curry map))
(define r:filter (r:curry filter))
(define r:group-by (r:curry group-by))
(define r:reduce (r:curryN foldl))
(define r:reduce-right (r:curryN foldr))

; Zip functions
(define r:zip (curry map list))
(define/c (r:zip-with fn lst1 lst2)
  (map (curry apply fn) (r:zip lst1 lst2)))
(define r:flatzip (compose flatten r:zip))

; Invert arguements
(define r:take (r:curryN r:flip take))
(define r:drop (r:curryN r:flip drop))
(define r:sort (r:curryN r:flip sort))

; Various list functions
; 1-argument
(define r:flatten flatten)
(define r:reverse reverse)
(define r:uniq remove-duplicates)

; 2-argument
(define r:append (r:curry append))
(define/c (r:prepend x lst) (append (list x) lst))
(define r:count (r:curry count))
(define r:all (r:curry andmap))
(define r:any (r:curry ormap))

(define/c (r:contains? x lst)
  (if (findf (curry eq? x) lst)
      #t
      #f))
(define/c (r:find x lst)
  (if (member x lst)
      x
      #f))

; List union and intersections
(define r:union
  (compose r:uniq append))
(define/c (r:intersection lst1 lst2)
  (r:uniq (r:filter (r:curryN r:flip r:contains? lst1)
                    lst2)))

; List rotations
(define (r:rotate-left lst)
  (r:append (cdr lst)
            (list (car lst))))
(define (r:rotate-right lst)
  (r:append (list (r:last lst))
            (r:take (sub1 (length lst)) lst)))

; Nest list
; (r:nest-list fn n lst) → '((fn lst) (fn (fn list) (fn (fn (fn list)))... )
(define (r:nest-list fn n lst)
  (let ([x lst])
    (for/list ([i (range n)])
      (set! x (fn x))
      (fn x))))

; Math functions
(define r:add      (r:curry +))
(define r:subtract (r:curry -))
(define r:multiply (r:curry *))
(define r:divide   (r:curry /))
(define r:modulo   (r:curry modulo))

; The End