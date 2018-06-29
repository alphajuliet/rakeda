#lang racket
; rakeda.rkt
; Develop a similar set of Ramda JS functions in Racket
; Andrew 2018-06-23 

(provide (all-defined-out))

(define r:curry (curry (λ (f a b) (f a b))))
(define r:curryN (curry (λ (f . args) (apply f args))))

; Reverse two arguments
(define r:flip
  (curry (λ (f a b)
           (apply f (list b a)))))
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
(define r:zip-with
  (curry (λ (fn lst1 lst2)
           (map (curry apply fn) (r:zip lst1 lst2)))))
(define r:flatzip (compose flatten r:zip))

; Invert arguements
(define r:take (r:curryN r:flip take))
(define r:drop (r:curryN r:flip drop))
(define r:sort (r:curryN r:flip sort))

; Various list functions
; 1-argument
(define r:flatten flatten)
(define r:uniq remove-duplicates)

; 2-argument
(define r:append (r:curry append))
(define r:prepend (curry (λ (x lst) (append (list x) lst))))
(define r:count (r:curry count))
(define r:all (r:curry andmap))
(define r:any (r:curry ormap))

(define r:contains?
  (r:curry (λ (x lst)
           (if (findf (curry eq? x) lst)
               #t
               #f))))
(define r:find
  (r:curry (λ (x lst)
           (if (member x lst)
               x
               #f))))

; List union and intersections
(define r:union
  (compose r:uniq append))
(define r:intersection
  (r:curry (λ (lst1 lst2)
           (r:uniq (r:filter (r:curryN r:flip r:contains? lst1)
                             lst2)))))

; Math functions
(define r:add      (r:curry +))
(define r:subtract (r:curry -))
(define r:multiply (r:curry *))
(define r:divide   (r:curry /))
(define r:modulo   (r:curry modulo))

; The End