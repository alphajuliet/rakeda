#lang racket/base
; main.rkt
; A set of useful FP definitions, taken from Javascript, Clojure, and J
; Andrew 2018-06-23 

(provide (all-defined-out))

(require racket/list
         racket/string
         racket/function)

;;-----------------------
; define/curry → a pre-curried function definition
(define-syntax-rule (define/curry (fn args ...) body)
  (define fn (curry (λ (args ...) body))))

(define r/curry (curry (λ (f a b) (f a b))))
(define r/curryN (curry (λ (f . args) (apply f args))))

;;-----------------------
;; Logic functions

(define (true? x) (if x #t #f))
(define (false? x) (if x #f #t))
(define (true _) #t)
(define (false _) #f)
(define/curry (constant x _) x)

(define r/eq? (r/curry eq?))
(define r/not-eq? (r/curry (negate eq?)))

; Reverse two arguments
(define/curry (r/flip f a b)
  (apply f (list b a)))

;;-----------------------
;; Functions from J

(define (r/hook f g . x)
  ;; Hook from J
  (if (= 1 (length x))
      (f (first x) (g (first x)))
      (f (first x) (g (second x)))))

(define (r/train f g h . x)
  ;; Train from J
  (if (= 1 (length x))
      (g (f (first x)) (h (first x)))
      (g (f (first x)) (h (second x)))))

(define (r/appose f g x y)
  ;; Dyadic f&:g from J
  (f (g x) (g y)))

(define/curry (r/~ f x)
  ;; Reflexive from J (~)
  (f x x))

;;-----------------------
;; Renamed functions, curried if more than one argument, and some with arguments flipped

(define r/all (r/curry andmap))
(define r/any (r/curry ormap))
(define r/append (r/curry append))
(define r/argmax (r/curry argmax))
(define r/argmin (r/curry argmin))
(define r/count (r/curry count))
(define r/drop (r/curryN r/flip drop))
(define r/head first)
(define r/index-of (r/flip index-of))
(define r/index-where (r/flip index-where))
(define r/join (r/flip string-join))
(define r/last last)
(define r/nth (r/flip list-ref))
(define r/sort (r/curryN r/flip sort))
(define r/take (r/curryN r/flip take))
(define r/uniq remove-duplicates)

;;-----------------------
;; Curried versions of list functions

(define r/filter (r/curry filter))
(define r/group-by (r/curry group-by))
(define r/map (r/curry map))
(define r/reduce (r/curryN foldl))
(define r/reduce-right (r/curryN foldr))

(define/curry (map+ f x)
  ;; Generic map that is polymorphic across lists and values
  ;; map+ :: (a → b) → a → b
  ;; map+ :: (a → b) → [a] → [b]
  (cond
    [(list? x) (r/map f x)]
    [else (f x)]))

;;-----------------------
;; Additional list functions

(define r/zip (curry map list))

(define/curry (r/zip-with fn lst1 lst2)
  ;; r/zip-with :: (a -> b -> c) -> [a] -> [b] -> [c]
  (map (curry apply fn) (r/zip lst1 lst2)))

(define r/flatzip (compose flatten r/zip))

(define (r/random-element lst)
  (r/nth (random (length lst)) lst))

(define/curry (r/prepend x lst)
  (r/append (list x) lst))

(define/curry (r/in? x lst)
  ;; r/in? :: a -> [a] -> Boolean
  (if (findf (r/eq? x) lst) #t #f))

(define/curry (r/find-in x lst)
  ;; r/find-in :: a -> [a] -> a | False
  (if (member x lst) x #f))

(define (r/repeatedly n f)
  (for/list ([_ (in-range n)])
    (f)))

;; List union and intersections
(define r/union (compose r/uniq append))

(define/curry (r/intersection lst1 lst2)
  ;; r/intersection :: [a] -> [a] -> [a]
  (r/uniq (r/filter (r/curryN r/flip r/in? lst1) lst2)))

; List rotations
(define (r/rotate-left lst)
  ;; r/rotate-left :: [a] -> [a]
  (r/append (cdr lst)
            (list (car lst))))

(define (r/rotate-right lst)
  ;; r/rotate-right :: [a] -> [a]
  (r/append (list (r/last lst))
            (r/take (sub1 (length lst)) lst)))

(define (r/map-pair fn pairs)
  ;; Map a function over the values in a list of pairs
  (map (λ (p) (cons (car p) (fn (cdr p)))) pairs))

;;-----------------------
;; Additional hash functions

;; Create a hash from a list of keys and a list of values
;; r/create-hash :: List a -> List b -> Hash a b
(define r/create-hash
  (compose (curry apply hash) r/flatzip))

;; Map over hash values
;; r/map-hash :: (k -> v -> a) -> Hash k v -> Hash k a
(define (r/map-hash fn h)
  (r/create-hash (hash-keys h) (map fn (hash-values h))))

(define (r/filter-hash fn h)
  ;; Filter a hash based on the given key/value pair
  ;; r/filter-hash :: (k -> v -> Boolean) -> Hash k v -> Hash k v
  (for/hash ([(key value) (in-hash h)]
             #:when (fn key value))
    (values key value)))

(define (r/select-keys key-list h)
  ;; Return just the key/value pairs in the given list
  ;; r/select-keys :: List k -> Hash k v -> Hash k v
  (for/hash ([(key value) (in-hash h)]
             #:when (r/in? key key-list))
    (values key value)))

(define (r/get-in hsh path)
  ;; Get a deep item in a complex data structure
  ;; e.g. (r/get-in '(a 1 b) coll)
  (foldl (λ (r coll)
           (if (list? coll)
               (list-ref coll r)
               (hash-ref coll r)))
         hsh
         path))

;;-----------------------
;; Functional patterns

(define (r/iterate fn n x)
  ;; Apply a function n times to x
  ;; @@TODO Create instead a lazy sequence without needing n
  ;; (r/iterate fn n x) → '(x (fn x) (fn (fn x)) ... (fn^n x))
  (for/fold ([acc (list x)])
            ([i (range n)])
    (let ([y (fn (last acc))])
      (append acc (list y)))))

(define/curry (r/juxt fs x)
  ;; r/juxt :: [(a -> b)] -> a -> [b]
  (map (λ (f) (f x)) fs))

;;-----------------------
;; Curried basic math functions

(define r/+       (r/curry +))
(define r/-       (r/curry -))
(define r/*       (r/curry *))
(define r/div     (r/curry /))
(define (r/neg x) (- x))
(define r/expt    (r/curry expt))
(define r/modulo  (r/curry modulo))
(define r/squ     (r/~ *))
(define r/=       (r/curry =))
(define r/!=      (r/curry (negate =)))

; The End
