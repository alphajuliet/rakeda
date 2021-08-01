#lang racket
; rakeda-test.rkt

(require rackunit
         rackunit/text-ui
         "main.rkt")

(define-test-suite rakeda-tests

  #:before
  (λ () #t)

  (test-case "Logic functions"
             (define f (r/eq? 5))
             (check-equal? (f 6) #f)
             (check-equal? (true 34) #t))

  (test-case "r/flip"
             (check-equal? (r/flip - 10 4) -6))

  (test-case "r/head r/last r/nth"
             (check-equal? (r/head '(1 2 3)) 1)
             (check-equal? (r/last '(1 2 3)) 3)
             (check-equal? (r/nth 2 '(1 2 3)) 3)
             (define f (r/nth 2))
             (check-equal? (f '(1 2 3)) 3)
             (check-equal? (r/index-of 2 '(1 2 3)) 1))
  
  (test-case "Direct mappings"
             (check-equal? (r/map add1 '(1 2 3))
                           '(2 3 4))
             (define f (r/map add1))
             (check-equal? (f '(1 2 3))
                           '(2 3 4))
             (define g (r/filter odd?))
             (check-equal? (g '(1 2 3))
                           '(1 3))
             (check-equal? (r/reduce + 1 '(1 2 3)) 7))
  (test-case "r/zip"
             (check-equal? (r/zip '(1 2) '(3 4))
                           '((1 3) (2 4))))
  (test-case "r/zip-with"
             (check-equal? (r/zip-with + '(1 2) '(3 4))
                           '(4 6)))
  (test-case "r/flatzip"
             (check-equal? (r/flatzip '(1 2) '(3 4))
                           '(1 3 2 4)))
  (test-case "r/juxt"
             (define fns (list (r/+ 2) (r/* 2)))
             (check-equal? (r/juxt fns 10) '(12 20)))
  
  (test-case "r/take r/drop"
             (check-equal? (r/take 2 '(1 2 3))
                           '(1 2))
             (check-equal? (r/drop 2 '(1 2 3 4))
                           '(3 4)))
  (test-case "r/any and r/all"
             (check-equal? (r/any odd? '(1 2 3)) #t)
             (define f (r/any odd?))
             (check-equal? (f '(2 4 6)) #f)
             (check-equal? (r/all odd? '(1 2 3)) #f)
             (check-equal? (r/all odd? '(1 3 5)) #t))

  (test-case "r/sort"
             (check-equal? (r/sort < '(3 2 1))
                           '(1 2 3)))
  (test-case "r/uniq"
             (check-equal? (r/uniq '(5 1 2 2 3 4 4))
                           '(5 1 2 3 4)))
  (test-case "r/prepend"
             (check-equal? (r/prepend 1 '(2 3))
                           '(1 2 3))
             (check-equal? (r/prepend 1 '()) '(1)))
  (test-case "r/in?"
             (check-equal? (r/in? 2 '(1 2 3)) #t)
             (check-equal? (r/in? 4 '(1 2 3)) #f)
             (check-equal? (r/find-in 2 '(1 2 3)) 2)
             (check-equal? (r/find-in 5 '(1 2 3)) #f))

  (test-case "Set functions"
             (check-equal? (r/union '(1 2 3) '(2 3 4))
                           '(1 2 3 4))
             (check-equal? (r/intersection '(1 2 3) '(2 3 4))
                           '(2 3)))

  (test-case "r/count"
             (check-equal? (r/count odd? '(1 2 3)) 2))

  (test-case "Rotations"
             (check-equal? (r/rotate-left '(1 2 3))
                           '(2 3 1))
             (check-equal? (r/rotate-right '(1 2 3))
                           '(3 1 2)))

  (test-case "Iterate function application"
             (check-equal? (r/iterate r/rotate-left 2 '(1 2 3))
                           '((1 2 3) (2 3 1) (3 1 2))))

  (test-case "Math"
             (check-equal? (r/+ 2 3) 5)
             (define inc (r/+ 1))
             (check-equal? (inc 2) 3))

  (test-case "Composition"
             (define f (compose (r/+ 1) (r/* 3)))
             (check-equal? (f 4) 13))
  )

(run-tests rakeda-tests)
