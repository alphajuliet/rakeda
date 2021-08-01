# Rakeda

Implement a subset of the functions in the Ramda JS library in Racket.  This is to provide greater support to functional programming in Racket, especially by pre-currying the functions, and in some cases reordering the arguments to be more naturally FP-like.

This may eventually become a package.

## Function definition

- define/curry
- r/curry
- r/curryN
- r/flip

## Logic functions

- true?
- false?
- true
- false
- constant
- r/eq?
- r/not-eq?

## Renamed functions

- r/all
- r/any
- r/append
- r/argmax
- r/argmin
- r/count
- r/drop
- r/head
- r/index-of
- r/index-where
- r/join
- r/last
- r/nth
- r/sort
- r/take
- r/uniq

## Curried versions of list functions

- r/filter
- r/group-by
- r/map
- r/reduce
- r/reduce-right

## Additional list functions

- r/zip
- r/zip-with
- r/flatzip
- r/random-element
- r/prepend
- r/in?
- r/find-in
- r/union
- r/intersection
- r/rotate-left
- rotate-right

## Functional patterns

- r/iterate
- r/juxt

## Curried basic math functions

- r/+
- r/-
- r/*
- r/div
- r/expt
- r/modulo
- r/=
- r/!=

