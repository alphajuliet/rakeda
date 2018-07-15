# Rakeda

Implement a subset of the functions in the Ramda JS library in Racket.  This is to provide greater support to functional programming in Racket, especially by pre-currying the functions, and in some cases reordering the arguments to be more naturally FP-like.

This may eventually become a package.

## Functions

All functions of two or more arguments are curried by default.

### Currying

* `(define/c (f...) (body))`: define a curried version of `(f...)`
* `(r:curry f a b)`: return a curried version of `(f a b)`
* `(r:curryN f args...)`: return a curried version of `(f args...)`

* `(>> x . fs)`: thread `x` through the list of functions `fs`. Essentially the
same as `~>>` from the `threading` package.

### (Mostly) existing list functions

Curried or just renamed versions of existing functions.

* `(r:flip f a b)`: reverses the arguments to `f`
* `(r:head lst)`: same as `(first lst)`
* `(r:last lst)`: same as `(last lst)`
* `(r:nth n lst)`: nth item in `lst`
* `(r:map f lst)`: curried version of `map`
* `(r:filter f lst)`: curried version of `filter`
* `(r:group-by f lst)`: curried version of `group-by`
* `(r:reduce acc init lst)`: curried version of `foldl`
* `(r:reduce-right acc init lst)`: curried version of `foldr`
* `(r:take pos lst)`: curried version of `take`, with arguments reversed
* `(r:drop pos lst)`: curried version of `drop`, with arguments reversed
* `(r:sort pos lst)`: curried version of `sort`, with arguments reversed
* `(r:flatten lst)`: same as `flatten`
* `(r:reverse lst)`: same as `reverse`
* `(r:uniq lst`): same as `remove-duplicates`
* `(r:append lst1 lst2)`: same as `append`
* `(r:count lst)`: same as `count`
* `(r:all f lst)`: same as `andmap`
* `(r:any f lst)`: same as `ormap`
* `(r:contains? x lst)`: is `x` in `lst`?
* `(r:find x lst)`: same as `member`

New functions

* `(r:index-of x lst)`: return the first position of `x` in `lst`
* `(r:prepend x lst)`: prepend `x` to `lst`

### Zipping

* `(r:zip lst1 lst2)`: zip two lists
* `(r:zip-with f lst1 lst2)`: zip two lists with a combining function
* `(r:flatzip lst1 lst2)`: zip and flatten two lists

### Union and intersection

* `(r:union lst1 lst2)`: append lists and remove duplicates
* `(r:intersection lst1 lst2)`: only return the common elements in the two
lists, without dupicates.

### Rotations

* `(r:rotate-left lst)`: rotate a list one position to the left
* `(r:rotate-right lst)`: rotate a list one position to the right

### Repeated function application

* `(r:nest-list f n lst)`: return a list of repeatedly applied results of `f` to
the list. Inspired by the Mathematica function `NestList`.

### Mathematical functions

Curried versions of existing mathematical functions, but with two arguments
only.

* (`r:add a b)`
* (`r:subtract a b)`
* (`r:multiply a b)`
* (`r:divide a b)`
* (`r:modulo a b)`

