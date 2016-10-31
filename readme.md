# BCKW interpreter

An interpreter of the four combinators: `B`, `C`, `K` and `W`.
To input/output, a Church-encoded list of Scott-encoded numbers is used.

## example

``` sh
$ echo foo | ./Main.hs <(echo '``CKK')
foo
```

``` sh
$ echo '``C``C``CKK``B``B`BW``BB``BC``B`BB`C``CKK``B`BK`C``CKK`K``CKK' > increment.bckw
$ echo abcdef | ./Main.hs increment.bckw
bcdefg
      %
```

## tutorial

The combinators act as follows:

-   `Bxyz` -> `x(yz)`
-   `Cxyz` -> `xzy`
-   `Kxy` -> `x`
-   `Wxy` -> `xyy`

And you can translate lambda expressions to terms of BCKW system:

-   `\x.x` -> `CKK`
-   `\x.y` -> `Ky`
-   `\x.MN(x)` -> `BM(\x.N(x))`
-   `\x.M(x)N` -> `C(\x.M(x))N`
-   `\x.M(x)N(x)` -> `W(\x.\y.M(y)N(x))`

## todo

-   Support parens, not only the `` ` ``.
