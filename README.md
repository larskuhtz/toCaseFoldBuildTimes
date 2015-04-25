It seems that
[`Data.Text.toCaseFold`](https://hackage.haskell.org/package/text-1.2.0.4/docs/Data-Text.html#g:8)
can cause long compilation times with recent versions of GHC. This function is
also used in the
[FoldCase](https://hackage.haskell.org/package/case-insensitive-1.2.0.4/docs/Data-CaseInsensitive.html#t:FoldCase)
instance for `Data.Text.Text`.

The code used for running the tests is [here](ToCaseFoldBuildTimes.hs).

Simple Test
===========

First we test the build of the following simple Haskell application with
different versions of GHC.

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Main

import Data.Text

main ∷ IO ()
main = do
    -- the following line is repeated n times:
    print (toCaseFold "a")
```

With `-O0` the build times scale constant in the number of calls to
`toCaseFold` in the example program, which indicates that the build times are
dominated by other factors and not the compilation of the `toCaseFold`
statement. GHC-7.8.3 shows by far the worst build times. Build times with
GHC-7.10.1 are almost back to where they are with GHC-7.6.3.

![Results with -O0 (see below)](images/results-app0-O0.png?raw=true "Results with -O0")

With `-O` the build times scale linearly with the number of calls to
`toCaseFold` for all three compiler versions, which indicates that the build
times are dominated by calls to `toCaseFold`. The linear factor is by far the
smallest for GHC-7.6.3. The factor with GHC-7.10.1 is somewhat larger than with
GHC-7.8.3.

![Results with -O (see below)](images/results-app0-O.png?raw=true "Results with -O")

With `-O2` the situation similar to the `-O` case but the compilation times are
much worse:

![Results with -O2 (see below)](images/results-app0-O2.png?raw=true "Results with -O2")

Using `Data.Text.toLower` instead of `Data.Text.toCaseFold` reduces the build
times considerably so that they scale constant in the number of calls to
`toCaseFold` (at least for the values that we considered).

Nested Test
===========

Next we test the build of the following Haskell application with different
version of GHC.

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Main
( main
) where

import qualified Data.Text as T

main :: IO ()
main = print
    -- the following line is repeated n times:
    $ T.toCaseFold . T.take 1
    $ "abc"
```




