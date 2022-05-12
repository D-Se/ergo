
# ergo

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

Ergonomic tools for R.

## Installation

You can install the development version of ergo from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("D-Se/ergo")
```

## Why `ergo`?

Control flow is a basic component of any programming language. Would be a shame if it requires people to juggle braces, consider vectorization, or in general write more glyphs than needed.

``` r
if(5 > 3) 10 else 5
ifelse(1:5 > 3, 10, 5)

#becomes
5 > 3 ? 10 ~ 5
1:5 > 3 ? 10 ~ 5


is.numeric(x)
x ? num

as.numeric(x)
x ?~ num
```

