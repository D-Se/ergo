
# ergo

<!-- badges: start -->
[![R-CMD-check](https://github.com/D-Se/ergo/workflows/R-CMD-check/badge.svg)](https://github.com/D-Se/ergo/actions)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

Ergonomic tools for R.

## Installation
At the console, run `r devtools::install_github("D-Se/ergo")`

## Why `ergo`?

- minimize hand travel distance
- maximize recall

### Control flow
In natural language an important concept is usually easy to express. `ergo` re-purposes the `?` operator and combines it with R's formula `~` interface to reduce glyphs needed.

|                   Base R                  | Tidyverse                                     |             **Ergo**            |
|:-----------------------------------------:|-----------------------------------------------|:-------------------------------:|
|           `if(5 > 3) 10 else 5`           | -                                             |         `5 > 3 ? 10 ~ 5`        |
|          `ifelse(1:5 > 3, 10, 5)`         | `if_else(1:5 > 3, 10, 5)`                     |        `1:5 > 3 ? 10 ~ 5`       |
| `ifelse(x>3, 5, ifelse(x>2, 10, 20))` | `case_when(x>3 ~ 5, x>2 ~ 10, TRUE ~ 20)` | `x>3 ? 5 ~ {x>2 ? 10 ~ 20}` |

### Type checks

|            Base R           |          **Ergo**          |
|:---------------------------:|:--------------------------:|
|      `is.numeric("5")`      |        `"5" ? num`         |

### Type conversion

|            Base R           |          **Ergo**          |
|:---------------------------:|:--------------------------:|
|      `as.numeric("5")`      |        `"5" ?~ num`        |
| `as.list(e, sorted = TRUE)` | `e ?~ lst [sorted = TRUE]` |




