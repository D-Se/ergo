library(ergo)


list(
  regular_if = bench::mark(
    ergo = 5 > 3 ? 10 ~ 20,
    base = if(5 > 3) 10 else 20,
    dplyr = dplyr::if_else(5 > 3, 10, 20), iterations = 1e4
  )[c(1,3,4,5,7)],
  if_else = bench::mark(
    ergo = c(T, T, F) ? c(10, 20, 30) ~ c(90, 70, 50),
    base = ifelse(c(T, T, F), c(10, 20, 30), c(90, 70, 50)),
    dplyr = dplyr::if_else(c(T, T, F), c(10, 20, 30), c(90, 70, 50)), iterations = 1e4
  )[c(1,3,4,5,7)],
  generalized_if = bench::mark(
    ergo = df$new <- ?~ 
      a > 3 ~ TRUE +
      b %in% c("c", "d", "e") ~ FALSE +
      NA,
    base = transform(df, new = ifelse(a > 3, TRUE,
                                      ifelse(b %in% c("c", "d", "e"), FALSE, NA))),
    dplyr = dplyr::mutate(df, new = dplyr::case_when(
      a > 3 ~ TRUE,
      b %in% c("c", "d", "e") ~ F,
      TRUE ~ NA
    )), check = F, iterations = 1e4
  )[c(1,3,4,5,7)],
  type_check = bench::mark(
    ergo = "5" ? int,
    base = is.integer("5"), iterations = 1e4
  )[c(1,3,4,5,7)],
  type_conversion = bench::mark(
    ergo = "5" ?~ int,
    base = as.integer("5"), iterations = 1e4
  )[c(1,3,4,5,7)]
)


df = data.frame(a = 1:5, b = letters[1:5])
a = 1:5
b = letters[1:5]
# base 120 us, dplyr ~ 1.25 ms
bench::mark(
  ergo = df$new <- ?~ 
    a > 3 ~ TRUE +
    b %in% c("c", "d", "e") ~ FALSE +
    NA,
  base = transform(df, new = ifelse(a > 3, TRUE,
                                    ifelse(b %in% c("c", "d", "e"), FALSE, NA))),
  dplyr = dplyr::mutate(df, new = dplyr::case_when(
    a > 3 ~ TRUE,
    b %in% c("c", "d", "e") ~ F,
    TRUE ~ NA
  )), check = F
)[c(1,3,4,5,7)]
