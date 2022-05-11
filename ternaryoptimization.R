`?` <- function(...){
  cat("new ? \n")
  switch(...length(),
         `?_help`(...),
         switch(length(..2),
                type_check(...), # no ..2 or no formula 5 ? 3
                type_convert(...), # starts with formula 5 ? ~ 3
                {
                if(is.data.frame(..1)) {
                  cat("In DF\n")
                  within(..1, {
                    #browser()
                    g = do.call(gif, list(...)[-1L])
                    #g = gif(...)
                    eval(g)
                  })
                   #    })
                  #gif(...)  
                }else {
                  cat("in Control Flow \n")
                  control_flow(...)
                  }
                }), # formula in body 5 ? 3 ~ 5, nested 5 ~ 3
         stop("invalid 2")
  )
}

df = data.frame(a = 1:5, b = letters[1:5])
df ?~
  a > 3 ~ TRUE +
  b %in% c("c", "d", "e") ~ FALSE +
  NA



# `?` <- function(...){
#   return(length(..2))
#   switch(...length(),
#          F,
#          gif(...)
#   )
# }
gif <- function(...){
  stringi::stri_replace_all_regex(
    ..., 
    pattern = c("~", "\\+", "((?<=~).+?$)", "^.{1}"),
    replacement = c("?", "~", "{$1}", ""),
    vectorise_all = F) |>
    str2lang()
}

x = "~a > 3 ~ T + b %in% c(\"c\", \"d\", \"e\") ~ F + NA"
# correcct
y = "a > 3 ? T ~ {b %in% c(\"c\", \"d\", \"e\") ? F ~ NA}"

stringi::stri_replace_all_regex(
  x, 
  pattern = c("~", "\\+", "((?<=~).+?$)", "^.{1}"),
  replacement = c("?", "~", "{$1}", ""),
  vectorise_all = F)

control_flow <- function(query, ...){
  if(length(query) > 1L){
    cat("calls IFELSE \n")
    do.call("ifelse", list(query, ..1[[2]], ..1[[3]]))
  } else {
    cat("calls IF \n")
    if(query) ..1[[2]] else ..1[[3]]
  }
}
# 
# control_flow <- function(query, ..., env = parent.frame()){
#   if(length(query) > 1L) {
#     do.call("ifelse", list(query, ..1[[2L]], ..1[[3L]), envir = parent.frame(2))
#   } else {
#     if(query) ..1[[2L]] else ..1[[3L]]
#   }
# }


type_check <- function(...){
 y <- as.character(substitute(...())[[2]])
 do.call(paste0("is.", full(y), collapse = ""), list(..1)) 
}

type_convert <- function(...){
  y <- as.character(..2[[2]])
  do.call(paste0("as.", full(y), collapse = ""), list(..1))
}

gif <- function(...){
  #x = as.character(substitute(...())[-1L])
  x = as.character(substitute(...()))
  
  # f <- function(...) {
  #   
  # }
  #formals(f) <- names(..1)
  # body(f) <- stringi::stri_replace_all_regex(x, 
  #                                 c("~", "\\+", "((?<=~).+?$)", "^.{1}"),
  #                                 c("?", "~", "{$1}", ""),
  #                                 FALSE ) |>
  #   str2lang()
  stringi::stri_replace_all_regex(x, 
                                  c("~", "\\+", "((?<=~).+?$)", "^.{1}"),
                                  c("?", "~", "{$1}", ""),
                                  FALSE ) |>
    str2expression()
}


df = data.frame(a = 1:5, b = letters[1:5])


df ?~ a > 3 ~ TRUE +
  b %in% c("c", "d", "e") +
  NA

f <- function(x, y, z){
  ifelse(x, y, z)
}

#e <- expression(f(a > 3, TRUE, f(b %in% c("c", "d", "e"), FALSE, NA)))
g <- function() {
  expression(f(a > 3, TRUE, f(b %in% c("c", "d", "e"), FALSE, NA)))
}


with(df, {
  eval(g())
})


