load_tv <- function(){
  expression(
    box::use(
      magrittr[`%>%`],
      dplyr[
        m = mutate, f = filter, s = select, z = summarise, gb = group_by,
        n,
        sw = starts_with, ew = ends_with
      ],
      rlang[`!!`, `!!!`,
            enquo,
            enquos,
            sym,
            syms]
    )
  )
}