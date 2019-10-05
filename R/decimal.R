new_decimal <- function(n = integer(), d = integer()) {
  vec_assert(n, ptype = integer())
  vec_assert(d, ptype = integer())

  new_rcrd(list(n = n, d = d), class = "decimal")
}

decimal <- function(n, d) {
  c(n, d) %<-% vec_cast_common(n, d, .to = integer())
  c(n, d) %<-% vec_recycle_common(n, d)

  new_decimal(n, d)
}


format.decimal <- function(x, ...) {
  n <- field(x, "n")
  d <- field(x, "d")

  out <- n/d
  out <- signif(out, 3)
  out <- formatC(paste0(out))
  out[is.na(n) | is.na(d)] <- NA

  out
}

vec_ptype_abbr.decimal <- function(x, ...) "dec"
vec_ptype_full.decimal <- function(x, ...) "decimal"
