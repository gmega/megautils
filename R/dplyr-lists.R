#' @export
filter.list <- function(X, pred) {
  f <- function(.) { }
  body(f) <- substitute(pred)
  Filter(X, f = f)
}

#' @export
pull.list <- function(X, what) {
  what <- deparse(substitute(what))
  sapply(X, function(x) { x[[what]] })
}
