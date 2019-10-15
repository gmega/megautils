#' @export
l <- function(...) {
  xs <- quos(..., .named = TRUE)
  tibble:::lst_quos(xs)$output
}