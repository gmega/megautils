#' @export
l <- list

#' @export
g <- glue

#' @export
gprint <- function(...) print(g(..., .envir = parent.frame()))