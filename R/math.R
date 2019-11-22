#' @export
log1pneg <- function(x) {
  pos <- x >= 0 
  not_na <- !is.na(x)
  x[pos & not_na] <- log(x[pos & not_na] + 1)
  x[!pos & not_na] <- -log(abs(x[!pos & not_na] - 1))
  x
}
