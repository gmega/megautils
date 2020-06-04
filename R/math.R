#' @export
log1pneg <- function(x) {
  pos <- x >= 0 
  not_na <- !is.na(x)
  x[pos & not_na] <- log(x[pos & not_na] + 1)
  x[!pos & not_na] <- -log(abs(x[!pos & not_na] - 1))
  x
}

#' Computes rolling statistics
#' 
#' Computes a rolling statistic `stat` over windows of size `n` taken from
#' vector `x`.
#' 
#' @param x a vector
#' @param n a window size 
#' @stat stat an aggregation function which returns a value for each window
#' 
#' @examples 
#' 
#'   x <- c(1, 0, 1, 1, 0, 1, 0, 0, 0)
#'   # returns 2 2 2 2 1 1 0
#'   roll_stat(x, n = 3, sum)
#' 
#' @export
roll_stat <- function(x, n, stat) {
  sapply(1:(length(x) - n + 1), function(i) stat(x[i:(i + n - 1)]))
}