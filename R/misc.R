#' @include shortcuts.R

#' Lagged Differences
#'
#' A version of \code{\link{diff}} which returns a vector
#' of the same length as the original (beginning with a NA),
#' and does not propagates NAs. \code{diff_na} takes the same
#' parameters as \code{\link{diff}}.
#'
#' @seealso
#' \code{\link{diff}} for valid parameters to \code{diff_na}.
#'
#' @examples
#'
#' x <- c(2, 4, 8, NA, NA, 16, 32)
#'
#' # will result in 'c(2, 2, 6, NA, NA, NA, 16)'
#' diff(x)
#'
#' # will result in 'c(NA, 2, 2, 6, NA, NA, 8, 16)'
#' diff_na(x)
#'
#' @export
diff_na <- function(x, ...) {
  defined <- !is.na(x)
  x[defined] <- c(NA, base::diff(x[defined], ...))
  x
}

#' @export
order_factor <- function(X, col) {
  cols <- names(X)
  col <- eval(substitute(select_vars(cols, col)))
  X[[col]] <- factor(X[[col]], levels = X[[col]])
  X
}

#' @export
paste_vec <- function(data, sep = '') {
  do.call(paste, c(l(sep = sep), data))
}
