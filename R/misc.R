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

#' Concatenate a vector of strings 
#' 
#' `paste_vec` works the same as \link{paste}, but instead of taking arguments 
#' directly (i.e. using `...`) it takes them as a vector. 
#' 
#' @examples 
#' 
#' data_vec <- c('hello', 'world')
#' 
#' # Same as do.call(paste, c(data_vec, list(sep = ' ')))
#' paste_vec(data_vec, sep = ' ')
#' 
#' @export
paste_vec <- function(.dots, sep = '') {
  do.call(paste, c(l(sep = sep), .dots))
}

#' @export
gprint <- function(...) print(g(..., .envir = parent.frame()))

#' @export
log1pneg <- function(x) {
  pos <- x >= 0 
  not_na <- !is.na(x)
  x[pos & not_na] <- log(x[pos & not_na] + 1)
  x[!pos & not_na] <- -log(abs(x[!pos & not_na] - 1))
  x
}
