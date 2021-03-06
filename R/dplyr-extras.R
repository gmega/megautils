#' @export
select_opt <- function(.data, ...) {
  exprs <- rlang::exprs(...)
  optional <- mandatory <- c()
  for (expr in exprs) {
    if (is.call(expr)) {
      flat <- as.list(expr)
      if (flat[[1]] == '?') {
        optional <- c(deparse(flat[[2]]), optional)
        next
      }
    }
    mandatory <- c(expr, mandatory)
  }

  .data %>% select(
    !!!mandatory, intersect(optional, names(.data))
  )
}

#' @export
with_colnames <- function(.tbl, ..., .names = c()) {
  .names <- if (length(.names) == 0) list(...) else .names
  colnames(.tbl) <- .names
  .tbl
}

#' @export
filter_cols <- function(.tbl, ...) {
  vals <- list(...)
  vars <- names(vals)
  if (length(vars) == 0) { return(.tbl) }
  for (i in 1:length(vars)) {
    .tbl <- .tbl %>%
      filter(!!as.symbol(vars[i]) == !!vals[[i]])
  }
  .tbl
}

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

#' @export
order_columns <- function(X, ...) {
  selected <- dplyr::select_vars(names(X), ...)
  unselected <- setdiff(names(X), selected)
  X %>% select(
    c(selected, unselected)
  )
}

#' @export
rename_columns <- function(X, ...) {
  colnames(X) <- list(...)
  X
}

#' @export
t.tbl <- function(X, cols = 'columns') {
  t_colnames <- names(X)
  Y <- X %>% t.data.frame %>% as_tibble
  Y[[cols]] <- t_colnames
  Y %>% order_columns(initial = c(cols))
}

#' @export
count_defined <- function(X, ...) {
  cols <- dplyr::select_vars(names(X), ...)
  X %>% summarise_at(vars(cols), funs(sum(!is.na(.))))
}

#' @export
if_na <- function(X, default) {
  X[is.na(X)] <- default
  X
}

#' @export
unnest <- function(X) {
  for (colname in names(X)) {
    column <- X[[colname]]
    if (is.matrix(column)) {
      X[[colname]] <- as.vector(column[,1])
    }
  }
  X
}
