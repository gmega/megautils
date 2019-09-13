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
