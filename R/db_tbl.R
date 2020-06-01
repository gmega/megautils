#' @export
db_tbl <- function(name) {
  tbl(do.call(db_conn, default_db()), name)
}

#' @export
db_tbl_sql <- function(query, ...) {
  conn <- do.call(db_conn, default_db())
  tbl(conn, sql(DBI::sqlInterpolate(conn, query, ...)))
}

#' @export
set_default_db <- function(
  default_db_name, 
  default_on_error = c('return_null', 'rethrow')
) {
  action <- match.arg(default_on_error)
  globals$default_db <- l(name = default_db_name, on_error = action)
}

#' @export
default_db <- function() {
  default <- globals$default_db
  if (is.null(default)) {
    stop('A default db has not been set.')
  }
  default
}
