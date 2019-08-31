#' Inspect and import/cache database tables as tibbles.
#'
#' Utility functions for reading remote database tables (using SQL) into 
#' tibbles and caching them into disk for future use.
#'
#' \describe{
#'    \item{`db_table()`}{lazy reference to a database table which can 
#'    be read into memory as a tibble and cached into disk.}
#'
#'    \item{`query_table()`}{lazy reference to a query result which can
#'    be read into memory as a tibble and cached into disk.}
#'    
#'    \item{`parametric_table()`}{same as `query_table()`, but taking a
#'    parametric query and a list of named query parameters.}
#'    
#'    \item{`cached_table()`}{lazy reference to a table which has been
#'    previously cached. This exists mainly so that cached tables can 
#'    be read without setting up a database connection, which is required
#'    by all other reference types.}
#'    
#'    \item{`materialize()`}{materializes a lazy reference into an actual
#'    tibble by accessing the remote database. This function never reads
#'    or writes to the local cache.}
#'    
#'    \item{`size()`}{returns the size, in megabytes, of a lazy table reference 
#'    without materializing it. This may be useful for probing the size of a 
#'    database table before deciding to download it.}
#'    
#'    \item{`import()`}{materializes a lazy reference into an actual tibble
#'    by reading from the local cache, or calling `materialize()` and then 
#'    caching results if nothing is cached. The resulting tible will be 
#'    automatically bound to a variable with the table's `name` in the parent
#'    environment.}
#'    
#'    \item{`import_all()`}{utility function. Equivalent to looping through a
#'    set of lazy references and calling import in each.}
#'    
#'    \item{`cached_tables()`}{utility function for creating several cached
#'    table references from their names alone.}
#' }
#' 
#' @param reference a lazy table reference.
#' 
#' @param name a string containing the name of the lazy table reference. For
#'     `db_table` and `cached_table`, this has to match the name of the table in 
#'     the database and/or the name of the cache file.
#' 
#' @param conn a database connection created with `DBI::dbConnect`.
#' 
#' @param query a string containing a (possibly parametric) SQL query.
#' 
#' @param query_parameters a list of named parameters (e.g. 
#' `list(par1='value1', par2='value2')` to be substituted into the query.
#'  
#' @param ref_parameters a list of named parameters to be passed to `ref_type`
#'  with each call.
#'  
#' @param ref_type a lazy reference type. Either `db_table`, `cached_table`, or,
#' less commonly, `parametric_table` or `cached_table`.
#'
#' @rdname db
#' @export
materialize <- function(reference, ...)
  UseMethod('materialize', reference)

#' @rdname db
#' @export
size <- function(reference, ...)
  UseMethod('size', reference)

#' @rdname db
#' @export
import <- function(reference, ignore_cache = FALSE) {
  cached <- cache_file(reference)
  if (file.exists(cached) & !ignore_cache) {
    message(sprintf('- read %s from local cache', cached))
    data <- materialize.cached_table(reference)
  } else {
    message(sprintf('- fetch %s from remote source', cached))
    data <- materialize(reference) %>% collect()
    write_rds(data, cached, 'gz')
  }
  env <- parent.frame(n = length(sys.parents()))
  env[[reference$name]] <- data
  data  
}

cache_file <- function(reference) {
  sprintf('%s.rds', reference$name)
}

#' @rdname db
#' @export
cached_table <- function(name) {
  obj <- list(name = name)
  class(obj) <- c('cached_table')
  obj
}

#' @export
materialize.cached_table <- function(reference, ...) {
  cached <- cache_file(reference)
  if (!file.exists(cached)) {
    stop(sprintf('Nothing found on cache for table %s', reference$name))
  }
  read_rds(cached)
}

#' @export
size.cached_table <- function(reference, ...) {
  stop(sprintf('Size is not supported for cached tables. Materialize them
               and then inspect the size.', reference$name))
}

#' @export
print.cached_table <- function(reference) {
  sprintf(
    'Named reference to %s', reference$name
  )
}

#' @rdname db
#' @export
db_table <- function(conn, name) {
  obj <- list(name = name, conn = conn)
  class(obj) <- c('db_table', class(obj))
  obj
}

#' @export
materialize.db_table <- function(reference) {
  tbl(reference$conn, reference$name)
}

#' @export
size.db_table <- function(reference) {
  # This will only work in MariaDB. I should have a way of conveying this.
  size_query <- 
    'SELECT
        table_name AS tbl,
          round(((data_length + index_length) / 1024 / 1024), 2) size
      FROM information_schema.TABLES
      WHERE table_name = ?table'
  
  tbl(
    reference$conn, 
    sql(sqlInterpolate(reference$conn, size_query, table = reference$name))
  ) %>% collect()
}

#' @rdname db
#' @export
query_table <- function(conn, query, name) {
  obj <- list(name = name, conn = conn)
  obj$query <- query
  class(obj) <- c('query_table', class(obj))
  obj
}

#' @rdname db
#' @export
parametric_reference <- function(conn, query, name, query_parameters) {
  query_table(
    conn,
    sqlInterpolate(conn, query, .dots = query_parameters),
    name
  )
}

#' @export
materialize.query_table <- function(reference) {
  tbl(reference$conn, sql(reference$query))
}

#' @rdname db
#' @export
import_all <- function(...) {
  for (table in c(...)) {
    import(table)
  }
}

#' @rdname db
#' @export
table_references <- function(ref_type, ref_parameters = l(), ...) {
  lapply(rlang::dots_list(...), function(name) do.call(
    ref_type, args = c(name = name, ref_parameters)))
}