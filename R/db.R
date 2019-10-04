#' Inspect and import/cache database tables as tibbles.
#'
#' Utility functions for reading remote tabular data into tibbles and caching 
#' them into disk for future use. If your data is stored in 
#' \href{https://cloud.google.com/storage}{Google Cloud Storage}, see 
#' \link{gcs_data}.
#'
#' \describe{
#'    \item{`db_table()`}{lazy reference to a database table which can 
#'    be read into memory as a tibble and cached into disk.}
#'
#'    \item{`query_table()`}{lazy reference to a query result which can
#'    be read into memory as a tibble and cached into disk.}
#'    
#'    \item{`pquery_table()`}{same as `query_table()`, but taking a
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
#'    automatically bound to a variable with the table's `name` in the global
#'    or parent environment.}
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
#' @param conn a database connection created with `DBI::dbConnect`. Can be 
#'        set to `NULL` if the user is sure there is a cached version
#'        on disk and the reference is being used with `import`.
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

table_cache <- Cache$new(cache_folder = 'table_cache')

#' @rdname db
#' @export
import <- function(reference, ignore_cache = FALSE, global = TRUE, 
                   overwrite = TRUE) {
  target_env <- if (global) sys.frame(which = 0) else parent.frame(n = 1)
  if ((reference$name %in% names(target_env)) && !overwrite) {
    message(paste(
      'element', reference$name, 'already present in target environment',
      'and overwrite set to FALSE. Skipping.'
    ))
    return()
  }
  
  entry <- cache_entry(reference)
  if (table_cache$exists(entry) & !ignore_cache) {
    message(g('- read <{reference$name}> from local cache'))
    data <- materialize.cached_table(reference)
  } else {
    message(g('- fetch <{reference$name}> from remote source'))
    data <- materialize(reference)
    if (!('data.frame' %in% class(data))) {
      stop(g('Import can only deal with data.frame-like objects,', 
             'not {do.call(paste, as.list(class(data)))}.')) 
    }
    print(table_cache$entry_path(entry))
    write_rds(data, path = table_cache$entry_path(entry), compress = 'gz')
  }
  
  target_env[[reference$name]] <- data
}

cache_entry <- function(reference) {
  g('{reference$name}.rds')
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
  entry <- cache_entry(reference)
  if (!table_cache$exists(entry)) {
    stop(g('Nothing found on cache for table {reference$name}'))
  }
  read_rds(table_cache$entry_path(entry))
}

#' @export
size.cached_table <- function(reference, ...) {
  stop(g('Size is not supported for cached tables.',
         'Materialize them and then inspect the size.'))
}

#' @export
print.cached_table <- function(reference) {
  g('Named reference to {reference$name}')
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
  tbl(reference$conn, reference$name) %>% collect()
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
  obj <- list(name = name, conn = conn, query = query)
  class(obj) <- c('query_table', class(obj))
  obj
}

#' @rdname db
#' @export
pquery_table <- function(conn, query, name, query_parameters) {
  obj <- list(name = name, 
              query = query, 
              conn = conn, 
              query_parameters = query_parameters)
  class(obj) <- c('pquery_table', class(obj))
  obj
}

#' @export
materialize.pquery_table <- function(reference) {
  query_table(
    conn = reference$conn,
    query = sqlInterpolate(
      reference$conn, 
      reference$query, 
      .dots = reference$query_parameters
    ),
    name = reference$name
  ) %>% materialize()
}

#' @export
materialize.query_table <- function(reference) {
  tbl(reference$conn, sql(reference$query)) %>% collect()
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