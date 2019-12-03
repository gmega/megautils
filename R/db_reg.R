.db_reg <- new.env()
.conn_cache <- new.env()

#' @export
db_register <- function(
  name,
  driver,
  user,
  ...,
  password = NULL
) {
  .db_reg[[name]] <- l(
    name = name, 
    driver = driver, 
    user = user, 
    pars = list(...),
    password = password
  )
}

#' @export
db_ls <- function() {
  ls(.db_reg)
}

#' @export
db_conn <- function(name, on_error = c('return_null', 'rethrow')) {
  entry <- .db_reg[[name]]
  if (is.null(entry)) {
    stop(g('No database server named {name} was found.'))
  }
  
  # Do we have a valid DB connection cached?
  conn <- .conn_cache[[name]]
  if (tryCatch(DBI::dbIsValid(conn), error = function(e) FALSE)) {
    return(conn)
  }
  
  # We'll try to open a new connection. what should we do if it fails?
  policy <- match.arg(on_error)
  
  # Fetches the password.
  password <- entry$password
  if (is.null(password)) {
    # Keyring will throw an error if the key isn't found.
    password <- keyring::key_get(
      service = name,
      username = entry$user
    )
  }
  
  # Actually attempts to connect.
  tryCatch({
    .conn_cache[[name]] <- do.call(
      DBI::dbConnect,
      c(
        l(
          drv = entry$driver,
          user = entry$user,
          password = password
        ),
        entry$pars
      )
    )
    .conn_cache[[name]]
  }, 
  error = function(e) {
    if (policy == 'rethrow') stop(e)
    NULL
  })
}

#' @export
db_close <- function(name, on_error = c('warn', 'rethrow')) {
  conn <- .conn_cache[[name]]
  # Nothing to do.
  if (is.null(conn)) {
    return()
  }
  
  policy <- match.arg(on_error)
  tryCatch({
    DBI::dbDisconnect(conn)
    # If disconnect fails, connection remains in cache.
    rm(name, .conn_cache)
  },
  error = function(e) {
    if (policy == 'rethrow') stop(e)
    warning(g('Failed to close connection to {name}: {e}.'))
  })
}