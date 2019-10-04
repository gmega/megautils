AUTH_SCOPES = c('https://www.googleapis.com/auth/cloud-platform')
#' Access datasets from Google Cloud Storage
#' 
#' Helper functions for loading datasets from Google Cloud Storage. In case of
#' tabular data, provides functions which can be used in concert with 
#' \link{import}.
#' 
#' @export
gcs_data <- function(bucket, path, loader) {
  tmp <- tempfile()
  tryCatch({
    if(!gcs_get_object(
      object_name = path,
      bucket = bucket,
      saveToDisk = tmp)) {
      stop(g('Failed to download {path} from bucket {bucket}.'))
    }
    loader(tmp)
  }, finally = {
    file.remove(tmp)  
  })
}

#' @rdname gcs_data
#' @export
gcs_table <- function(bucket, path, loader, name = NULL) {
  if (is.null(name)) {
    # FIXME not sure using basename is the way to go.
    name <- gsub(pattern = '\\.[^\\.]+$', replacement = '', x = basename(path))
  }
  obj <- list(
    bucket = bucket, 
    path = path, 
    loader = loader, 
    name = name
  )
  class(obj) <- c('gcs_table', class(obj))
  obj
}

#' @rdname gcs_data
#' @export
materialize.gcs_table <- function(reference) {
  do.call(gcs_data, reference[1:(length(reference) - 1)])
}

#' @rdname gcs_data
#' @export
gcs_auth <- function(...) {
  token <- gargle::token_fetch(scopes = AUTH_SCOPES, ...)
  googleAuthR::gar_auth(scopes = AUTH_SCOPES, token)
}