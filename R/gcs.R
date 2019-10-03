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
      stop('Failed to download %s from bucket %s.', path, bucket)
    }
    loader(tmp)
  }, finally = {
    file.remove(tmp)  
  })
}

#' @rdname gcs_data
#' @export
gcs_table <- function(bucket, path, loader) {
  obj <- list(
    bucket = bucket, 
    path = path, 
    loader = loader, 
    # FIXME not sure this is very portable.
    name = basename(path)
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
gcs_auth <- function() {
  token <- gargle::token_fetch(scopes = AUTH_SCOPES)
  googleAuthR::gar_auth(scopes = AUTH_SCOPES, token)
}