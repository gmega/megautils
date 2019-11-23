AUTH_SCOPES = c('https://www.googleapis.com/auth/cloud-platform')
GCS_PATH_SEPARATOR = '/'
#' Access datasets from Google Cloud Storage
#' 
#' Helper functions for loading datasets from Google Cloud Storage (GCS). In case of
#' tabular data, provides functions which can be used in concert with 
#' \link[megautils]{import}.
#' 
#' \describe{
#'    \item{`gcs_data()`}{loads and parses data from GCS.}
#'    \item{`gcs_table()`}{lazy table reference for using GCS objects with 
#'          \link{import}.}
#'    \item{`gcs_auth()`}{thin wrapper over \link{gargle} and \link{googleAuthR}
#'          for faciliting user-base authentication.}
#' }
#' 
#' @param bucket a valid GCS bucket name.
#' 
#' @param path a path pointing to a dataset into the GCS bucket.
#' 
#' @param loader a post-processing function (e.g. \link{read_csv}) which
#'        transforms the downloaded file into usable data. When used with
#'        \link[megautils]{import}, `loader` must return an object with S3 class 
#'        `data.frame` (e.g. a \link{tibble}).
#' 
#' @examples
#' 
#' \dontrun{
#'    # The typical use case is importing datasets into notebooks locally.
#'    gcs_auth(email = 'user@example.com')
#'    
#'    # Places the contents of cities.csv into global object "cities", and
#'    # stores the data into local cache for future use.
#'    import(
#'       gcs_table(
#'          bucket = 'somebucket.example.com',
#'          path = '/datasets/cities.csv',
#'          loader = read_csv
#'       )
#'    )
#' }
#' 
#' 
#' @export
gcs_data <- function(bucket, path, loader) {
  tryCatch({
    target <- file.path(tempdir(), gcs_basename(path))
    if(!googleCloudStorageR::gcs_get_object(
      object_name = path,
      bucket = bucket,
      saveToDisk = target)) {
      stop(g('Failed to download {path} from bucket {bucket}.'))
    }
    loader(target)
  }, finally = {
    file.remove(target)  
  })
}

#' @rdname gcs_data
#' @export
gcs_table <- function(bucket, path, loader, name = NULL) {
  if (is.null(name)) {
    name <- gsub(pattern = '\\.[^\\.]+$', replacement = '', x = gcs_basename(path))
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

gcs_basename <- function(path) {
  parts <- stringr::str_split(path, GCS_PATH_SEPARATOR, simplify = TRUE)
  parts[length(parts)]
}