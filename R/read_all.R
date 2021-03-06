#' Read multiple files
#'
#' Composable utility functions for reading multiple files from a
#' given location.
#'
#' \describe{
#'    \item{`from_fs()`}{reads a set of files from the local filesystem.}
#'    
#'    \item{`from_gcs()`}{reads a set of files from a Google Cloud Storage 
#'    bucket.}
#'    
#'    \item{`using()`}{specifies which read function to apply to the downloaded
#'    data. Defaults to \link{read_csv}}
#' 
#'    \item{`into_tibble()`}{combines all files into a single
#'    Tibble. Equivalent to looping through the files, reading
#'    them, and then binding them together.}
#'
#'    \item{`into_env()`}{reads each file and assigns
#'    its contents to a key within an environment. }
#' }
#'
#' @param readfun the function which will be used to read each
#' individual file. Defaults to \link{read_csv}.
#'
#' @param ... parameters passed on to readfun (e.g. `delim = ';'`)
#' 
#' @param path filesystem path from where to read files (e.g. `/home/user/files`).
#' 
#' @param pattern a pattern to filter files by. Accepts wildcards (e.g. 
#'   `pattern = '*.csv'`). Defaults to `*` (read all files).
#'   
#' @param bucket name of the GCS bucket where to read files from.
#' 
#' @param prefix GCS prefix of the files to be read.
#'
#' @param namefun a function which takes a string and returns a
#'   string. Each file processed by `into_env` will call this
#'   function once to determine which key to use into the destination
#'   environment. The default function uses the file name without its
#'   extension and replaces `-` with `_`.
#'
#' @examples
#' 
#' # So we can use %>% syntax.
#' library(magrittr)
#'
#' # Folder with two files
#' path <- system.file('extdata', 'read_all', package = 'megautils')
#'
#' # Reads contents as single tibble.
#' schema <- list(a = readr::col_integer(), b = readr::col_integer())
#'
#' read_all() %>%
#'   using(readr::read_csv, col_types = schema) %>%
#'   from_fs(path) %>%
#'   into_tibble()
#'
#' # Reads contents into environment.
#' e <- read_all() %>%
#'    using(readr::read_csv, col_types = schema) %>%
#'    from_fs(path) %>%
#'    into_env()
#'
#' ls(e)
#'
#' @export
read_all <- function() {
  list(
    fun = read_csv
  )
}

#' @rdname read_all
#' @export
using <- function(.p, readfun = read_csv, ...) {
  .p$fun <- purrr::partial(readfun, ...)
  .p
}

#' @export
from_fs <- function(.p, path, pattern = '*') {
  .p$paths = list.files(
    path = path,
    full.names = TRUE
  )
  .p
}

#' @export
from_gcs <- function(.p, bucket, prefix) {
  .p$paths = googleCloudStorageR::gcs_list_objects(
    bucket = bucket, prefix = prefix,
  )$name
  loader <- .p$fun
  .p$fun = function(path) {
    gcs_data(bucket = bucket, path = path, loader = loader)
  }
  .p
}

#' @rdname read_all
#' @export
into_env <- function(.p, namefun = default_naming) {
  result <- new.env()
  for (path in .p$paths) {
    result[[namefun(path)]] <- .p$fun(path)
  }
  result
}

#' @rdname read_all
#' @export
into_tibble <- function(.p, namefun = default_naming, namecol = 'origin') {
  lapply(
    .p$paths,
    function(path) .p$fun(path) %>% mutate(!!namecol := namefun(path))
  ) %>% bind_rows
}

#' @export
default_naming <- function(path) {
  stringr::str_split(
    gsub('-', '_', basename(path)),
  '\\.')[[1]][1]
}


