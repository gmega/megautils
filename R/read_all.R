#' Read multiple files
#'
#' Composable utility functions for reading multiple files from a
#' given folder.
#'
#' \describe{
#'    \item{`into_tibble()`}{combines all files into a single
#'    Tibble. Equivalent to looping through the files, reading
#'    them, and then binding them together.}
#'
#'    \item{`into_env()`}{reads each file and assigns
#'    its contents to a key within an environment. }
#' }
#'
#' @param readfun the function which will be used to read each
#' individual file; e.g., `dplyr::read_csv`.
#'
#' @param ... parameters passed on to readfun.
#'
#' @param namefun a function which takes a string and returns a
#'   string. Each file processed by `into_env` will call this
#'   function once to determine which key to use into the destination
#'   environment. The default function uses the file name without its
#'   extension.
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
#' read_all(path) %>%
#'   using(readr::read_csv, col_types = schema) %>%
#'   into_tibble()
#'
#' # Reads contents into environment.
#' e <- read_all(path) %>%
#'    using(readr::read_csv, col_types = schema) %>%
#'    into_env()
#'
#' ls(e)
#'
#' @export
read_all <- function(path, pattern = '*') {
  list(
    paths = list.files(
      path = path,
      full.names = TRUE
    ),
    fun = read_csv
  )
}

#' @rdname read_all
#' @export
using <- function(.p, readfun, ...) {
  .p$fun <- purrr::partial(readfun, ...)
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
into_tibble <- function(.p) {
  lapply(
    .p$paths,
    .p$fun
  ) %>% bind_rows
}

#' @export
default_naming <- function(path) {
  stringr::str_split(
    gsub('-', '_', basename(path)),
  '\\.')[[1]][1]
}


