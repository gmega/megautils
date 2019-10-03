DEFAULT_CACHE_FOLDER <- 'megautils'

.cache_folder <- NULL

#' @export
set_cache_folder <- function(cache_folder) {
  .cache_folder <<- cache_folder
}

#' @export
cache_folder <- function() {
  if (is.null(.cache_folder)) {
    set_cache_folder(
      file.path(rappdirs::user_cache_dir(), DEFAULT_CACHE_FOLDER))
  }
  
  if (!dir.exists(.cache_folder)) {
    message(g('Creating cache folder {.cache_folder}.'))
    dir.create(.cache_folder)
  }
  
  .cache_folder
}

#' @export
cache_entry <- function(entry) {
  file.path(cache_folder(), entry)
}

#' @export
is_cached <- function(filename) {
  file.exists(cache_entry(filename))
}

#' @export
clear_entry <- function(entry) {
  entry <- cache_entry(entry)
  if (file.exists(entry)) {
    file.remove(entry)
  }
}