#' @include shortcuts.R
#' @include lists.R

ROOT_CACHE_FOLDER <- file.path(rappdirs::user_cache_dir(), 'megautils')

Cache <- R6::R6Class("Cache", l(
  cache_folder = NULL,
  
  initialize = function(cache_folder = '') {
    cache_folder <- file.path(ROOT_CACHE_FOLDER, cache_folder)
    if (!dir.exists(cache_folder)) {
      message(g('Creating cache folder {cache_folder}.'))
      if (!dir.create(cache_folder, recursive = TRUE)) {
        stop(g('Could not initialize cache at {cache_folder}.', 
               'Check permissions.'))
      }
    }
    self$cache_folder <- cache_folder
  },
  
  entry_path = function(entry) {
    file.path(self$cache_folder, entry)
  },
  
  exists = function(entry) {
    file.exists(self$entry_path(entry))
  },
  
  clear = function() {
    for (entry in list.files(path = self$cache_folder, full.names = TRUE)) {
      file.remove(entry)
    }
  }
))
