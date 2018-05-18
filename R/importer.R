#' @include shortcuts.R

#' Utility class for sourcing scripts.
#' @export
.UtilImporter <- R6::R6Class("UtilImporter", l(
  lookup = NULL,
  initialize = function(lookup = NULL) {
    lookup <- if(is.null(lookup)) list() else lookup
    lookup <- c(lookup, list('./'))

    utils_home <- Sys.getenv('R_UTILS_HOME')
    if(utils_home != '') {
      lookup <- c(lookup, utils_home)
    }

    self$lookup <- lookup
  },

  import = function(script) {
    for(base in self$lookup) {
      path <- file.path(base, sprintf('%s.R', script))
      if(file.exists(path)) {
        source(path)
        return()
      }
    }

    stop(sprintf('%s not found in any lookup path.', script))
  }
))

#' @export
UtilImporter <- .UtilImporter$new
