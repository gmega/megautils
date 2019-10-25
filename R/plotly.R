#' @include shortcuts.R
#' 
#' @export
add_caption <- function(x, ..., .label = 'text') {
  ellipsis::check_dots_unnamed()
  labels <- lapply(
    tidyselect::vars_select(names(x), ...), 
    function(varname) g('{varname}: {x[[varname]]}')
  )
  
  x[[.label]] <- do.call(
    str_c,
    c(labels, sep = '\n')
  )
  x
}