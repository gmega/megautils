#' @export
weekday_find_closest <- function(weekday, current = NULL) {
  current <- if (is.null(current)) lubridate::now() else current
  weekday <- if (!is.numeric(weekday)) weekday_index(weekday) else weekday
  current - lubridate::make_difftime(
    day = (as.POSIXlt(current)$wday - weekday) %% 7)
}

#' @export
weekday_index <- function(weekday) {
  index <- which(str_to_lower(weekday) == weekday_all_labels())
  if (length(index) == 0) {
    stop(sprintf(
      'Invalid weekday %s. Valid week days are: %s.',
      weekday,
      str_flatten(weekday_all_labels(), ', ')
    ))
  }
  index - 1
}

#' @export
weekday_all_labels <- function() {
  now <- lubridate::now()
  offset <- as.POSIXlt(now)$wday
  weekdays(
    now + lubridate::make_difftime(day = 0:6 - offset)
  )
}