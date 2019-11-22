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
    stop(g(
      'Invalid weekday {weekday}.', 
      "Valid week days are: {str_flatten(weekday_all_labels(), ', ')}."
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

#' @export
week_to_date <- function(weeks) {
  dates <- as.POSIXct(rep('2019-01-01', length(weeks)))
  week(dates) <- weeks
  dates
}
