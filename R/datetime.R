#' Find the closest date matching a day of the week
#' 
#' Given a reference date object and a weekday as a string or numeric index, finds
#' the closest instance of that weekday that happened before the reference date.
#' 
#' For instance, if the reference date is a Friday and the weekday is "Saturday",
#' the function will return last Saturday. 
#'
#' @param weekday a day of the week, as a string or an index. These are 
#' locale-specific. Use \link{weekday_labels} to figure out the valid 
#' labels for your system.
#' 
#' @param reference a reference date from where to look for the required 
#' weekday.
#' 
#' @export
weekday_find_closest <- function(weekday, reference = NULL) {
  reference <- if (is.null(reference)) lubridate::now() else as.POSIXct(reference)
  weekday <- if (!is.numeric(weekday)) weekday_index(weekday) else weekday
  reference - lubridate::make_difftime(
    day = (as.POSIXlt(reference)$wday - weekday) %% 7)
}

#' Tells if a given date falls on a weekend or not.
#' 
#' @param a date or datetime object.
#'
#' @export
is_weekend <- function(date) {
  lubridate::wday(date) %in% c(6, 7)
}

#' @export
weekday_index <- function(weekday) {
  index <- which(str_to_lower(weekday) == weekday_labels())
  if (length(index) == 0) {
    stop(g(
      'Invalid weekday {weekday}.', 
      "Valid week days are: {str_flatten(weekday_all_labels(), ', ')}."
    ))
  }
  index - 1
}

#' @export
weekday_labels <- function() {
  now <- lubridate::now()
  offset <- as.POSIXlt(now)$wday
  weekdays(
    now + lubridate::make_difftime(day = 0:6 - offset)
  )
}

#' Convert datetime to weekday label
#' 
#' Returns locale-specific day-of-the-week labels for a date/datetime vector.
#' 
#' @seealso weekday_labels
#' 
#' @examples
#' 
#' datetime <- c('2020-04-23', '2020-04-24')
#' weekday_label(datetime)
#' 
#' @export
weekday_label <- function(datetime) {
  weekday_labels()[lubridate::wday(datetime)]
}

#' @export
week_to_date <- function(weeks, year = NULL) {
  index_to_date(weeks, lubridate::`week<-`, year)
}

#' @export
month_to_date <- function(months, year = NULL) {
  # Can't use `month<-` from lubridate as it generates the 
  # wrong months for some reason.
  years <- rep(NA, length(months))
  years[] <- year
  as.POSIXct(g('{year}-{months}-01'))
}

index_to_date <- function(indices, part, year = NULL) {
  year <- if (is.null(year)) lubridate::year(lubridate::today()) else year
  dates <- as.POSIXct(rep(NA, length(indices)))
  dates[] <- g('{year}-01-01')
  dates <- part(dates, indices)
  dates
}