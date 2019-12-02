
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
  reference <- if (is.null(reference)) lubridate::now() else reference
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

#' @export
week_to_date <- function(weeks, year = NULL) {
  index_to_date(weeks, lubridate::`week<-`, year)
}

#' @export
month_to_date <- function(months, year = NULL) {
  # Workaround for lubridate bug: months with index above 2
  # get mapped into one month before than what they should,
  # so we sum 1.
  months[months > 2] <- months[months > 2] + 1
  index_to_date(months, lubridate::`month<-`, year)
}

index_to_date <- function(indices, part, year = NULL) {
  year <- if (is.null(year)) lubridate::year(lubridate::today()) else year
  dates <- as.POSIXct(rep(g('{year}-01-01'), length(indices)))
  dates <- part(dates, indices)
  dates
}