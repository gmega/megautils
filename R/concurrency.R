#' Simple wrapper to \link{parallel::detectCores} which yields the number of
#' physical cores in Linux as well.
#' 
#' @export
realCores <- function(){
  if (Sys.info()[1] != 'Linux') {
    return(parallel::detectCores())
  }

  # Assuming the number of core ids represents the number of
  # physical cores:
  cpuinfo <- readLines('/proc/cpuinfo')
  cores <- cpuinfo[grepl('core id'), cpuinfo]
  length(unique(cores))
}
