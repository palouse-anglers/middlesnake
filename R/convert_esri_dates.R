#' Convert ESRI Epoch Millisecond Dates to POSIXct
#'
#' Converts numeric ESRI date values (milliseconds since 1970-01-01)
#' to standard R `POSIXct` date-time objects.
#'
#' @param x A numeric vector of ESRI date values in milliseconds.
#'
#' @return A `POSIXct` vector representing human-readable date-times.
#'
#' @examples
#' \dontrun{
#' # LastSurveyDate looks like 1.0508e+12
#' convert_esri_dates(LastSurveyDate)
#' }
#' @export

convert_esri_dates <- function(x) {
  return(as.POSIXct(x / 1000, origin = "1970-01-01", tz = "UTC"))
}
