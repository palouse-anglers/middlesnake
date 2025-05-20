#' Get County Boundary Geometry for a Washington County
#'
#' @param county_name Character. Name of the county (e.g., "Columbia" or c("Columbia","Asotin)
#' @param crs Integer or character. Desired coordinate reference system (default: 4326)
#'
#' @return An sf object with the county boundary
#' @export

get_county_polygon <- function(county_name, crs = 4326) {

  # Get all counties for Washington
  #wa_counties <- tigris::counties(state = "WA", year = 2023, class = "sf")
  data("wa_counties", package = "middlesnake", envir = environment())

  # Filter for the specified county
  county <- dplyr::filter(wa_counties, NAME %in% county_name)

  if (nrow(county) == 0) {
    cli::cli_abort("County '{county_name}' not found in Washington State.")
  }

  # Transform to requested CRS
  county_transformed <- sf::st_transform(county, crs = crs)

  return(county_transformed)
}
