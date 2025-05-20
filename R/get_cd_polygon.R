#' Get Conservation District Boundary Geometry for a Washington County
#'
#' @param district_name Character. Name of the district (e.g., "Columbia" or c("Columbia","Palouse")
#' @param crs Integer or character. Desired coordinate reference system (default: 4326)
#'
#' @return An sf object with the swcd boundary
#' @examples
#'
#' get_cd_polygon(c("Columbia","Palouse"))
#'
#' get_cd_polygon("Columbia")
#'
#' @export

get_cd_polygon <- function(district_name, crs = 4326) {

  # Get all counties for Washington
  #wa_counties <- tigris::counties(state = "WA", year = 2023, class = "sf")
  data("swcd_boundaries", package = "middlesnake", envir = environment())

  # Filter for the specified county
  swcd <- swcd_bcoundaries %>%
    dplyr::mutate(swcd_name = sub(" CD$", "", CNSVDST)) %>%
    dplyr::mutate(swcd_name = sub(" County$", "", swcd_name)) %>%
    dplyr::filter(swcd_name %in% district_name)

  if (nrow(swcd) == 0) {
    stop(paste0("CD '", district_name, "' not found in Washington State."))
  }

  # Transform to requested CRS
  cd_transformed <- sf::st_transform(swcd, crs = crs)

  return(cd_transformed)
}

