#' Assign Each HUC12 Watershed to Its Dominant WRIA
#'
#' This function intersects HUC12 watersheds with Water Resource Inventory Areas (WRIAs)
#' and assigns each HUC12 to the WRIA that overlaps it the most by area. Both input layers
#' are transformed to EPSG:5070 (NAD83 / Conus Albers) for accurate area calculation.
#'
#' @param huc12_sf An `sf` object of HUC12 polygons. Must include numeric column for HUC12 values.
#' @param wria_sf An `sf` object of WRIA polygons.
#' @param huc_id_col Character name of the HUC12 ID column (e.g., "huc12").
#' @param huc_name_col Character name of the HUC12 watershed
#'
#' @return A `data.frame`
#'
#' @examples
#' \dontrun{
#' huc12_wria_table <- assign_wria_to_huc12(
#'   huc12_sf = huc12_layer,
#'   wria_sf = wria_layer,
#'   huc_id_col = "huc12",
#'   huc_name_col = "name"
#' )
#'
#' # Join to spatial layer
#' huc12_with_wria <- dplyr::left_join(huc12_layer, huc12_wria_table, by = "HUC12")
#' }
#'
#' @export
assign_wria_to_huc12 <- function(huc12_sf, wria_sf,
                                          huc_id_col = "huc12",
                                          huc_name_col = "name") {


  # Transform to projected CRS (EPSG:5070 - NAD83 / Conus Albers)
  huc12_proj <- huc12_sf %>%
    sf::st_make_valid() %>%
    sf::st_transform(crs = 5070) %>%
    dplyr::select(dplyr::all_of(c(huc_id_col, huc_name_col)))

  wria_proj <- wria_sf %>%
    sf::st_make_valid() %>%
    sf::st_transform(crs = 5070)

  # Intersect and calculate area of overlap
  intersection <- sf::st_intersection(huc12_proj, wria_proj) %>%
    dplyr::mutate(overlap_area = as.numeric(units::set_units(sf::st_area(.), "m^2")))

  # Get dominant WRIA (by largest overlap) for each HUC12
  dominant_wria <- intersection %>%
    sf::st_drop_geometry() %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(huc_id_col))) %>%
    dplyr::slice_max(overlap_area, n = 1, with_ties = FALSE) %>%
    dplyr::ungroup()

  return(as.data.frame(dominant_wria))
}
