#' Summarize Acres of Overlay Features within HUC12 Watersheds
#'
#' @param huc12 An sf object representing HUC12 watersheds
#' @param overlay An sf object representing layers to be summarized (e.g., wetlands)
#' if overlay is polygon it calculates acres, if point it just returns a count
#' @param huc_id_col The name of the column identifying each HUC12 unit (default: "huc12")
#' @param group_vars Optional character vector of additional grouping columns from the overlay layer (e.g., c("wetland_type"))
#'
#' @return A data frame summarizing acres by HUC12 and optional groupings
#' @examples
#' \dontrun{
#'
#' huc12 <- get_geoserver_layer("all_columbia_huc_12s")
#' geohazards <- get_geoserver_layer("columbia-geologic-hazard")
#'
#' geohaz_acres <- summarize_acres_by_huc12(huc12 = huc12,
#' overlay = geohazards, group_vars = "forpehrtdc")
#' }
#'
#'
#' @export
summarize_acres_by_huc12 <- function(huc12, overlay, huc_id_col = "huc12", group_vars = NULL) {
  cli::cli_alert_info("Validating geometries...")
  huc12   <- sf::st_make_valid(huc12)
  overlay <- sf::st_make_valid(overlay)

  cli::cli_alert_info("Performing spatial intersection...")
  intersection <- sf::st_intersection(huc12, overlay)

  if (!huc_id_col %in% names(intersection)) {
    cli::cli_abort("Column {.val {huc_id_col}} not found in intersected data.")
  }

  # Check overlay geometry type
  overlay_geom_type <- unique(sf::st_geometry_type(overlay))

  grouping_columns <- c(huc_id_col, group_vars)

  if (all(overlay_geom_type %in% c("POLYGON", "MULTIPOLYGON"))) {
    # Reproject for area calculation if in geographic CRS
    if (sf::st_is_longlat(intersection)) {
      cli::cli_alert_info("Reprojecting to EPSG:5070 for accurate area calculation...")
      intersection <- sf::st_transform(intersection, 5070)
    }

    cli::cli_alert_info("Calculating area in acres...")
    intersection <- intersection %>%
      dplyr::mutate(acres = as.numeric(units::set_units(sf::st_area(.), "acre")))

    summary <- intersection %>%
      sf::st_drop_geometry() %>%
      dplyr::group_by(across(all_of(grouping_columns))) %>%
      dplyr::summarise(acres = sum(acres, na.rm = TRUE), .groups = "drop") %>%
      dplyr::arrange(dplyr::desc(acres))
  } else {
    cli::cli_alert_info("Summarizing point or line data by feature count...")
    summary <- intersection %>%
      sf::st_drop_geometry() %>%
      dplyr::group_by(across(all_of(grouping_columns))) %>%
      dplyr::summarise(count = dplyr::n(), .groups = "drop") %>%
      dplyr::arrange(dplyr::desc(count))
  }

  cli::cli_alert_success("Summary complete.")
  return(summary)
}
