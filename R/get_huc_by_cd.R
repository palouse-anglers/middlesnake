#' Load Washington HUC Watersheds by Conservation District
#'
#' This function downloads Watershed Boundary Dataset (WBD) HUC polygons
#' for a given county in Washington and clips HUC polygon to the conservation district
#' provided. Optionally saves the results as a shapefile. Clipped acres of
#' each watershed are automatically recalculated and added as `Acres_in_huc` variable
#'
#' @param district_name Character. Name of the district (e.g., "Columbia").
#' @param huc_level Character. One of "huc02" "huc04" "huc06" "huc08" "huc10" "huc12" "huc12_nhdplusv2"
#' @param save_shp Logical. Whether to save the result as a shapefile. Default is FALSE.
#' @param shp_path Character. File path to save shapefile (without extension). Default is NULL.
#'
#' @return An `sf` object with watershed polygons assigned to the input county.
#' @export
#'
#' @examples
#'\dontrun{
#'
#'  asotin_huc12s <- get_huc_by_cd("Asotin",
#'  huc_level = "huc12")
#'
#'   leaflet() %>%
#'   addTiles() %>%
#'   addPolygons(data=asotin_huc12s,label=~name)
#'
#'  get_huc_by_cd("Asotin",
#'  huc_level = "huc12",
#'  save_shp = TRUE,
#'  shp_path = "../../../Documents/asotin_huc12_clipped")
#'
#' }
#'
get_huc_by_cd <- function(district_name,
                              huc_level = "huc12",
                              save_shp = FALSE,
                              shp_path = NULL) {



  data("swcd_boundaries", package = "middlesnake", envir = environment())

  # Filter for the specified county
  swcd <- swcd_bcoundaries %>%
    dplyr::mutate(swcd_name = sub(" CD$", "", CNSVDST)) %>%
    dplyr::mutate(swcd_name = sub(" County$", "", swcd_name)) %>%
    dplyr::filter(swcd_name %in% district_name) %>%
    sf::st_transform(5070)


  if (nrow(swcd) == 0) {
    cli::cli_abort("CD '{district_name}' not found in Washington State.")
  }

  cli::cli_alert_info("Downloading {huc_level} boundaries from Watershed Boundary Dataset...")
  huc_data <- nhdplusTools::get_huc(AOI = swcd,type = huc_level) %>%
    sf::st_transform(5070)

  if (is.null(huc_data) || nrow(huc_data) == 0) {
    cli::cli_abort("No {huc_level} boundaries returned. Check inputs or USGS availability.")
  }

  cli::cli_alert_info("Assigning HUCs to {district_name}...")
  huc_assigned <- sf::st_intersection(huc_data,swcd) %>%
    mutate(acres_in_huc =as.numeric(units::set_units(st_area(.), "acre")),
         huc_level=huc_level) %>%
        dplyr::select(swcd_name,name,huc12,acres_in_huc,huc_level,dplyr::everything()) %>%
    sf::st_transform(4326)

  cli::cli_alert_success("Downloaded and processed {nrow(huc_assigned)} HUC polygons.")

  if (save_shp) {
    if (is.null(shp_path)) {
      shp_path <- paste0("huc_", huc_level, "_", tolower(district_name))
    }

    cli::cli_alert_info("Saving shapefile to {.file {shp_path}}...")
    huc_assigned %>%
    st_transform(4326) %>%
    st_write(paste0(shp_path, ".shp"), delete_dsn = TRUE)
    cli::cli_alert_success("Shapefile saved.")
  }

  return(huc_assigned)
}
