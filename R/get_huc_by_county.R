#' Load Washington HUC Watersheds by County
#'
#' This function downloads Watershed Boundary Dataset (WBD) HUC polygons
#' for a given county in Washington and clips HUC polygon to the county
#' provided. Optionally saves the results as a shapefile. Clipped acres of
#' each watershed are automatically recalculated and added as `Acres_in_huc` variable
#'
#' @param county_name Character. Name of the county (e.g., "Columbia").
#' @param huc_level Character. One of "huc8", "huc10", "huc12". Default is "huc12".
#' @param save_shp Logical. Whether to save the result as a shapefile. Default is FALSE.
#' @param shp_path Character. File path to save shapefile (without extension). Default is NULL.
#'
#' @return An `sf` object with watershed polygons assigned to the input county.
#' @export
#'
#' @examples
#'\dontrun{
#'
#'  asotin_huc12s <- get_huc_by_county("Asotin",
#'  huc_level = "huc12")
#'
#'   leaflet() %>%
#'   addTiles() %>%
#'   addPolygons(data=asotin_huc12s,label=~name)
#'
#'  get_huc_by_county("Asotin",
#'  huc_level = "huc12",
#'  save_shp = TRUE,
#'  shp_path = "../../../Documents/asotin_huc12_clipped")
#'
#' }
#'
get_huc_by_county <- function(county_name,
                              huc_level = "huc12",
                              save_shp = FALSE,
                              shp_path = NULL) {


  cli::cli_alert_info("Fetching county boundaries from TIGRIS...")
  wa_counties <- tigris::counties(state = "WA", cb = TRUE, year = 2021, class = "sf") %>%
    sf::st_transform(5070)

  county <- wa_counties %>%
    filter(tolower(NAME) == tolower(county_name))

  if (nrow(county) == 0) {
    cli::cli_abort("County {.val {county_name}} not found in Washington State.")
  }

  cli::cli_alert_info("Downloading {huc_level} boundaries from WBD...")
  huc_data <- nhdplusTools::get_huc(AOI = county,type = huc_level) %>%
    sf::st_transform(5070)

  if (is.null(huc_data) || nrow(huc_data) == 0) {
    cli::cli_abort("No {huc_level} boundaries returned. Check inputs or USGS availability.")
  }

  cli::cli_alert_info("Assigning HUCs to {county_name} County...")
  huc_assigned <- sf::st_intersection(huc_data,county) %>%
    mutate(Acres_in_huc =as.numeric(units::set_units(st_area(.), "acre")))%>%
    sf::st_transform(4326)

  cli::cli_alert_success("Downloaded and processed {nrow(huc_assigned)} HUC polygons.")

  if (save_shp) {
    if (is.null(shp_path)) {
      shp_path <- paste0("huc_", huc_level, "_", tolower(county_name))
    }

    cli::cli_alert_info("Saving shapefile to {.file {shp_path}}...")
    huc_assigned %>%
    st_transform(4326) %>%
    st_write(paste0(shp_path, ".shp"), delete_dsn = TRUE)
    cli::cli_alert_success("Shapefile saved.")
  }

  return(huc_assigned)
}
