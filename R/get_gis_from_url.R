#' Download and parse GIS features from an ArcGIS REST GeoJSON URL
#'
#' This function queries an ArcGIS REST FeatureServer or MapServer URL that returns GeoJSON,
#' optionally filters it using a `where` SQL clause, and loads the result as an `sf` object.
#' Look at REST API documentation for the url layer to find acceptable variable names to query.
#' Optionally, the features can be saved to a Shapefile in the current working directory.
#'
#' @param url Character. A full ArcGIS REST API query URL returning GeoJSON (must include `f=geojson`).
#' @param save_as Optional character. Base file name (without extension) to save the result as a Shapefile.
#'   The `.shp` extension is added automatically. File is saved to the working directory.
#' @param where Optional character. SQL-like `WHERE` clause to filter features on the server
#'   (e.g., `"County = 'Columbia'"`). This will be URL-encoded and appended or replace any existing `where=`.
#'
#' @return An `sf` object with the parsed features.
#'
#' @examples
#' \dontrun{
#' # Download all features and save to shapefile
#' get_gis_from_url(
#'   url = "https://geodataservices.wdfw.wa.gov/arcgis/rest/services/MapServices/EasternWashingtonPastures/MapServer/1/query?where=1=1&outFields=*&returnGeometry=true&f=geojson",
#'   save_as = "pastures_all"
#' )
#'
#' # Filter by attribute and load only
#' get_gis_from_url(
#'   url = "https://geodataservices.wdfw.wa.gov/arcgis/rest/services/MapServices/EasternWashingtonPastures/MapServer/1/query?outFields=*&returnGeometry=true&f=geojson",
#'   where = "County = 'Columbia'"
#' )
#' }
#'
#' @export

get_gis_from_url <- function(url, save_as = NULL, where = NULL) {
  # Validate that URL requests JSON or GeoJSON
  if (!grepl("f=geojson|f=json", url, ignore.case = TRUE)) {
    cli::cli_abort("URL must request be JSON or GeoJSON (e.g., contain 'f=geojson').")
  }

  # Append or replace WHERE clause if specified
  if (!is.null(where)) {
    if (grepl("where=", url, ignore.case = TRUE)) {
      url <- sub("where=[^&]*", paste0("where=", utils::URLencode(where, reserved = TRUE)), url)
    } else {
      url <- paste0(url, ifelse(grepl("\\?", url), "&", "?"),
                    "where=", utils::URLencode(where, reserved = TRUE))
    }
  }

  cli::cli_alert_info("Requesting GIS data from {.url {url}}")
  resp <- httr::GET(url)
  if (httr::status_code(resp) != 200) {
    cli::cli_abort("Failed to download data. HTTP status: {httr::status_code(resp)}")
  }

  geojson <- httr::content(resp, as = "text", encoding = "UTF-8")
  data_sf <- geojsonsf::geojson_sf(geojson)

  cli::cli_alert_success("Parsed {.strong {nrow(data_sf)}} features from GeoJSON.")

  # Save shapefile to working directory if requested
  if (!is.null(save_as)) {
    file_stub <- tools::file_path_sans_ext(save_as)
    shp_file <- paste0(file_stub, ".shp")
    sf::st_write(data_sf, dsn = shp_file, driver = "ESRI Shapefile", delete_dsn = TRUE)
    cli::cli_alert_success("Saved Shapefile to {.file {shp_file}} in working directory.")
  }

  return(data_sf)
}
