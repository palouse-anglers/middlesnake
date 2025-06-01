#' Download and parse GIS features from an ArcGIS REST GeoJSON URL, with optional AOI
#'
#' @param url Character. A full ArcGIS REST API query URL returning GeoJSON (must include `f=geojson` or `f=json`).
#' @param save_as Optional character. Base file name (without extension) to save the result as a Shapefile.
#' @param where Optional character. SQL-like `WHERE` clause to filter features (e.g., `"County = 'Columbia'"`).
#' @param aoi Optional `sf` polygon. If supplied, calculates an envelope for spatial filtering.
#'
#' @return An `sf` object with the parsed features.
#' @export
get_gis_from_url <- function(url, save_as = NULL, where = NULL, aoi = NULL) {
  if (!grepl("f=geojson|f=json", url, ignore.case = TRUE)) {
    cli::cli_abort("URL must request GeoJSON (e.g., contain 'f=geojson').")
  }

  # Append WHERE clause
  if (!is.null(where)) {
    if (grepl("where=", url, ignore.case = TRUE)) {
      url <- sub("where=[^&]*", paste0("where=", utils::URLencode(where, reserved = TRUE)), url)
    } else {
      url <- paste0(url, ifelse(grepl("\\?", url), "&", "?"),
                    "where=", utils::URLencode(where, reserved = TRUE))
    }
  }

  # Append geometry envelope from AOI
  if (!is.null(aoi)) {
    if (!inherits(aoi, "sf")) cli::cli_abort("AOI must be an sf polygon object.")
    aoi <- sf::st_transform(aoi, 4326)  # ensure it's in WGS84 for API
    bbox <- sf::st_bbox(aoi)
    geom_param <- paste(bbox["xmin"], bbox["ymin"], bbox["xmax"], bbox["ymax"], sep = ",")
    url <- paste0(url,
                  ifelse(grepl("\\?", url), "&", "?"),
                  "geometry=", geom_param,
                  "&geometryType=esriGeometryEnvelope",
                  "&inSR=4326&spatialRel=esriSpatialRelIntersects")
  }

  cli::cli_alert_info("Requesting GIS data from {.url {url}}")
  resp <- httr::GET(url)
  if (httr::status_code(resp) != 200) {
    cli::cli_abort("Failed to download data. HTTP status: {httr::status_code(resp)}")
  }

  geojson <- httr::content(resp, as = "text", encoding = "UTF-8")
  data_sf <- geojsonsf::geojson_sf(geojson)

  cli::cli_alert_success("Parsed {.strong {nrow(data_sf)}} features from GeoJSON.")

  if (!is.null(save_as)) {
    shp_file <- paste0(tools::file_path_sans_ext(save_as), ".shp")
    sf::st_write(data_sf, dsn = shp_file, driver = "ESRI Shapefile", delete_dsn = TRUE)
    cli::cli_alert_success("Saved Shapefile to {.file {shp_file}}.")
  }

  return(data_sf)
}
