#' Load a vector (WFS) or raster (WCS) layer from GeoServer
#'
#' This function retrieves a GeoServer WFS (Web Feature Service) vector layer as an `sf` object.
#' It assumes the GeoServer layer is served in GeoJSON format.
#'
#' @param layer_name Name of the layer (without workspace prefix)
#' @param workspace Workspace name (default: `"Columbia"`)
#' @param base_url Base WFS URL (default: Columbia GeoServer endpoint)
#'
#' @return An `sf` object representing the requested vector layer, or `NULL` with a warning if the request fails.
#'
#' @section GeoServer Protocol Comparison:
#'
#' | Protocol | Full Name               | Purpose                               | Typical Data             | Access with          |
#' |----------|--------------------------|----------------------------------------|---------------------------|-----------------------|
#' | **WFS**  | Web Feature Service      | Retrieve vector features + attributes | Shapefiles, GeoJSON      | `sf::st_read()`       |
#' | **WMS**  | Web Map Service          | Display map tiles (no analysis)       | Rendered image tiles     | `leaflet::addWMSTiles()` |
#' | **WCS**  | Web Coverage Service     | Access raster data for analysis       | GeoTIFF, NetCDF, imagery | `terra::rast()`       |
#'
#' Use this function when you need **vector data** for analysis. For raster datasets (e.g., USDA cropland,lidar), use a WCS endpoint and the `terra` package.
#'
#' @examples
#' \dontrun{
#'   get_geoserver_layer("all_columbia_huc_12s")
#' }
#' @export
get_geoserver_layer <- function(layer_name,
                                workspace = "Columbia",
                                base_url = "geoserver.megaloptera-data.com/geoserver",
                                raster = FALSE) {
  full_layer <- paste0(workspace, ":", layer_name)

  if (!raster) {
    wfs_url <- paste0(
      base_url, "/", workspace, "/ows?",
      "service=WFS&version=1.1.0&request=GetFeature",
      "&typeName=", full_layer,
      "&outputFormat=application/json"
    )

    cli::cli_alert_info("Attempting to download vector layer {.val {layer_name}} via WFS")

    if (httr::http_error(wfs_url)) {
      cli::cli_alert_danger("GeoServer WFS request failed. Check layer name or server availability.")
      return(invisible(NULL))
    }

    tryCatch({
      layer_sf <- sf::st_read(wfs_url, quiet = TRUE)
      if (nrow(layer_sf) == 0) {
        cli::cli_alert_warning("Layer downloaded, but contains no features.")
      } else {
        cli::cli_alert_success("Layer downloaded: {.strong {nrow(layer_sf)}} features.")
      }
      return(layer_sf)
    }, error = function(e) {
      cli::cli_alert_danger("Failed to read WFS layer: {.emph {e$message}}")
      return(invisible(NULL))
    })
  } else {
    wcs_url <- paste0(
      base_url, "/", workspace, "/wcs?",
      "service=WCS&version=2.0.1&request=GetCoverage",
      "&coverageId=", full_layer,
      "&format=image/tiff"
    )

    cli::cli_alert_info("Attempting to download raster layer {.val {layer_name}} via WCS")

    tryCatch({
      layer_rast <- terra::rast(wcs_url)
      cli::cli_alert_success("Raster layer downloaded successfully.")
      return(layer_rast)
    }, error = function(e) {
      cli::cli_alert_danger("Failed to read WCS raster: {.emph {e$message}}")
      return(invisible(NULL))
    })
  }
}

