#' Get List of Layers from Columbia GeoServer
#'
#' This function queries a GeoServer WMS GetCapabilities endpoint and extracts a list of available WMS layer names.
#' By default, it points to the Columbia County WA workspace.
#'
#' @param capabilities_url A character string specifying the WMS GetCapabilities URL.
#'   Defaults to the Columbia workspace on your GeoServer:
#'   \code{"http://142.93.92.104:8080/geoserver/Columbia/wms?service=WMS&version=1.1.0&request=GetCapabilities"}
#'
#' @return A character vector of layer names (e.g., \code{"Columbia:usda_2024_cropland"}).
#'
#' @examples
#' \dontrun{
#' get_wms_layers()
#' get_wms_layers("http://your-custom-server/geoserver/your-workspace/wms?...")
#' }
#' @export
show_geoserver_layers <- function(
    geoserver_url = "http://142.93.92.104:8080/geoserver/Columbia/wms?service=WMS&version=1.1.0&request=GetCapabilities"
) {
  cli::cli_inform("Querying GeoServer WMS capabilities...")
  response <- httr::GET(capabilities_url)

  if (httr::http_error(response)) {
    cli::cli_abort("Failed to fetch capabilities from GeoServer.")
  }

  xml <- xml2::read_xml(response)
  layers <- xml2::xml_find_all(xml, ".//Layer/Name")
  layer_names <- xml2::xml_text(layers)

  cli::cli_alert_success("Retrieved {.strong {length(layer_names)}} WMS layers.")
  return(layer_names)
}
