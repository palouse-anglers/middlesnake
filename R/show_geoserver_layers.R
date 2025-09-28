#' Show available layers from a GeoServer instance
#'
#' This function retrieves and displays the available layers from a GeoServer WFS
#' (Web Feature Service) capabilities document.
#'
#' @param geoserver_url Character string specifying the base URL of the GeoServer instance
#'
#' @return A character vector of available layer names
#' @export
#'
#' @examples
#' \dontrun{
#' # Show available layers
#' layers <- show_geoserver_layers("http://my-geoserver.com/geoserver")
#' print(layers)
#' }
show_geoserver_layers <- function(
    geoserver_url = "https://geoserver.megaloptera-data.com/geoserver/Columbia/wms?service=WMS&version=1.1.0&request=GetCapabilities"
) {
  cli::cli_inform("Querying GeoServer WMS capabilities...")
  response <- httr::GET(geoserver_url)

  if (httr::http_error(response)) {
    cli::cli_abort("Failed to fetch url from GeoServer.")
  }

  xml <- xml2::read_xml(response)
  layers <- xml2::xml_find_all(xml, ".//Layer/Name")
  layer_names <- xml2::xml_text(layers)

  cli::cli_alert_success("Retrieved {.strong {length(layer_names)}} WMS layers.")
  return(layer_names)
}
