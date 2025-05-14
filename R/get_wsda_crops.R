#' Query WSDA Crop Landcover for a Specific Washington County
#'
#' @param county_name Name of the Washington State county (e.g., "Columbia")
#' @return An sf object with crop sections intersecting the county
#' @export
get_wsda_crops <- function(county_name) {

     # Step 1: Get the county boundary in WGS84
  wa_counties <- tigris::counties(state = "WA", year = 2023, class = "sf")

  poly <- wa_counties %>%
    filter(NAME  %in% county_name) %>%
    st_transform(4326)

  if (nrow(poly) == 0) {
    stop(paste0("County '", county_name, "' not found."))
  }

  # Step 2: Convert to Esri JSON geometry
  coords <- st_coordinates(poly)[, 1:2]
  ring <- lapply(1:nrow(coords), function(i) as.numeric(coords[i, ]))
  esri_geom <- list(
    rings = list(ring),
    spatialReference = list(wkid = 4326)
  )

  # Step 3: Construct body for POST request
  post_body <- list(
    geometry = toJSON(esri_geom, auto_unbox = TRUE),
    geometryType = "esriGeometryPolygon",
    spatialRel = "esriSpatialRelIntersects",
    outFields = "*",
    returnGeometry = "true",
    f = "geojson"
  )

  # Step 4: Send POST request
  query_url <- "https://fortress.wa.gov/agr/gis/wsdagis/rest/services/NRAS/SectionsWithCrops2023/MapServer/0/query"
  response <- httr::POST(query_url, body = post_body, encode = "form")

  # Step 5: Read GeoJSON response into sf
  if (status_code(response) == 200) {
    crop_data <- sf::st_read(content(response, "text", encoding = "UTF-8"), quiet = TRUE)
    return(crop_data)
  } else {
    stop("WSDA request failed. Status: ", status_code(response))
  }
}
