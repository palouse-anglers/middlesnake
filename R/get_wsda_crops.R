#' Query WSDA Crop Landcover for a Specific Washington County
#'
#' @param county_name Name of the Washington State county (e.g., "Columbia")
#' @return An sf object with crop sections intersecting the county
#'@examples
#' \dontrun{
#'
#' crops <- get_wsda_crops("Columbia)
#'
#' crop_colors <- c(
#'   "Berry" = "#E600AC",
#'   "Cereal Grain" = "#FFEABF",
#'   "Commercial Tree" = "#D6D69F",
#'   "Developed" = "#D3D3D3",
#'   "Flower Bulb" = "#FFFF75",
#'   "Green Manure" = "#FFBFE9",
#'   "Hay/Silage" = "#4D7300",
#'   "Herb" = "#005CE6",
#'   "Melon" = "#FF8080",
#'   "Nursery" = "#CC6666",
#'   "Oilseed" = "#E6E600",
#'   "Orchard" = "#FF0000",
#'   "Other" = "#CCA866",
#'   "Pasture" = "#90EE90",
#'   "Seed" = "#44876E",
#'   "Shellfish" = "#BFE9FF",
#'   "Turfgrass" = "#AACCAA",
#'   "Vegetable" = "#A52A2A",
#'   "Vineyard" = "#8B008B"
#' )
#'
#' pal <- leaflet::colorFactor(
#'   palette = crop_colors,
#'   domain = names(crop_colors),
#'   na.color = "#999999"
#' )
#'
#' leaflet(crops) %>%
#'   addTiles() %>%
#'   addPolygons(
#'     fillColor = ~pal(CropGroup),
#'     fillOpacity = 0.7,
#'     color = "#333333",
#'     weight = 0.3,
#'     label = ~CropGroup
#'   ) %>%
#'   addLegend(
#'     position = "bottomright",
#'     pal = pal,
#'     values = crops$CropGroup,
#'     title = "Crop Group",
#'     opacity = 1
#'   )
#' }
#'
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

  } else {
    stop("WSDA request failed. Status: ", status_code(response))
  }


  invalid <- which(!sf::st_is_valid(crop_data))

  if (length(invalid) > 0) {
    crops <- sf::st_make_valid(crop_data)
  } else{
    crops <- crop_data
  }

  return(crops)


}








