#' Statewide Washington Integrated Fish Distribution
#'
#' Which fish species are found where? Download and clip by County.Data provided by WA Dept of Fish and Wildlife
#'
#' @param county_names Character vector. One or more county names (e.g., `"Columbia"`, `"Walla Walla"`).
#' @param save_path Optional character. Directory where the output Shapefile should be saved.
#'   A merged Shapefile will be saved to `swifd_<joined_counties>.shp` inside this directory.
#'
#' @return An `sf` object of SWIFD features clipped to the specified counties.
#'
#' @details
#' This function:
#' - Uses `{tigris}` to get Washington county boundaries.
#' - Downloads the full SWIFD dataset in GeoJSON format via `{httr}`.
#' - Parses the JSON using `{geojsonsf}`.
#' - Clips to the union of the selected counties, applying a small buffer to capture stream overlaps.
#' - Optionally writes the result to disk as a Shapefile.
#'
#' @examples
#' \dontrun{
#'
#' swifd_multi <- get_swifd_by_county(c("Columbia", "Garfield"))
#'
#' get_swifd_by_county(c("Columbia", "Garfield"), save_path = "data/southwest_swifd")
#' }
#'
#' @export

get_swifd_by_county <- function(county_names, save_path = NULL) {


  # Step 1: Load and match counties
  cli::cli_alert_info("Retrieving counties from TIGRIS...")
  counties_sf <- tigris::counties(state = "WA", cb = TRUE, class = "sf")
  matched <- counties_sf[counties_sf$NAME %in% county_names, ]

  if (nrow(matched) == 0) cli::cli_abort("No counties matched. Check spelling.")
  if (any(!county_names %in% matched$NAME)) {
    cli::cli_alert_warning("Some counties not found: {.val {setdiff(county_names, matched$NAME)}}")
  }

  cli::cli_alert_info("Selected counties: {.val {matched$NAME}}")

  # Step 2: Download SWIFD data
  cli::cli_alert_info("Downloading full SWIFD dataset from WDFW...")
  url <- "https://geodataservices.wdfw.wa.gov/arcgis/rest/services/MapServices/SWIFD/MapServer/0/query?where=1%3D1&text=&objectIds=&time=&timeRelation=esriTimeRelationOverlaps&geometry=&geometryType=esriGeometryPolyline&inSR=&spatialRel=esriSpatialRelIntersects&distance=&units=esriSRUnit_Foot&relationParam=&outFields=*&returnGeometry=true&returnTrueCurves=false&maxAllowableOffset=&geometryPrecision=&outSR=&havingClause=&returnIdsOnly=false&returnCountOnly=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&returnZ=false&returnM=false&gdbVersion=&historicMoment=&returnDistinctValues=false&resultOffset=&resultRecordCount=&returnExtentOnly=false&sqlFormat=none&datumTransformation=&parameterValues=&rangeValues=&quantizationParameters=&featureEncoding=esriDefault&f=geojson"

  resp <- httr::GET(url)
  if (httr::status_code(resp) != 200) cli::cli_abort("Failed to download SWIFD data. Status code: {httr::status_code(resp)}")

  geojson_text <- httr::content(resp, as = "text", encoding = "UTF-8")
  swifd_all <- geojsonsf::geojson_sf(geojson_text)

  # Step 3: Validate and transform
  cli::cli_alert_info("Validating and transforming geometries...")
  swifd_all <- sf::st_make_valid(swifd_all)
  matched <- sf::st_make_valid(matched)
  swifd_all <- sf::st_transform(swifd_all, sf::st_crs(matched))

  # Step 4: Keep only stream features entirely within the selected counties
  cli::cli_alert_info("Filtering to features entirely within county boundary...")

  swifd_crop <- sf::st_crop(swifd_all, sf::st_bbox(matched))

  # 3. Intersect geometries to retain only clipped portions
  clipped <- sf::st_intersection(swifd_crop, matched)




  # Step 5: Save as shapefile
  if (!is.null(save_path)) {
    if (!dir.exists(save_path)) dir.create(save_path, recursive = TRUE)
    file_stub <- tolower(gsub("\\s+", "_", paste(county_names, collapse = "_")))
    shp_file <- file.path(save_path, paste0("swifd_", file_stub, ".shp"))

    sf::st_write(clipped, dsn = shp_file, driver = "ESRI Shapefile", delete_dsn = TRUE)
    cli::cli_alert_success("Saved Shapefile to {.file {shp_file}}")
  }

  cli::cli_alert_success("Returned {.strong {nrow(clipped)}} SWIFD features fully within {.val {length(county_names)}} counties.")
  return(clipped)
}
