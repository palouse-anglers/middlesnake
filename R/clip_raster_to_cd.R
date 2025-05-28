#' Clip a raster to a Conservation District in Washington State
#'
#' @param path_to_tif File path to a GeoTIFF raster
#' @param district_name Name of the Washington State Conservation District (e.g., "Columbia")
#' @description
#' [https://www.nass.usda.gov/Research_and_Science/Cropland/metadata/meta.php]
#'
#' @return A `SpatRaster` object cropped and masked to the county boundary
#' ' @example
#' \dontrun{
#'
#'
#' r_mask_2024 <- clip_raster_to_cd("../../../Downloads/2024_30m_cdls/2024_30m_cdls.tif",
#' district_name = "Columbia")
#'
#' terra::writeRaster(
#' r_mask_2024,
#' filename = "cropland_data_layer_2024.tif",
#' overwrite = TRUE,
#' filetype = "GTiff",
#' gdal = c("COMPRESS=LZW")
#' )
#'
#'
#'
#' }
#' @export
clip_raster_to_cd <- function(path_to_tif, district_name) {

  # Load raster
  r <- terra::rast(path_to_tif)


  data("swcd_boundaries", package = "middlesnake", envir = environment())

  # Filter for the specified county
  swcd <- swcd_bcoundaries %>%
    dplyr::mutate(swcd_name = sub(" CD$", "", CNSVDST)) %>%
    dplyr::mutate(swcd_name = sub(" County$", "", swcd_name)) %>%
    dplyr::filter(swcd_name %in% district_name)


  if (nrow(swcd) == 0) {
    cli::cli_abort("CD '{district_name}' not found in Washington State.")
  }

  # Get county boundary from TIGRIS and transform to raster CRS
  county <- swcd %>%
    sf::st_transform(terra::crs(r))

  # Convert to SpatVector
  county_vect <- terra::vect(county)

  # Clip: crop and mask
  r_crop <- terra::crop(r, county_vect)
  r_mask <- terra::mask(r_crop, county_vect)

  message(glue::glue("Clipped raster to {district_name}"))

  return(r_mask)
}


#' Reclassify CDL raster to 7 generalized NLCD-style land cover groups
#'
#' @param r A `SpatRaster` object representing CDL land cover codes
#'
#' @return A new `SpatRaster` with values 1–7 representing generalized land cover categories
#'         and a category table for labeling and mapping.
#'
#' [https://www.nass.usda.gov/Research_and_Science/Cropland/metadata/meta.php]
#'
#' ' @example
#' \dontrun{
#'
#' category_colors <- c(
#'   "Row Crops"        = "#FFFF64",  # Cropland yellow
#'   "Non-Crop Natural" = "#DCD939",  # Grassland/herbaceous
#'   "Specialty Crops"  = "#A8E6A0",  # Orchard/vineyard
#'   "Developed"        = "#FF0000",  # High intensity development
#'   "Water"            = "#476BA1",  # Open water
#'   "NLCD Natural"     = "#397D49",  # Evergreen forest
#'   "Other/No Data"    = "#D3D3D3"   # No data
#' )
#'
#' leaflet() %>%
#'  addTiles() %>%
#'   leaflet::addRasterImage(r_mask_2024, project = TRUE, opacity = 0.7) %>%
#'   addLegend(
#'   position = "bottomright",
#'   colors = category_colors,
#'   labels = names(category_colors),
#'   title = "Land Cover 2011",
#'   opacity = 1
#'   )
#' }
#'
#' @export
reclassify_raster <- function(r) {
  stopifnot(inherits(r, "SpatRaster"))

  # Reclassification matrix: from, to, new_class_id
  rcl <- rbind(
    c(1, 60,     1),  # Row Crops
    c(61, 65,    2),  # Non-Crop Natural
    c(66, 80,    3),  # Specialty Crops
    c(81, 81,    7),  # No Data
    c(82, 82,    1),  # Row Crops
    c(83, 83,    5),  # Water
    c(84, 84,    1),  # Row Crops
    c(85, 85,    4),  # Developed
    c(86, 109,   7),  # Undefined/missing
    c(110, 110,  5),  # Open water
    c(111, 111,  5),  # Open water
    c(112, 120,  7),  # Snow/ice/unmapped
    c(121, 124,  4),  # Developed
    c(131, 131,  2),  # Barren
    c(141, 143,  6),  # Forest
    c(152, 152,  2),  # Shrubland
    c(171, 171,  2),  # Grassland
    c(181, 181,  2),  # Pasture/hay
    c(190, 190,  6),  # Woody wetland
    c(195, 195,  6),  # Herbaceous wetland
    c(196, 254,  3),  # Specialty crops
    c(0,   0,     7)  # Background
  )

  # Apply reclassification to integers 1–7
  r_reclass <- terra::classify(r, rcl = rcl)
  r_reclass[] <- as.integer(r_reclass[])

  # Assign labels using `levels()`
  levels(r_reclass) <- data.frame(
    value = 1:7,
    label = c(
      "Row Crops",
      "Non-Crop Natural",
      "Specialty Crops",
      "Developed",
      "Water",
      "NLCD Natural",
      "Other/No Data"
    )
  )

  return(r_reclass)
}
