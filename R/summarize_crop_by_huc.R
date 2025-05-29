#' Summarize Crop Types Within HUC12 Watersheds
#'
#' This function summarizes raster-based cropland data within each HUC12 polygon,
#' returning acreage by crop type per watershed.
#'
#' @param crops_rast A `SpatRaster` object representing crop types (e.g., from CDL)
#' @param huc12 An `sf` object of HUC12 watershed boundaries with a unique "name" column
#'
#' @return A `tibble` with columns: ID, name, crop, crop_name, n_pixels, acres
#' @export
summarize_crops_by_huc12 <- function(crops_rast, huc12) {
  # Load crop code CSV from package

  data("crop_codes", package = "middlesnake", envir = environment())

  huc12 <- sf::st_transform(huc12,
                            crs = terra::crs(crops_rast)) %>%
    dplyr::mutate(ID = dplyr::row_number())

  # Convert huc12 to terra vect
  huc12_vect <- terra::vect(huc12)

  # Crop and mask raster
  crops_cropped <- terra::crop(crops_rast, huc12_vect)
  crops_masked <- terra::mask(crops_cropped, huc12_vect)

  # Extract values and summarize
  extracted_vals <- terra::extract(crops_masked, huc12_vect, ID = TRUE)

  crop_summary <- extracted_vals %>%
    dplyr::filter(!is.na(tiff)) %>%
    dplyr::count(ID, crop = tiff, name = "n_pixels") %>%
    dplyr::mutate(acres = n_pixels * 0.222394)

  # Join HUC names
  crop_summary_named <- crop_summary %>%
    dplyr::left_join(huc12 %>% sf::st_drop_geometry(), by = "ID") %>%
    dplyr::relocate(ID, name, crop, n_pixels, acres)

  # Add crop names from codebook
  crop_summary_named <- crop_summary_named %>%
    dplyr::left_join(crop_codes, by = c("crop" = "Code")) %>%
    dplyr::rename(crop_name = Type) %>%
    dplyr::relocate(crop_name, .after = crop)

  return(crop_summary_named)
}
