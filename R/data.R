#' Conservation District Boundaries for Washington State
#'
#' A simple features (sf) object containing conservation district boundaries.
#'
#' @format An `sf` data frame
#' @source Washington State GIS Open Data Portal
#' @docType data
#' @name swcd_boundaries
#' @keywords datasets
#' @docType data
"swcd_boundaries"


#' USDA Cropland Data Layer Codes
#'
#' A lookup table for CDL numeric codes and their corresponding crop names and categories.
#'
#' @format A tibble with columns:
#' \describe{
#'   \item{crop}{CDL numeric code}
#'   \item{crop_label}{Descriptive crop name}
#'   \item{category}{Optional category (e.g., grain, forest, developed)}
#' }
#' @details
#' USDA National Agricultural Statistics Service, 2024 Cropland Data Layer
#' STATEWIDE AGRICULTURAL ACCURACY REPORT
#' @source \url{https://nassgeodata.gmu.edu/CropScape/}
#' @name crop_codes
#' @docType data
"crop_codes"


#' @import sf
NULL
