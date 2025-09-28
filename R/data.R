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


#' Washington State Counties
#'
#' A dataset containing boundary polygons for all counties in Washington State.
#' This data is derived from the US Census Bureau's TIGER/Line shapefiles.
#'
#' @format An sf object with 39 rows and the following variables:
#' \describe{
#'   \item{GEOID}{Geographic identifier for the county}
#'   \item{NAME}{County name}
#'   \item{geometry}{Spatial geometry (polygon boundaries)}
#' }
#' @source US Census Bureau TIGER/Line Shapefiles
#' @references \url{https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.html}
#' @name wa_counties
#' @docType data
"wa_counties"


#' @import sf
NULL
