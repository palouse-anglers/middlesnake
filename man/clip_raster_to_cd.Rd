% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clip_raster_to_cd.R
\name{clip_raster_to_cd}
\alias{clip_raster_to_cd}
\title{Clip a raster to a Conservation District in Washington State}
\usage{
clip_raster_to_cd(path_to_tif, district_name)
}
\arguments{
\item{path_to_tif}{File path to a GeoTIFF raster}

\item{district_name}{Name of the Washington State Conservation District (e.g., "Columbia")}
}
\value{
A \code{SpatRaster} object cropped and masked to the county boundary
' @example
\dontrun{


r_mask_2024 <- clip_raster_to_cd("../../../Downloads/2024_30m_cdls/2024_30m_cdls.tif",
district_name = "Columbia")

terra::writeRaster(
r_mask_2024,
filename = "cropland_data_layer_2024.tif",
overwrite = TRUE,
filetype = "GTiff",
gdal = c("COMPRESS=LZW")
)



}
}
\description{
\link{https://www.nass.usda.gov/Research_and_Science/Cropland/metadata/meta.php}
}
