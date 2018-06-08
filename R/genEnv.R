#' @include genTrees.R
#' @importFrom raster shapefile raster crop resample hillShade stack extent
NULL

#' Environmental data generation
#'
#' Generates environmental data set from external data extracted from the database
#'
#' @param area data.frame. Corners of studied area
#'
#' @return Environmental data set
#' @export
#'
#' @examples
#' summary(genEnv())
#'
genEnv <- function(area = data.frame(x = c(571675.8, 572011.3),
                                     y = c(1385704.7, 1386010.8),
                                     row.names = c('min', 'max'))){
  # Opening
  extent <- extent(t(as.matrix(area)))
  DEM <- raster(file.path('data', 'DEM.tif'))
  DEM <- crop(DEM, extent)
  Canopy <- raster(file.path('data', 'Canopy.tif'))
  Canopy <- crop(Canopy, extent)
  return(list(DEM = DEM, Canopy = Canopy))
}
