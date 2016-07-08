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
  DEM <- raster(system.file('extdata', 'DEM.tif', package = 'Uppangala'))
  DEM <- crop(DEM, extent)
  # Distance to rivers generation (kept code but no just loading raster)
  # rivers <- shapefile('~/data/Env/Rivers/stream_order1.shp') # Generated under ArcGis
  # rivers.r <- rasterize(rivers, Env$DEM, field=1)
  # rivdist.r <- distance(rivers.r)
  # rivdist.r <- log(reclassify(rivdist.r, c(100,Inf,100)))
  # writeRaster(rivdist.r, 'inst/extdata/Rivers.tif', 'GTiff')
  Rivers <- raster(system.file('extdata', 'Rivers.tif', package = 'Uppangala'))
  Rivers <- crop(Rivers, extent)
  Canopy <- raster(system.file('extdata', 'Canopy.tif', package = 'Uppangala'))
  Canopy <- crop(Canopy, extent)
  return(list(DEM = DEM,
              Rivers = Rivers,
              Canopy = Canopy))
}
