#' @importFrom sp bbox GridTopology SpatialGridDataFrame coordinates proj4string
NULL

#' Quadrats
#'
#' @param cs int. Cell size (x*y quadrats)
#' @param proj CRS. Gird CRS
#' @param area data.frame. Corners of studied area
#'
#' @return List : com int. Vector with quadrat number & grd SpatialGridDataFrame. Quadrats'grid
#' @export
#'
#' @examples
#' data(Trees)
#' quadrats(Trees$X, Trees$Y, n = 20)
#'
quadrats <- function(cs = c(20,20),
                     proj = '+proj=utm +zone=43 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0',
                     area = data.frame(x = c(571675.8, 572011.3),
                                        y = c(1385704.7, 1386010.8),
                                        row.names = c('min', 'max'))){

  # Generating the grid
  bb <- bbox(as.matrix(area))
  if(cs[1] != cs[2]){
    warning('You need square cells for RSAGA')
  }
  cc <- bb[, 1] + (cs/2)  # cell offset
  cd <- floor(diff(t(bb))/cs)  # number of cells per direction
  grd <- GridTopology(cellcentre.offset = cc, cellsize = cs, cells.dim = cd)
  grd <- SpatialGridDataFrame(grd,  dat = data.frame(id=1:prod(cd)), proj4string = proj)
  grd@data$id <- paste0('C', grd@data$id)

  return(grd)
}
