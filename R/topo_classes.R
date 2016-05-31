#' Topographical classes
#'
#' @param DEM num. Digital elevation model values
#' @param Slope num. Slope model values
#' @param Curvature num. Curvature model values
#' @param elev num. Discriminant elevation, autoset with NULL
#' @param degree num. Discriminant slope, autoset with NULL
#' @param dir num. Aspect direction to classify aspect in two categories (default SW for monsoon wind)
#'
#' @return char. Vector of topographical classes
#' @export
#'
#' @examples
#' data("Env")
#' data("Trees")
#' topo_classes(Trees$X, Trees$Y, Env$DEM$Ground, Env$DEM$Slope, Env$DEM$Curvature)
#'
topo_classes = function(Slope, Curvature, Aspect, degree = c(20,30), dir = 225){

  # Factorizing aspect
  AspectDir <- sin((Aspect + dir) / 180 * pi)
  AspectDir <- c(0, 1)[findInterval(AspectDir, c(-1, 0, 1))] # To change with wind true direction

  # Attributing classes
  topo <- data.frame(d = Slope, c = Curvature, a = AspectDir)
  topo$class <- NA
  topo$class[topo$d < degree[1] &  topo$c >= 0] <- 'CvP' # Concave plane surface
  topo$class[topo$d < degree[1] &  topo$c < 0] <- 'CxP' # Convex plane surface
  topo$class[topo$d >= degree[2] &  topo$a == 1] <- 'SWSS' # South - West steep slope
  topo$class[topo$d >= degree[2] &  topo$a == 0] <- 'NESS' # North - East steep slope
  topo$class[topo$d >= degree[1] & topo$d < degree[2] &  topo$a == 1] <- 'SWMS' # South - West mean slope
  topo$class[topo$d >= degree[1] & topo$d <  degree[2] & topo$a == 0] <- 'NEMS' # North - East mean slope

#   # Classes Gunatileke et al, 2006
#   topo$class = NA
#   topo$class[topo$z < elev & topo$deg < degree & topo$curv < 0] = 1  # USS
#   topo$class[topo$z < elev & topo$deg < degree & topo$curv >= 0] = 2 # USG
#   topo$class[topo$z < elev & topo$deg >= degree & topo$curv < 0] = 3 # ULS
#   topo$class[topo$z < elev & topo$deg >= degree & topo$curv >= 0] = 4 # ULG
#   topo$class[topo$z >= elev & topo$deg < degree & topo$curv < 0] = 5 # LSS
#   topo$class[topo$z >= elev & topo$deg < degree & topo$curv >= 0] = 6 # LLG
#   topo$class[topo$z >= elev & topo$deg >= degree & topo$curv < 0] = 7  # LLS
#   topo$class[topo$z >= elev & topo$deg >= degree & topo$curv >= 0] = 8 # LLG

  return(topo$class)
}
