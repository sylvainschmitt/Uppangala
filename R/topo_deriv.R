#' @importFrom RSAGA rsaga.env rsaga.slope rsaga.curvature rsaga.plan.curvature rsaga.profile.curvature rsaga.aspect rsaga.fill.sinks rsaga.wetness.index
#' @importFrom raster writeRaster raster hillShade readAll
NULL

#' Create DEM derivative
#'
#' @param DEM raster. digital elevation model
#' @param choices char. choices of DEM derivative to compute
#' @param minslope num. for minslope, see help(rsaga.fill.sinks)
#' @param dir int. Direction for cosapspect computation in degree
#' @param workspace char. current workspace
#' @param path char. path is the folder of the SAGA software
#' @param method char. for methods see help(rsaga.curvature)
#'
#' @return Raster stack with chosen DEM derivative
#'
#' @author From Maxime Rejou-Mechain
#'
#' @export
#'
#' @examples
#' DEM <- genEnv()[[1]]
#' DEMderiv(DEM)
#'
DEMderiv = function(DEM, choices = c('slope', 'curvature', 'plancurvature', 'profcurvature', 'aspect',
                                     'cosaspect', 'wetness', 'flowdir', 'hillshade'),
                    minslope = 0.5, dir = 225, workspace = NULL, method = "poly2zevenbergen",
                    path = "C:/Program Files/QGIS Essen/apps/saga") {

  if(is.null(workspace)){
    dir.create('.tmp')
    workspace <- file.path(getwd(), '.tmp')
  }

  Deriv <- stack() # Result

  myenv = rsaga.env(workspace = workspace, path = path)
  writeRaster(DEM, file.path(workspace, "dem.srgd"), 'SAGA')

  ##########################
  ##### slope
  if('slope' %in% choices){
    rsaga.slope(in.dem = "dem.sgrd", out.slope = "slope.sgrd", env = myenv,
                method = method, show.output.on.console = F)
    Slope <- raster(file.path(workspace, 'slope.sdat'))
    Slope <- Slope / pi * 180
    names(Slope) <- 'Slope'
    Deriv <- stack(Deriv, Slope)
  }

  ##########################
  ##### Curvature
  if('curvature' %in% choices){
    rsaga.curvature(in.dem="dem.sgrd", out.curv="Curvature.sgrd",
                    env=myenv,method=method, show.output.on.console = F)
    Curvature <- raster(file.path(workspace, 'Curvature.sdat'))
    Deriv <- stack(Deriv, Curvature)
  }

  ##########################
  ##### PlanCurvature
  if('plancurvature' %in% choices){
    rsaga.plan.curvature(in.dem = "dem.sgrd", out.hcurv ="PlanCurvature.sgrd",
                         env = myenv, method = method, show.output.on.console = F)
    PlanCurvature <- raster(file.path(workspace, 'PlanCurvature.sdat'))
    Deriv <- stack(Deriv, PlanCurvature)
  }

  ##########################
  ##### ProfCurvature
  if('plancurvature' %in% choices){
    rsaga.profile.curvature(in.dem = "dem.sgrd", out.vcurv = "ProfileCurvature.sgrd",
                            env = myenv, method = method, show.output.on.console = F)
    ProfileCurvature <- raster(file.path(workspace, 'ProfileCurvature.sdat'))
    Deriv <- stack(Deriv, ProfileCurvature)
  }

  ##########################
  ##### Aspect & Cosaspect
  if('aspect' %in% choices || 'cosaspect' %in% choices){
    rsaga.aspect(in.dem="dem.sgrd", out.aspect="aspect.sgrd",env=myenv,
                 method=method, show.output.on.console = F)
    Aspect <- raster(file.path(workspace, 'aspect.sdat'))
    Aspect <- Aspect / pi * 180
    names(Aspect) <- 'Aspect'
    if('aspect' %in% choices){
      Deriv <- stack(Deriv, Aspect)
    }
    if('cosaspect' %in% choices){
      Cosaspect <- sin((Aspect + dir) / 180 * pi)
      names(Cosaspect) <- 'Cosaspect'
      Deriv <- stack(Deriv, Cosaspect)
    }
  }

  ##########################
  ##### Wetness
  if('wetness' %in% choices){
    rsaga.fill.sinks(in.dem="dem.sgrd",out.dem="demfilledsinks.sgrd",
                     method = "xxl.wang.liu.2006", minslope=0.5,
                     env=myenv, show.output.on.console = F)
    rsaga.wetness.index(in.dem="demfilledsinks.sgrd", out.wetness.index="wetness.sgrd",
                        env=myenv, show.output.on.console = F)
    Wetness <- raster(file.path(workspace, 'wetness.sdat'))
    names(Wetness) <- 'Wetness'
    Deriv <- stack(Deriv, Wetness)
  }

  ##########################
  ##### FlowDir
  if('flowdir' %in% choices){
      FlowDir <- terrain(DEM, 'flowdir')
      names(FlowDir) <- 'FlowDir'
      Deriv <- stack(Deriv, FlowDir)
  }

  ##########################
  ##### Hillshade
  if('hillshade' %in% choices){
    if('slope' %in% choices && 'aspect' %in% choices){
      Slope_rad <- (Slope * pi) / 180
      Aspect_rad <- (Aspect * pi) / 180
      Hillshade <- hillShade(Slope_rad, Aspect_rad)
      names(Hillshade) <- 'Hillshade'
      Deriv <- stack(Deriv, Hillshade)
    } else {
      stop('You need to compute slope and aspect to get hillshade')
    }
  }

  if(length(Deriv@layers) == 1){ # From stack to raster if only one layer
    Deriv = Deriv[[1]]
  }
  Deriv <- readAll(Deriv) # Pass raster in memory before destruction
  unlink(workspace, recursive = T, force = T)
  return(Deriv)
}
