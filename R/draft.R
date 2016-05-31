# # Wood desnity & Dryades DB
# wood <- read.csv('~/data/Cleaning/test/wood.csv')
# str(wood)
# wood <- wood[c(2,4,3,5)]
# names(wood) <- c('ID', 'Sp', 'WD', 'Ref')
# wood$Ref <- as.numeric(as.character(wood$Ref))
# wood <- wood[-which(is.na(wood$Ref)),]
# wood
# # test <- wilcox.test(wood$WD, wood$Ref, paired = T)
# test <- cor.test(wood$WD, wood$Ref, method = 'spearman')
# plot(Ref ~ WD, wood, xlim = c(0,1), ylim = c(0,1), main = test$method)
# abline(0, 1)
# mtext(paste('p-value =', test$p.value, ', alternative hypothesis: true rho is not equal to 0'))

# Env <- genEnv()
# Topo <- stack(Env$DEM, DEMderiv(Env$DEM, choices = c('slope', 'curvature', 'plancurvature', 'aspect')))
# Topo$Aspect <- reclassify(Topo$Aspect, c(0, 180, 1, 180, 360, 2))
# writeRaster(Topo$Aspect, '/Users/sylvain/Desktop/Asp.tif', 'GTiff')
# plot(reclassify(Topo$Slope, c(0, 20, 0, 20, 30, 1, 30, Inf, 2)), main = 'Slope')
# contour(Topo$DEM, add = T)

# col <- terrain.colors(7)[c(1,2,5,6,3,4)]
# plot(raster(com, 'topo'), col = col, main = 'Topographical classes')
# contour(Env$DEM, add = T)
# legend('bottomleft', fill = col, legend = c('Concave surface', 'Convex plane surface', 'West steep slope', 'East steep slope', 'West mean slope', 'East mean slope'), cex = 0.7)
# col = heat.colors(100)[as.numeric(cut(Trees$WD, breaks = 100))]
# points(Y ~ X, Trees, col = "black", cex = `2013_girth`/100, pch = 20)
# points(Y ~ X, Trees, cex = `2013_girth`/100, pch = 20, col = col)

# plot(raster(com, 'WD'), main = 'CWMbin(WD)')
# contour(raster(com, 'topo'), levels = c(0:6), add = T)
