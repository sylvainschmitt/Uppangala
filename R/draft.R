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

# library(vegan)
# pca <- princomp(~ LA + WD + Thick + LDMC + SLA, data = PFT_ind, cor = T)
# lambda <- pca$sdev * sqrt(nrow(pca$scores))
# plot(t(t(pca$scores[,c(2,3)])/lambda),pch=16, col = 'lightgrey',
#      xlab = paste('Axe 2 -', round(lambda[2]/sum(lambda)*100, 2), '%'),
#      ylab = paste('Axe 3 -', round(lambda[3]/sum(lambda)*100, 2), '%'))
# par(new=T)
# Rot <- t(t(pca$loadings[,c(2,3)])*lambda)
# XLIM <- c(-max(abs(Rot[,1])),max(abs(Rot[,1])))
# XLIM <- XLIM+(XLIM*0.2)
# plot(Rot,col=4,axes=FALSE,xlim=XLIM,ylim=XLIM,pch="",xlab = "", ylab = "")
# lines(XLIM, c(0,0), lty = 2)
# lines(c(0,0), XLIM, lty = 2)
# col <- c('green', "blue", "firebrick")[c(2, 3, 2, 3, 1)]
# arrows (rep(0,nrow(pca$loadings)),rep(0,nrow(pca$loadings)),Rot[,1],Rot[,2],col=col,lwd=2)
# text (Rot[,1:2], pos = c(4, 2, 4, 2, 3), rownames(Rot),col=col,cex=1.2)
# axis (3)
# axis (4)
# legend('topright', c('Acquisitive species',
#                      'Conservative species',
#                      'Leaf resource capture and defense'),
#        text.col = c("blue", "firebrick", "green"))

# library(rgl)
# plot3d(pca$scores[,1:3])
# text3d(pca$loadings[,1:3], texts=rownames(pca$loadings), col="red")
# coords <- NULL
# for (i in 1:nrow(pca$loadings)) {
#   coords <- rbind(coords, rbind(c(0,0,0),pca$loadings[i,1:3]))
# }
# lines3d(coords, col="red", lwd=4)

# library(spgrass6)
# loc <- initGRASS('C:/Program Files/QGIS Essen/apps/grass/grass-7.0.4',
#                  tempdir(), override = T)
# execGRASS(cmd='r.watershed', flags='overwrite',
#           parameters =  list(elevation='val_srtm30@flood', threshold='2000',
#                              drainage='wat30_drain', accumulation='wat30_accu', basin='wat30__basin',
#                              stream='wat30__stream', memory='2000'))
