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

# StratInf <- merge(PFT_sp, Species)
# AOV <- with(StratInf, list(Thick = aov(Thick ~ Strata),
#                            LA = aov(LA ~ Strata),
#                            LDMC = aov(LDMC ~ Strata),
#                            SLA = aov(SLA ~ Strata),
#                            WD = aov(WD ~ Strata)))
# lapply(AOV, summary)

# # Data
# library(Uppangala)
# library(raster)
# library(ade4)
# Env <- genEnv()
# Deriv <- DEMderiv(Env$DEM, c('slope', 'curvature', 'aspect', 'wetness'))
# Deriv$SW <- cos(Deriv$Aspect + 220)
# Deriv <- Deriv[[-3]]
#
# # Individuals
# TreesXY <- Trees[-which(is.na(Trees$X)),]
# TreesXY <- TreesXY[-which(is.na(TreesXY$`2013_girth`)),]
# TreesXY <- TreesXY[-which(is.na(extract(Env$DEM, TreesXY[2:3], df = T)[2])),]
# TreesXY <- TreesXY[-which(TreesXY$SpCode %in% levels(droplevels(PFT_sp[which(is.na(PFT_sp$WD)),1]))),]
#
# # Matrices
# Lm <- TreesXY[1]
# Lm <- data.frame(tapply(rep(1, length(Lm[,1])), list(row.names(Lm), Lm$SpCode), sum))
# Lm[is.na(Lm)] <- 0
# Qm <- PFT_sp[which(names(PFT_sp) %in% c('SLA', 'LDMC', 'WD', 'Thick', 'LA'))]
# row.names(Qm) <- PFT_sp$Sp_Code
# Qm <- Qm[-which(is.na(Qm$WD)),]
# Qm <- Qm[names(Lm),]
# Rm <- cbind(extract(Deriv, TreesXY[2:3], df = T)[-1], TreesXY$`2013_girth`)
# names(Rm)[5] <- 'Girth'
# row.names(Rm) <- row.names(Lm)
#
# # RLQ
# L <- dudi.coa(Lm, scannf = FALSE)
# R <- dudi.pca(Rm, scannf = FALSE, row.w = L$lw)
# Q <- dudi.pca(Qm, scannf = FALSE, row.w = L$cw)
# rlq <- rlq(R, L, Q, scannf = FALSE)
# rd <- randtest(rlq)
# fq <- fourthcorner.rlq(rlq, type = "Q.axes")
# fr <- fourthcorner.rlq(rlq, type = "R.axes")
# fqr <- fourthcorner(Rm, Lm, Qm)
# fqr.adj <- p.adjust.4thcorner(fqr, p.adjust.method.G = "fdr", p.adjust.method.D = "fdr")
#
# # Results
# summary(rlq)
# plot(rlq)
# s.arrow(rlq$l1)
# s.arrow(rlq$c1)
# s.label(rlq$lQ, boxes = FALSE)
# plot(fqr.adj, alpha = 0.5, stat = "D2")
# plot(fqr.adj, x.rlq = rlq, alpha = 0.5, stat = "D2", type = "biplot")
# plot(fq, alpha = 0.1, stat = "D2")
# plot(fr, alpha = 0.1, stat = "D2")
# print(fq, stat = 'D')
# print(fr, stat = 'D')

# # Moran I {ape}
# CWMab <- com_abund@data[c('LES', 'SLA', 'WES', 'WD', 'LDMC', 'Thick', 'LA')]
# CWMab.dist <- as.matrix(dist(coordinates(com_abund)))
# CWMab.dist.inv <- 1/CWMab.dist
# diag(CWMab.dist.inv) <- 0
# Moran.I(CWMab$LES, CWMab.dist.inv)
# CWMab.Moran <- lapply(as.list(CWMab), Moran.I, weight = CWMab.dist.inv)
# CWMab.Moran <- do.call(rbind.data.frame, CWMab.Moran)
# CWMab.resid <- lapply(LM$Abundance, residuals)
# CWMab.resid.Moran <- lapply(CWMab.resid, Moran.I, weight = CWMab.dist.inv)
# CWMab.resid.Moran <- do.call(rbind.data.frame, CWMab.resid.Moran)

# # Phylogenetic tree
# library(phytools)
# library(Uppangala)
# path <- 'C:/Users/sylvain/Downloads/Phylogeny/'
# source(file.path(path, 'R_codes for function S.PhyloMaker.txt'))
# Species <- genSpecies()
# phylo <- read.tree(file.path(path, 'PhytoPhylo.tre'))
# nodes <- read.csv(file.path(path, 'nodes.csv'), h = T)
# sp <- Species[PFT_sp$Sp_Code,2:3]
# sp$genus <- unlist(lapply(strsplit(sp$LatinName, ' ', fixed = T), function(x){x[2]}))
# sp$species <- sub(' ', '_', sp$LatinName, fixed = T)
# sp$family <- sp$Family
# sp <- sp[c('species', 'genus', 'family')]
# sp[which(sp$family == 'Flacourtiaceae'),3] <- 'Salicaceae'
# sp[which(sp$family == 'Steruliaceae'),3] <- 'Malvaceae'
# sp[which(sp$family == 'Cesaliniaceae'),3] <- 'Fabaaceae'
# sp['cato','family'] <- 'Lamiaceae'
# sp['clvi',] <- c('Clerodendrum_infortunatum', 'Clerodendrum', 'Lamiaceae')
# sp['glma','family'] <-'Phyllanthaceae'
# sp['gote','family'] <- 'Stemonuraceae'
# sp['goca','family'] <- 'Annonaceae'
# sp['liol','family'] <- 'Lauraceae'
# sp['mewi','family'] <- 'Melastomataceae'
# results <- S.PhyloMaker(sp, phylo, nodes)
# str(results)
# par(mfrow=c(1,3),mar=c(0,0,1,0))
# plot(results$Scenario.1,cex=1.1,main="Scenarion One")
# plot(results$Scenario.2,cex=1.1,main="Scenarion Two")
# plot(results$Scenario.3,cex=1.1,main="Scenarion Three")

# ## FD
#
# # Building
# x <- data.frame(PFT_sp[-1], row.names = PFT_sp$Sp_Code)
# a <- data.frame(tapply(rep(1, length(Trees[,1])), list(Trees$com, Trees$SpCode), sum))
# a[is.na(a)] <- 0
# x <- x[names(a),]
# FD <- as.data.frame(dbFD(x, a, corr = 'cailliez', calc.CWM = F, print.pco = F, messages = F))[-c(1:2,4)]
# FD$id <- row.names(FD)
# com_FD <- merge(com$abundance, FD)
# plot(stack(com_FD)[[22:26]])
#
# # Variogram
# library(gstat)
# dist <- variogram(FRic~1, as(com_FD, 'SpatialPointsDataFrame'))$dist
# traits <- c('FRic', 'FEve', 'FDiv', 'FDis', 'RaoQ')
# plot(c(1:length(dist)) ~ dist, xlim = c(0, max(dist)), ylim = c(0.5,1), pch = '', xlab = 'distance (m)', ylab = 'relative variance (%)')
# var <- data.frame(lapply(lapply(as.list(traits), function(x){variogram(formula(paste(x, '~ 1')), as(com_FD, 'SpatialPointsDataFrame'))}), function(x){x[3]}))
# var <- data.frame(apply(var, 2, function(x){x/max(x)}))
# names(var) <- traits
# # colramp <- rainbow(length(var))
# # for(i in 1:length(var)){
# #   lines(var[,i] ~ dist, col = colramp[i])
# # }
# # legend('bottomright', names(var), fill = colramp, cex = 0.7)
#
# # SAC
# library(ape)
# traits <- c('FRic', 'FEve', 'FDiv', 'FDis', 'RaoQ')
# weights <- 1 / as.matrix(dist(coordinates(com$abundance)))
# diag(weights) <- 0
# SAC <- do.call(rbind.data.frame, lapply(as.list(com_FD@data[traits]), Moran.I, weights))
# SAC <- paste(round(SAC$observed, 2), unlist(lapply(as.list(SAC$p.value), stars)))
# names(SAC) <- c('Abundance', 'Presence-absence', 'Basal area')
# names(SAC) <- traits
# # kable(as.data.frame(SAC))
