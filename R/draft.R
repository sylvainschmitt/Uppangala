## Atypic communtiy metrics distributions##
# CWM.ab <- CWM$abundance
# nCWM.ab <- nCWM[[1]]
# nCWM.ab <- lapply(nCWM.ab, `[`, names(nCWM.ab[[1]]))
# nCWM.ab <- apply(do.call(rbind, nCWM.ab), 2, as.list)
# nCWM.ab <- lapply(nCWM.ab, function(x){do.call('cbind', x)})
# nCWM.ab <- lapply(nCWM.ab, data.frame)
# CWM.ab.ses <- data.frame(mapply(function(x,y){
#   mapply(function(p,q){
#     (p - mean(q))/sd(q)
#   }, p = as.list(x), q = as.list(data.frame(t(y))))
# }, x = CWM.ab, y = nCWM.ab))
# extrem_dist <- function(t, ses, l, q, cwm, cwv){
#   com <- row.names(ses)[c(which(ses[t] == min(ses[t], na.rm = T)), which(ses[t] == max(ses[t], na.rm = T)))]
#   dist.com <- apply(l[com,], 1, function(x){rep(t(q[t]),x)})
#   cwm.com <- cwm[[t]][com]
#   cwv.com <- cwv[[t]][com]
#   for(c in com){
#     x <- range(q[t], na.rm = T)
#     dth <- dnorm(seq(x[1], x[2], 0.01), mean = cwm.com[c], sd = cwv.com[c])
#     dth <- dth / max(dth)
#     h <- hist(dist.com[[c]], plot = F)
#     h$counts <- h$counts / max(h$counts)
#     plot(h, col = 'lightgrey', main = c, xlab = t, xlim = c(x[1],x[2]), ylim = c(0,max(max(h$counts),max(dth))))
#     lines(x = seq(x[1], x[2], 0.01), y = dth, col = 'red', lwd = 2)
#   }
# }
#
# par(mfrow = c(3,2))
# t = 'Thick'
# extrem_dist(t, CWM.ab.ses, L$abundance, Q, CWM$abundance, CWV)
# extrem_dist(t, CWV.ses, L$abundance, Q, CWM$abundance, CWV)
# extrem_dist(t, CVNND.ses, L$abundance, Q, CWM$abundance, CWV)

## Atypic SES metrics community composition##
# which(apply(1 - CWV.pval, 2, p.adjust, method = 'fdr') < 0.025, arr.ind = T)
# L$abundance['C196',which(L$abundance['C196',] != 0)]

## WD between communities and Spatial organization##
# com <- quadrats()
# com@data$WD <- CWM$`presence-absence`$WD[match(com$id, names(CWM$abundance$WD))]
# plot(raster(com, 'WD'), alpha = 0.7)
# plot(as(com, 'SpatialPolygons'), add = T, border = 'grey')
# # which(spafun$WD$pairwise.TAUst == max(spafun$WD$pairwise.TAUst), arr.ind = T)
# # which(spafun$WD$pairwise.TAUst == min(spafun$WD$pairwise.TAUst), arr.ind = T)
# mantel <- phytools::multi.mantel(as.matrix(spafun$WD$pairwise.TAUst), list(as.matrix(Ms), as.matrix(Me)), nperm = 0)
# mantel <- phytools::multi.mantel(as.matrix(spafun$WD$pairwise.TAUst), as.matrix(Me), nperm = 0)
# m <- as.matrix(mantel$residuals)
# max <- row.names(which(m == max(m), arr.ind = T))
# min <- row.names(which(m == min(m), arr.ind = T))
# col <- rep(1, length(com$id))
# col[match(max, com$id)] = 2
# col[match(min, com$id)] = 3
# text(coordinates(com), com$id, col = c('grey', 'red', 'blue')[col])

# ## Mortality inside Uppangala##
# # Opening trees
# path <- system.file('extdata', 'UppangalaCoordDataset.csv', package = 'Uppangala')
# tree <- read.table(path, header = T, sep = ",", dec = ".")
# # ID creations
# tree$ID <- NA
# tree$ID[which(is.na(tree$Plot_10Ha_ID))] <- as.character(tree$Tree_10Ha_Field_ID[which(is.na(tree$Plot_10Ha_ID))])
# tree$ID[which(!is.na(tree$Plot_10Ha_ID))] <- as.character(tree$Tree_10Ha_ID[which(!is.na(tree$Plot_10Ha_ID))])
# # ID issues
# tree[which(duplicated(tree$ID)),] # rh47 duplicated
# tree <- tree[-which(duplicated(tree$ID)),] # Remove
# # Formatting table
# row.names(tree) <- tree$ID
# species <- genSpecies()
# tree$SpCode <- species$SpCode[match(tree$Sp_code, species$SpCodeL)]
# # Opening girth
# path <- system.file('extdata', 'allGirth.csv', package = 'Uppangala')
# girth <- read.table(path, header = T, sep = ",", dec = ".")
# girth$ID <- NA
# girth$ID[which(is.na(girth$Tree_10Ha_ID))] <- as.character(girth$Tree_10Ha_Field_ID[which(is.na(girth$Tree_10Ha_ID))])
# girth$ID[which(!is.na(girth$Tree_10Ha_ID))] <- as.character(girth$Tree_10Ha_ID[which(!is.na(girth$Tree_10Ha_ID))])
# girth$Girth[girth$Girth == -999] = NA
# girth <- girth[-which(girth$CensusDate %in% c('1990-0.', '2014-03', '2015-12', '2016-04', '2016-05')),]
# girth <- droplevels(girth)
# test <- tapply(girth$Girth, list(girth$ID, girth$CensusDate), sum)
# sel <- match(row.names(test), tree$ID)
# test <- test[-which(is.na(sel)),]
# sel <- match(row.names(test), tree$ID)
# XY <- tree[c('x','y')][sel,]
# test <- test[-which(is.na(XY[,1])),]
# XY <- XY[-which(is.na(XY[,1])),]
# coordinates(XY) <- ~ x + y
# grid <- maptools::readShapePoly('C:/Users/sylvain/Desktop/Graphs/all5Ha/Shapeall5ha.shp')
# proj4string(XY) <- crs(grid)
# sel <- which(!is.na(XY %over% grid))
# test <- test[sel,]
# XY <- XY[sel,]
# death <- 1*is.na(test[,length(colnames(test))])
# XY <- data.frame(XY)
# XY$death <- death
# XY$species <- tree$SpCode[match(row.names(XY), tree$ID)]
# XY$WD <- PFT$WD[match(XY$species, PFT$SpCode)]
# coordinates(XY) <- ~ x + y
# XY$q <- (XY %over% grid)[,1]
# death.df <- aggregate(XY$death, list(XY$q), sum)
# death <- death.df$x
# names(death) <- death.df$Group.1
# L <- data.frame(tapply(rep(1, length(XY$death)), list(XY$q, XY$species), sum))
# L[is.na(L)] <- 0
# sel <- intersect(row.names(Q), names(L))
# Q <- Q[sel,]
# L <- L[sel]
# CWM <- apply(L, 1, function(x){weighted.mean(Q$WD, x, na.rm=T)})
# CWM <- CWM[match(names(death), names(CWM))]
# plot(CWM ~ log(death+1))
# summary(lm(CWM ~ log(death+1)))
# plot(grid[which(grid$ID == names(death[death == 9])),], add = T)


# ## CAH plot ##
# com <- quadrats()
# com@data$WD <- CWM$`presence-absence`$WD[match(com$id, names(CWM$abundance$WD))]
# mantel <- phytools::multi.mantel(as.matrix(spafun$WD$pairwise.TAUst), as.matrix(Me), nperm = 0)
# tree <- hclust(mantel$residuals, method = 'ward.D2')
# plot(tree)
# n = 3
# com$group <- cutree(tree, n)
# # 2 plot #
# par(mfrow = c(1,2))
# plot(raster(com, 'WD'), alpha = 0.7, main = 'WD')
# plot(as(com, 'SpatialPolygons'), add = T, border = 'grey')
# text(coordinates(com), com$id, col = 'grey', cex = 0.7)
# plot(raster(com, 'group'), col = rainbow(n), alpha = 0.7, main = 'group')
# plot(as(com, 'SpatialPolygons'), add = T, border = 'grey')
# text(coordinates(com), com$id, col = 'grey', cex = 0.7)
# # 1 plot #
# par(mfrow = c(1,1))
# plot(raster(com, 'WD'), alpha = 0.5, main = 'WD')
# plot(as(com, 'SpatialPolygons'), add = T, border = 'grey')
# text(coordinates(com), com$id, col = rainbow(n)[com$group], cex = 0.7)

# ## Light to soil and canopy height ##
# Canopy <- raster('./inst/extdata/Canopy.tif')
# terrain_rays <- read.csv("C:/Users/sylvain/Desktop/terrain_rays.csv", header=FALSE)
# light <- terrain_rays[c(1,2,4)]
# names(light) <- c('x','y','light')
# light <- rasterFromXYZ(light)
# Canopy <- crop(Canopy, extent(light))
# Canopy <- resample(Canopy, light)
# Comp <- stack(Canopy, light)
# plot(Comp)
# summary(lm(values(light) ~ values(Canopy)))
# plot(values(light) ~ values(Canopy))

# ## LDMC - Canopy ##
# com <- row.names(which(R == max(R$Canopy), arr.ind = T))
# hist(CWM$`presence-absence`$LDMC)
# CWM$`presence-absence`$LDMC[com]
# comp <- colnames(L$`presence-absence`[com,L$`presence-absence`[com,] > 0])
# Species$LatinName[match(comp, Species$SpCode)]
# barplot(t(Q[match(comp, row.names(Q)),][5]), las = 2)
# plot(Q$LDMC ~ as.numeric(Species$Strata[match(row.names(Q), Species$SpCode)]), xlab = 'Strata', ylab = 'LDMC')
# abline(lm(Q$LDMC ~ as.numeric(Species$Strata[match(row.names(Q), Species$SpCode)])), col = 'red')
# mtext(paste('LDMC ~ Strata***, R2 =', round(summary(lm(Q$LDMC ~ as.numeric(Species$Strata[match(row.names(Q), Species$SpCode)])), col = 'red')$r.squared, 3)))
# plot(Q$LDMC ~ as.numeric(Species$Strata[match(row.names(Q), Species$SpCode)]), xlab = 'Strata', ylab = 'LDMC')
# abline(lm(Q$LDMC ~ as.numeric(Species$Strata[match(row.names(Q), Species$SpCode)])), col = 'red')
# mtext(paste('LDMC ~ Strata***, R2 =', round(summary(lm(Q$LDMC ~ as.numeric(Species$Strata[match(row.names(Q), Species$SpCode)])), col = 'red')$r.squared, 3)))

# ## RLQ bis ##
# library(ade4)
# Q <- Q[-which(is.na(Q$WD)),-c(1,3)]
# L <- L$abundance[row.names(Q)]
# L.coa <- dudi.coa(L, scannf = F)
# R.pca <- dudi.pca(R, scannf = F, row.w = L.coa$lw)
# Q.pca <- dudi.pca(Q, scannf = F, row.w = L.coa$cw)
# rlq <- rlq(R.pca, L.coa, Q.pca, scannf = T)
# rlq$l1
# s.label(rlq$mQ, xax = 4, yax = 5, boxes = F)
# s.arrow(rlq$l1, xax = 4, yax = 5, add.plot = T)
# FamilyFact <- as.factor(Species$Family[match(row.names(rlq$mQ), Species$SpCode)])
# col <- rainbow(length(levels(FamilyFact)))[as.numeric(FamilyFact)]
# s.class(rlq$mQ, FamilyFact, clabel = 0, xax = 4, yax = 5, cellipse = 0, label = rep(' ',72), col = col, add.plot = T)
# legend('topleft', levels(FamilyFact), fill = col)
# selsp <- c('anmo', 'baco', 'blse', 'desi', 'dila', 'mrin')
# col = rep(1, length(Q$LDMC))
# col[match(selsp, row.names(Q))] <- 2
# barplot(Q$LDMC, col = c('grey','firebrick')[col], las = 2, names.arg = row.names(Q))
# s.label(rlq$mR, xax = 4, yax = 5, boxes = F)
# s.arrow(rlq$l1, xax = 4, yax = 5, add.plot = T)
#
# L['C224',which(L['C224',] > 0)]
# bca <- bca(Q.pca, as.factor(Species$Family[match(row.names(Q.pca$tab), Species$SpCode)]), scannf = T)
# bca$c1
# biplot(bca, xax = 5, yax = 3)
# randtest(bca)
