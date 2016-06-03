#' Null model
#'
#' Function to compute null model
#'
#' @param formula char. test formula
#' @param test char. name of the used test (Anova Fvalue or lm R2)
#' @param PFT_sp data.frame. PFT values by species
#' @param Trees data.frame. trees table
#' @param com SpatialGridDataFrame. communities
#' @param tcol int. column to keep in trees table
#' @param ccol int. communities column to keep
#' @param n int. number of repetitions
#' @param plot logical. do all null communitier need to be plot
#' @param count logical. do the number of the realized null model need to be counted
#' @param time logical. do the time of the operation need to be measured
#'
#' @return a list with the formula, the test, the null values, and the community value
#'
#' @export
#'
#' @examples
#' # No example yet see Uppangala_PFT vignette
#'
null_model = function(formulas,
                      test,
                      PFT_sp,
                      Trees,
                      com,
                      SpCode,
                      tcol = NULL,
                      cwmcol = c(1:1),
                      ccol = c(1:12),
                      n = 1000,
                      plot = F,
                      count = F,
                      time = T){
  if(is.null(tcol)){
    tcol <- which(names(Trees) %in% c('SpCode', 'com'))
  }
  formul <- formulas
  formulas <- as.list(formulas)
  formulas <- lapply(formulas, formula)
  stat <-  matrix(ncol = length(formulas), nrow = (n-1))
  colnames(stat) <- formul
  t0 <- Sys.time()
  for(i in 1:(n-1)){
    NullPFT_sp <- PFT_sp
    row.names(NullPFT_sp) <- sample(row.names(PFT_sp))
    NullPFT_sp$Sp_Code <- SpCode[row.names(NullPFT_sp)]
    NullTrees <- Trees[tcol]
    NullTrees$ID <- row.names(Trees)
    NullTrees <- merge(NullTrees, NullPFT_sp, by.x = 'SpCode', by.y = 'Sp_Code')
    row.names(NullTrees) <- NullTrees$ID
    NullTrees <- NullTrees[-which(names(NullTrees) == 'ID')]
    NullCWM <- aggregate(NullTrees, by = list(NullTrees$com), mean, na.rm = T)
    names(NullCWM)[1] <- 'id'
    NullCWM <- NullCWM[which(names(NullCWM)
                             %in% c('id', 'Thick', 'LA', 'LDMC', 'SLA', 'WD', 'PCA1', 'PCA2', 'PCA3'))]
    Nullcom <- com
    Nullcom@data <- Nullcom@data[ccol]
    Nullcom <- merge(Nullcom, NullCWM)
      if(test == 'Anova Fvalue'){
        stat[i,] <- unlist(lapply(formulas, function(x){summary(aov(x, Nullcom))[[1]][['F value']][1]}))
      } else if(test == 'Anova lm Fvalue'){
        stat[i,] <- unlist(lapply(formulas, function(x){summary(aov(lm(x, Nullcom)))[[1]][['F value']][1]}))
      }
    if(plot){
      plot(stack(raster(com, 'Comp.1'), raster(Nullcom, 'Comp.1')),
           main = c('Reality', paste('Null model', i)))
    }
    if(count){
      cat(i, '\n')
    }
  }
  if(test == 'Anova Fvalue'){
    stat.val <- lapply(formulas, function(x){summary(aov(x, com@data))[[1]][['F value']][1]})
  } else if(test == 'Anova lm Fvalue'){
    stat.val <- lapply(formulas, function(x){summary(aov(lm(x, com@data)))[[1]][['F value']][1]})
  }
  ranks <- mapply(function(x,y){(rank(c(x, y))/n)}, x = stat.val, y = as.list(data.frame(stat)))
  pval <- ranks[1,]
  pval[pval > 0.5] <- 1 - pval[pval > 0.5] # Two sided
  lim = unlist(mapply(function(X,Y,Z){c(X,Y)[which(abs(Z - 0.95) == min(abs(Z - 0.95)))]},
                      X = stat.val, Y = as.list(data.frame(stat)), Z = as.list(data.frame(ranks))))
  if(time){
    print(Sys.time() - t0)
  }
  return(list(formula = formul,
              test = test,
              rep = n,
              nullvalues = stat,
              value = unlist(stat.val),
              pvalue = pval,
              lim = lim))
}
