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
null_model = function(formula,
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
  formul <- formula(formula)
  stat <-  c()
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
    summary(com)
    Nullcom@data <- Nullcom@data[ccol]
    Nullcom <- merge(Nullcom, NullCWM)
    if(test == 'Anova Fvalue'){
      summary(Nullcom)
      Nullval <- summary(aov(formul, Nullcom))[[1]][['F value']][1]
    } else if(test == 'Anova lm Fvalue'){
      Nullval <- summary(aov(lm(formul, Nullcom)))[[1]][['F value']][1]
    }
    stat <- c(stat, Nullval)
    if(plot){
      plot(stack(raster(com, 'Comp.1'), raster(Nullcom, 'Comp.1')),
           main = c('Reality', paste('Null model', i)))
    }
    if(count){
      cat(i, '\n')
    }
  }
  if(test == 'Anova Fvalue'){
    stat.val <- summary(aov(formul, com@data))[[1]][['F value']][1]
  } else if(test == 'Anova lm Fvalue'){
    stat.val <- summary(aov(lm(formul, com@data)))[[1]][['F value']][1]
  }
  rm(NullPFT_sp, NullTrees, NullCWM, Nullcom)
  if(time){
    print(Sys.time() - t0)
  }
  ranks <- (rank(c(stat.val, stat))/n)
  pval <- ranks[1]
  if(pval > 0.5){
    pval <- 1 - pval # Two sided
  }
  x1 = c(stat.val, stat)[which(abs(ranks - 0.025) == min(abs(ranks - 0.025)))]
  x2 = c(stat.val, stat)[which(abs(ranks - 0.975) == min(abs(ranks - 0.975)))]
  x3 = c(stat.val, stat)[which(abs(ranks - 0.95) == min(abs(ranks - 0.95)))]
  return(list(formula = formula,
              test = test,
              rep = n,
              nullvalues = stat,
              value = stat.val,
              pvalue = pval,
              lim = c(x1, x2, x3)))
}
