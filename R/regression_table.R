#' Regression table
#'
#' @param LM list. List of tested regressions
#' @param reg char. Initial regression formula
#' @param model list. List of null models issued from null_model function
#' @param select char. subset of models to summarize
#'
#' @return A table summarizing all the regression
#' @export
#'
#' @examples
#' # No example yet
#'
regression_table <- function(LM, model, reg, select = '.abiot.cwm'){

  # Results subsetting
  LM <- lapply(LM, function(x){x[grep(select, names(x), fixed = T)]})
  model$formula <- lapply(model$formula, function(x){x[grep(select, names(x), fixed = T)]})
  model$nstat <- lapply(model$nstat, function(x){x[,grep(select, colnames(x), fixed = T)]})
  model$stat <- model$stat[grep(select, row.names(model$stat), fixed = T),]
  model$p.value <- model$p.value[grep(select, row.names(model$p.value), fixed = T),]

  # Regression coefficients
  var <- strsplit(substr(reg,3,nchar(reg)), ' + ', fixed = T)[[1]]
  names(var) <- var
  var_table <- data.frame(matrix(nrow = length(LM[[1]])*3, ncol = (length(var)+2)))
  names(var_table) <- c('Trait', 'CWM', var)
  var_table$Trait[c(1,((1:length(LM[[1]]))*3+1)[-length(LM[[1]])])] <- names(LM[[1]])
  var_table$CWM <- rep(names(LM), length(LM[[1]]))
  for(j in 1:length(LM)){
    for(i in 1:length(LM[[1]])){
      for(v in var){
        row <- which(var_table$Trait == names(LM[[1]])[i]) + j - 1
        if(v %in% row.names(summary(LM[[j]][[i]])$coefficients)){
          var_table[row,v] <- paste(format(summary(LM[[j]][[i]])$coefficients[v,1],
                                         scientific = T, digits = 2),
                                  stars(summary(LM[[j]][[i]])$coefficients[v,4]))
        } else {
          var_table[row,v] <- ''
        }
      }
    }
  }
  if("SouthWesterness" %in% names(var)){
    var["SouthWesterness"] <- 'SW'
  }
  names(var_table) <- c('Trait', 'CWM', var)
  var_table$Trait <- sub(select, '', var_table$Trait)

  # LM R squarred
  var_table$`R-squared` <- NA
  var_table$`R-squared`[c(1,((1:length(LM[[1]]))*3+1)[-length(LM[[1]])])] <- lapply(LM[[1]], function(x){round(summary(x)$r.squared,2)})
  var_table$`R-squared`[c(1,((1:length(LM[[1]]))*3+2)[-length(LM[[1]])])] <- lapply(LM[[2]], function(x){round(summary(x)$r.squared,2)})
  var_table$`R-squared`[c(3,((1:length(LM[[1]]))*3+3)[-length(LM[[1]])])] <- lapply(LM[[3]], function(x){round(summary(x)$r.squared,2)})

  # Null models
  var_table$`Null model` <- NA
  var_table$`Null model`[c(1,((1:length(LM[[1]]))*3+1)[-length(LM[[1]])])] <- paste(round(model$p.value[,1],3), lapply(model$p.value[,1], stars))
  var_table$`Null model`[c(2,((1:length(LM[[1]]))*3+2)[-length(LM[[1]])])] <- paste(round(model$p.value[,2],3), lapply(model$p.value[,2], stars))
  var_table$`Null model`[c(3,((1:length(LM[[1]]))*3+3)[-length(LM[[1]])])] <- paste(round(model$p.value[,3],3), lapply(model$p.value[,3], stars))
  var_table[is.na(var_table)] <- ''

  return(var_table)
}
