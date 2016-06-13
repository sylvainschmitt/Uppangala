#' Regression table
#'
#' @param LM list. List of tested regressions
#' @param reg char. Initial regression formula
#' @param model list. List of null models issued from null_model function
#'
#' @return A table summarizing all the regression
#' @export
#'
#' @examples
#' # No example yet
#'
regression_table <- function(LM, model, reg){
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
  var["SouthWesterness"] <- 'SW'
  names(var_table) <- c('Trait', 'CWM', var)
  var_table$`R-squared` <- NA
  var_table$`R-squared`[c(1,((1:length(LM[[1]]))*3+1)[-length(LM[[1]])])] <- lapply(LM[[1]], function(x){round(summary(x)$r.squared,2)})
  var_table$`R-squared`[c(1,((1:length(LM[[1]]))*3+2)[-length(LM[[1]])])] <- lapply(LM[[2]], function(x){round(summary(x)$r.squared,2)})
  var_table$`R-squared`[c(3,((1:length(LM[[1]]))*3+3)[-length(LM[[1]])])] <- lapply(LM[[3]], function(x){round(summary(x)$r.squared,2)})
  nul_models <- lapply(model, function(x){paste(x$pvalue, unlist(lapply(as.list(x$pvalue), stars)))})
  var_table$`Null model` <- NA
  var_table$`Null model`[c(1,((1:length(LM[[1]]))*3+1)[-length(LM[[1]])])] <- nul_models[[1]]
  var_table$`Null model`[c(2,((1:length(LM[[1]]))*3+2)[-length(LM[[1]])])] <- nul_models[[2]]
  var_table$`Null model`[c(3,((1:length(LM[[1]]))*3+3)[-length(LM[[1]])])] <- nul_models[[3]]
  var_table[is.na(var_table)] <- ''
  return(var_table)
}
