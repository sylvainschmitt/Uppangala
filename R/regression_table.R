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
regression_table <- function(LM, reg, pval, ppval){

  # Regression coefficients
  var <- strsplit(substr(reg,3,nchar(reg)), ' + ', fixed = T)[[1]]
  names(var) <- var
  var_table <- data.frame(matrix(nrow = length(LM), ncol = (length(var)+1)))
  names(var_table) <- c('Trait', var)
  var_table$Trait <- names(LM)
  for(i in 1:length(LM)){
    for(v in var){
      if(v %in% row.names(summary(LM[[i]])$coefficients)){
        var_table[i,v] <- paste(format(summary(LM[[i]])$coefficients[v,1], scientific = T, digits = 2),
                                  stars(ppval[i,v], ns = '    '))
      } else {
        var_table[i,v] <- ''
      }
    }
  }
  # LM R squarred
  var_table$`$R^2$` <- unlist(lapply(LM, function(x){round(summary(x)$r.squared,2)}))

  # Null models
  var_table$`Null model` <- round(pval,3)
  var_table$` ` <- unlist(lapply(pval, stars, ns = ' '))
  var_table[which(var_table$` ` == ' '),2:7] <- ' '

  return(var_table)
}
