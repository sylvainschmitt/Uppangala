#' Regression table
#'
#' @param LM list. Tested regressions
#' @param reg char. Initial regression formula
#' @param model list. Null models issued from null_model function
#'
#' @return A table summarizing all the regression
#' @export
#'
#' @examples
#' # No example yet
#'
regression_table <- function(LM, model, reg){
  table <- data.frame(row.names = names(LM),
                      step_formula = unlist(lapply(strsplit(unlist(lapply(LM, function(x){as.character(x$call)[2]})), ' ~ '), function(x){x[2]})))
  var <- strsplit(substr(reg,2,nchar(reg)), ' + ', fixed = T)[[1]]
  var_table <- data.frame(matrix(nrow = length(table[,1]), ncol = length(var)))
  names(var_table) <- var
  for(v in var){
    for(i in 1:length(LM)){
      if(v %in% row.names(summary(LM[[i]])$coefficients)){
        var_table[i,v] <- paste(format(summary(LM[[i]])$coefficients[v,1], scientific = T, digits = 2),
                                stars(summary(LM[[i]])$coefficients[v,4]))
      } else {
        var_table[i,v] <- ''
      }
    }
  }
  table <- cbind(table, var_table)
  table$Null_model <- unlist(lapply(as.list(model$pvalue), stars))
  return(table)
}
