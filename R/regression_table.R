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
regression_table <- function(LM, reg, pval, weight = 'abundance', metric = '.cwm'){

  # Results subsetting
  LM <- LM[[weight]]
  LM <- LM[grep(metric, names(LM), fixed = T)]
  pval <- as.data.frame(pval)[weight]
  pval <- pval[grep(metric, row.names(pval), fixed = T),]

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
                                  stars(summary(LM[[i]])$coefficients[v,4]))
      } else {
        var_table[i,v] <- ''
      }
    }
  }
  var_table$Trait <- gsub(metric, '', var_table$Trait, fixed = T)[1:7]

  # LM R squarred
  var_table$R2 <- unlist(lapply(LM, function(x){round(summary(x)$r.squared,2)}))

  # Null models
  var_table$`Null model` <- round(pval,3)
  var_table$` ` <- unlist(lapply(pval, stars, ns = ' '))
  # var_table[which(var_table$` ` == ' '),2:7] <- ' '

  return(var_table)
}
