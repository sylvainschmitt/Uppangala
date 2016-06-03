#' Plot null model
#'
#' Function to plot null model and get null model result
#'
#' @param model null_model list. result from null_model function (could do an object)
#' @param formula char. formula of the model to be plotted in the object
#' @param model int. indice of the model to be plotted in the object
#'
#' @return nothing but a plot of the result
#'
#' @export
#'
#' @examples
#' # No example yet see Uppangala_PFT vignette
#'
plot_model = function(model, formula = NULL, index = NULL){
  if(!is.null(formula)){
    index <- which(model$formula == formula)
  }
  model$formula <- model$formula[index]
  model$nullvalues <- model$nullvalues[,index]
  model$value <- model$value[[index]]
  model$pvalue <- model$pvalue[index]
  model$lim <- model$lim[index]
  hist(model$nullvalues, freq = F, col = 'lightgrey', border = F, xlab = "", breaks = 20,
       main = as.character(model$formula), xlim = range(c(model$nullvalues, (model$value+0.2))))
  lines(density(model$nullvalues),
       main = paste('Null Model with', model$test, ':', model$formula),
       xlim = range(c(model$nullvalues, (model$value+0.2))))
  lines(c(model$value, model$value), c(0,1), col = 'red', lwd = 2)
  text(model$value + 0.2, 0.1, 'Com\nval', col = 'red')
  mtext(paste('p-value =', model$pvalue, stars(model$pvalue)), 3)
  mtext(paste('Repetitions =', model$rep), 1)
  x <- model$lim
  lines(c(x, x), c(0,1), lty = 2)
  text(x + 0.2, 0.1, '0.95', cex = 0.7)
}
