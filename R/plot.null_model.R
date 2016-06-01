#' Plot null model
#'
#' Function to plot null model and get null model result
#'
#' @param model null_model list. result from null_model function (could do an object)
#'
#' @return nothing but a plot of the result
#'
#' @export
#'
#' @examples
#' # No example yet see Uppangala_PFT vignette
#'
plot_model = function(model){
  hist(model$nullvalues, freq = F, col = 'lightgrey', border = F, xlab = "", breaks = 20,
       main = paste('Null Model with', model$test, ':', model$formula),
       xlim = range(c(model$nullvalues, (model$value+0.2))))
  lines(density(model$nullvalues),
       main = paste('Null Model with', model$test, ':', model$formula),
       xlim = range(c(model$nullvalues, (model$value+0.2))))
  lines(c(model$value, model$value), c(0,1), col = 'red', lwd = 2)
  text(model$value + 0.2, 0.1, 'Com\nval', col = 'red')
  mtext(paste('p-value =', model$pvalue, stars(model$pvalue)), 3)
  mtext(paste('Repetitions =', model$rep), 1)
  x1 <- model$lim[1]
  x2 <- model$lim[2]
  x3 <- model$lim[3]
  lines(c(x1, x1), c(0,1), lty = 2)
  lines(c(x2, x2), c(0,1), lty = 2)
  lines(c(x3, x3), c(0,1), lty = 6)
  text(x1 + 0.2, 0.1, '0.025', cex = 0.7)
  text(x2 + 0.2, 0.1, '0.975', cex = 0.7)
  text(x3 + 0.2, 0.1, '0.95', cex = 0.7)
}
