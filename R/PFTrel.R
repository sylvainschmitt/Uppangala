#' @importFrom smatr sma
NULL

#' PFT relation
#'
#' @param PFT1 char. Name of the first PFT
#' @param PFT2 char. Name of the second PFT
#' @param data data.frame. PFT data set
#' @param captions vector. Captions for PFT
#' @param ... other argument pass to plot
#'
#' @return Graphs of relation between the two PFT
#' @export
#'
#' @examples
#' data(PFT)
#' data(captions)
#' PFTrel('SLA', 'WD', PFT, captions)
#'
PFTrel = function(PFT1, PFT2, data, captions, ...){
  plot(formula(paste(PFT1, '~', PFT2)), data = data,
       main = paste(PFT1, "~", PFT2), xlab = captions[PFT2], ylab = captions[PFT1], ...)
  abline(sma(formula(paste(PFT1, '~', PFT2)), data), col = 'red')
  mtext(paste('Standardized Major Axis : R\u00b2 =',round(sma(formula(paste(PFT1, '~', PFT2)), data)$r2[[1]], 2)), 1, col = 'red')
  c = cor.test(formula(paste('~', PFT1, '+', PFT2)), data, method = 'spearman')
  mtext(paste('Spearman\'s correlation : cor =', round(c$estimate, 2), stars(c$p.value)) , 3, col = 'blue')
}
