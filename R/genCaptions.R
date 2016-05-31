#' PFT data captions generation
#'
#' Generates PFT data captions
#'
#' @return PFT data captions
#' @export
#'
#' @examples
#' genCaptions()
#'
genCaptions <- function(){
  captions <- c('Leaf ID',
                'Tree ID',
                'Species',
                'Species code',
                'Crown exposure (dawkins 13 index)',
                'Simplified crown exposure (dawkins 5 index)',
                'Thickness (\u00b5m)',
                'Leaf area (mm\u00b2)',
                'Logarithm of leaf area (mm\u00b2)',
                'Leaf dry matter content (mg.g\u207b\u00b9)',
                'Specific leaf area (m\u00b2.kg\u207b\u00b9)',
                'Logarithm of specific leaf area (m\u00b2.kg\u207b\u00b9)',
                'Specific holes area (mm\u00b2.mm\u207b\u00b2)',
                'Logarithm of specific holes area (mm\u00b2.mm\u207b\u00b2)',
                'Wood density (g.cm\u207b\u00b3)')
  names(captions) = c('ID',
                      'Tree',
                      'SP',
                      'Species code',
                      'CE',
                      'CEs',
                      'Thick',
                      'LA',
                      'log(LA)',
                      'LDMC',
                      'SLA',
                      'log(SLA)',
                      'SHA',
                      'log(SHA)',
                      'WD')
  return(captions)
}
