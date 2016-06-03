#' Stars
#'
#' @param val num. p-value to convert in stars
#'
#' @return char. Number of stars corresponding to the p-value
#' @export
#'
#' @examples
#' stars(0.049)
#'
stars = function(val){
  if(val < 0.001){
    s = '*** '
  } else if (val < 0.01) {
    s = '**  '
  } else if (val < 0.05) {
    s = '*   '
  } else if (val < 0.1) {
    s = '.   '
  } else {
    s = 'n.s.'
  }
  return(s)
}
