#' Stars
#'
#' @param val num. p-value to convert in stars
#' @param ns char. non significant character
#'
#' @return char. Number of stars corresponding to the p-value
#' @export
#'
#' @examples
#' stars(0.049)
#'
stars = function(val, ns = 'n.s.'){
  if(!is.numeric(val) || is.nan(val)){
    val = 1
  }
  if(val < 0.001){
    s = '*** '
  } else if (val < 0.01) {
    s = '**  '
  } else if (val < 0.05) {
    s = '*   '
  } else if (val < 0.1) {
    s = '.   '
  } else {
    s = ns
  }
  return(s)
}
