#' Bold
#'
#' Render bold cell in Rmarkdown depending on a pattern (to put in blod
#' significant results for instance)
#'
#' @param table data.frame. Table on which to highlight cell in bold.
#' @param pattern char. Pattern to be detected to highlight cells.
#'
#' @return data.frame
#'
#' @export
#'
#' @examples
#' # See draft.Rmd
#'
bold <- function(table, pattern = '*'){
  table <- apply(table, 2, function(col){
    unlist(lapply(as.list(col), function(cell){
      if(grepl(pattern, cell, fixed = T)){
        cell <- paste0('__', cell, '__')
      }
      return(cell)
    }))
  })
  return(table)
}
