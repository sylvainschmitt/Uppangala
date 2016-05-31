#' Species data generation
#'
#' Generates Species data set from external data extracted from the database
#'
#' @return Species data set
#' @export
#'
#' @examples
#' summary(genSpecies())
#'
genSpecies <- function(){
  # Opening
  path <- system.file('extdata', 'Species.txt', package = 'Uppangala')
  species <- read.table(path,sep=",",dec=".", stringsAsFactors =F) # Dans BDD Uppangala
  # Formatting table
  species <- species[-1,c(3,4,10,5,9,11:13)]
  colnames(species) <- c("SpCode","LatinName","Family","Authority","VoucherRef","PotStrata","SpError","SpNote")
  species <- species[-which(is.na(species$LatinName)),]
  species$SpCode <- tolower(species$SpCode)
  rownames(species) <- species$SpCode
  return(species)
}
