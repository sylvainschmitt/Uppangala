#' @include genSpecies.R
NULL

#' Trees data generation
#'
#' Generates Trees data set from external data extracted from the database
#'
#' @return Trees data set
#' @export
#'
#' @examples
#' summary(genTrees())
#'
genTrees <- function(){

  # Opening trees
  path <- system.file('extdata', 'UppangalaCoordDataset.csv', package = 'Uppangala')
  tree <- read.table(path, header = T, sep = ",", dec = ".")
  # ID creations
  tree$ID <- NA
  tree$ID[which(is.na(tree$Plot_10Ha_ID))] <- as.character(tree$Tree_10Ha_Field_ID[which(is.na(tree$Plot_10Ha_ID))])
  tree$ID[which(!is.na(tree$Plot_10Ha_ID))] <- as.character(tree$Tree_10Ha_ID[which(!is.na(tree$Plot_10Ha_ID))])
  # ID issues
  tree[which(duplicated(tree$ID)),] # rh47 duplicated
  tree <- tree[-which(duplicated(tree$ID)),] # Remove
  # Formatting table
  row.names(tree) <- tree$ID
  species <- genSpecies()
  tree$SpCode <- species$SpCode[match(tree$Sp_code, species$SpCodeL)]
  tree <- tree[c('ID', 'SpCode', 'x', 'y')]

  # Opening girth
  path <- system.file('extdata', 'allGirth.csv', package = 'Uppangala')
  girth <- read.table(path, header = T, sep = ",", dec = ".")
  girth <- girth[which(girth$CensusDate %in% c('2013-03', '2014-03')),]
  girth$ID <- NA
  girth$ID[which(is.na(girth$Tree_10Ha_ID))] <- as.character(girth$Tree_10Ha_Field_ID[which(is.na(girth$Tree_10Ha_ID))])
  girth$ID[which(!is.na(girth$Tree_10Ha_ID))] <- as.character(girth$Tree_10Ha_ID[which(!is.na(girth$Tree_10Ha_ID))])
  girth <- girth[-which(duplicated(girth$ID)),]
  girth$Girth[girth$Girth == -999] = NA
  tree <- merge(tree, girth)
  tree <- tree[c('SpCode', 'x', 'y', 'Girth')]

  # Removing mismatch ?
  girth$ID[which(!(girth$ID %in% row.names(tree)))]

  # NA removing
  tree <- tree[-which(is.na(tree$Girth)),]
  tree <- tree[-which(is.na(tree$x)),]

  return(tree)
}
