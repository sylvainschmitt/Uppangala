#' @importFrom BIOMASS modelHD predictHeight
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
  path <- system.file('extdata', 'Sp_trees_allPlot.csv', package = 'Uppangala')
  tree <- read.csv(path, header = T, stringsAsFactors = F)
  # ID creations
  tree$ID <- NA
  tree$ID[which(is.na(tree$Plot_10Ha_ID))] <- as.character(tree$Tree_10Ha_Field_ID[which(is.na(tree$Plot_10Ha_ID))])
  tree$ID[which(!is.na(tree$Plot_10Ha_ID))] <- as.character(tree$Tree_10Ha_ID[which(!is.na(tree$Plot_10Ha_ID))])
  # ID issues
  tree[which(duplicated(tree$ID)),] # rh47 duplicated
  tree <- tree[-which(duplicated(tree$ID)),] # Remove
  # Formatting table
  tree <- data.frame(SpCode = tree$Sp_code, row.names = tree$ID)

  # Opening locations
  path <- system.file('extdata', 'Coord_trees_allPlot.csv', package = 'Uppangala')
  location <- read.table(path, header = T, sep = ",", dec = ".")
  # ID creations
  location$ID <- NA
  location$ID[which(is.na(location$Plot_10Ha_ID))] <- as.character(location$Tree_10Ha_Field_ID[which(is.na(location$Plot_10Ha_ID))])
  location$ID[which(!is.na(location$Plot_10Ha_ID))] <- as.character(location$Tree_10Ha_ID[which(!is.na(location$Plot_10Ha_ID))])
  # ID issues
  location[which(duplicated(location$ID)),] # rh47 duplicated
  location <- location[-which(duplicated(location$ID)),] # Remove
  # Formatting table
  row.names(location) <- location$ID
  location <- location[7:8]
  names(location) <- c('X', 'Y')

  # Opening girth 5 ha plots
  path <- system.file('extdata', 'Girth_5Ha.csv', package = 'Uppangala')
  girth5 <- read.table(path, header = T, sep = ",", dec = ".")
  girth5$ID.date <- paste(girth5$Tree_5Ha_ID,
                          substr(as.character(girth5$Census_date), start = 7, stop = 11),
                          substr(as.character(girth5$Census_date), start = 4, stop = 5),
                          sep = '_')
  # Opening height 5 ha plots
  path <- system.file('extdata', 'Height_5Ha.csv', package = 'Uppangala')
  height5 <- read.table(path, header = T, sep = ",", dec = ".")
  height5$ID.date <- paste(height5$Tree_5Ha_ID,
                          substr(as.character(height5$Census_date), start = 7, stop = 11),
                          substr(as.character(height5$Census_date), start = 4, stop = 5),
                          sep = '_')
  # Modelling height girth relation
  IDs <- intersect(height5$ID.date, girth5$ID.date)
  H <- data.frame(H = height5$Total_height, row.names = height5$ID.date)
  H[H == -999] <- NA
  H <- H[IDs,]
  D <- data.frame(D = girth5$Girth/2*pi, ID = girth5$ID.date)
  D <- D[which(!duplicated(D$ID)),]
  row.names(D) <- D$ID
  D[D == -999] <- NA
  D <- D[IDs,1]
  m <- modelHD(D, H, method = 'michaelis')
  # Preparing girth
  girth5$ID <- girth5$Tree_5Ha_ID
  girth5$Date <- paste(substr(as.character(girth5$Census_date), start = 7, stop = 11),
                       substr(as.character(girth5$Census_date), start = 4, stop = 5),
                       'girth', sep = '_')
  girth5 <- as.data.frame(with(girth5, tapply(Girth, list(ID, Date), mean)))
  girth5[girth5 == -999] <- NA
  summary(girth5)
  # Opening girth 10 ha plots
  path <- system.file('extdata', 'Girth_10Ha.csv', package = 'Uppangala')
  girth10 <- read.table(path, header = T, sep = ",", dec = ".")
  girth10$ID <- girth10$Tree_10Ha_ID
  girth10$Date <- paste(substr(as.character(girth10$Census_date), start = 7, stop = 11),
                       substr(as.character(girth10$Census_date), start = 4, stop = 5),
                       'girth', sep = '_')
  girth10 <- as.data.frame(with(girth10, tapply(Girth, list(ID, Date), mean))) # Data for girth on 10ha plots only for 01-2013
  girth10[girth10 == -999] <- NA
  summary(girth10)
  # Merging all girth
  girth <- data.frame(X = c(girth5$`2013_03_girth`, girth10$`2013_01_girth`),
                      row.names = c(row.names(girth5), row.names(girth10)))
  names(girth) <- '2013_girth'

  # Merging all tables
  tree$ID <- row.names(tree) # Stupid to optimize with precedent
  location$ID <- row.names(location)
  tree <- merge(tree, location, all.x = T)
  girth$ID <- row.names(girth)
  tree <- merge(tree, girth, all.x = T)
  tree <- tree[-which(is.na(tree$SpCode)),]
  row.names(tree) <- tree$ID
  tree <- tree[-1]
  tree$height <- predictHeight(tree$`2013_girth`, m)
  summary(tree)

  return(tree)
}
