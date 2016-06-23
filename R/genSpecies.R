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
  path <- system.file('extdata', 'Species.csv', package = 'Uppangala')
  species <- read.table(path,sep=",",dec=".", stringsAsFactors =F, h = T) # Dans BDD Uppangala
  # Formatting table
  species <- species[c('spco', 'Latin_name', 'Family', 'PotStrata')]
  colnames(species) <- c("SpCode","LatinName","Family","PotStrata")
  species <- species[-which(is.na(species$LatinName)),]
  species$SpCode <- tolower(species$SpCode)
  rownames(species) <- species$SpCode
  # Expliciting strata
  species$ESI <- 0; species[grep(x=species$PotStrata,pattern="\\bESI\\b"),]$ESI <- 1
  species$ESII <- 0; species[grep(x=species$PotStrata,pattern=", II"),]$ESII <- 1; species[grep(x=species$PotStrata,pattern="\\bESII\\b"),]$ESII <- 1
  species$ESIII <- 0; species[grep(x=species$PotStrata,pattern=", III"),]$ESIII <- 1; species[grep(x=species$PotStrata,pattern="ESIII"),]$ESIII <- 1
  species$ESIV <- 0; species[grep(x=species$PotStrata,pattern=", IV"),]$ESIV <- 1; species[grep(x=species$PotStrata,pattern="ESIV"),]$ESIV <- 1
  # Lot of species in different strata
  # Ayyappan suggest to take the lowest value
  species[species$ESI==1 & species$ESII==1,]$ESI <- 0
  species[species$ESII==1 & species$ESIII==1,]$ESII <- 0
  species[species$ESIII==1 & species$ESIV==1,]$ESIII <- 0
  # Strata as factor
  species$Strata <- NA
  species$Strata[species$ESI==1] <- 'ESI'
  species$Strata[species$ESII==1] <- 'ESII'
  species$Strata[species$ESIII==1] <- 'ESIII'
  species$Strata[species$ESIV==1] <- 'ESIV'
  species$Strata <- as.factor(species$Strata)
  species <- species[-which(names(species) %in% levels(species$Strata))]

  return(species)
}
