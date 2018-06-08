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
  path <- file.path('data', 'Species.csv')
  species <- read.table(path,sep=",",dec=".", stringsAsFactors =F, h = T) # Dans BDD Uppangala
  # Formatting table
  species <- species[c('Sp_code', 'Sp_code_large', 'Latin_name', 'Family', 'Genus', 'Species', 'Pot_strata', 'old_Sp_code')]
  colnames(species) <- c("SpCode", "SpCodeL", "LatinName", "Family", 'Genus', 'Species', 'PotStrata', 'OldCode')
  species$SpCode <- tolower(species$SpCode)
  species[duplicated(species$SpCode),1] <- species[duplicated(species$SpCode),8]
  rownames(species) <- species$SpCode
  # Opening strata
  path <- file.path('data', 'UPSP_Species_list.csv')
  strata <- read.table(path,sep=",",dec=".", stringsAsFactors =F, h = T) # Pelissier et al, 2011
  strata <- strata[c(1:100),]
  row.names(strata) <- strata$SpCode
  # Linking strata
  setdiff(row.names(species), strata$SpCode)
  setdiff(strata$SpCode, row.names(species))
  strata['agja',]$SpCode <- 'agla'
  strata['anme',]$SpCode <- 'anmo'
  strata['blme',]$SpCode <- 'blse'
  strata['chla',]$SpCode <- 'chro'
  strata['faze',]$SpCode <- 'pape'
  strata['glmc',]$SpCode <- 'glma'
  strata['lemo',]$SpCode <- 'leca'
  strata[which(strata$LatinName == 'Glochidion'),]$SpCode <- 'glel'
  row.names(strata) <- strata$SpCode
  species$Strata <- strata$PotStrata[match(row.names(species), row.names(strata))]
  species$LatinName[which(is.na(species$Strata))]
  species$Strata[which(is.na(species$Strata))] <- c(1, 2, 3, 3, 3, 3, 3,2, 3, 0, 3, 3, 1)
  # Errors corrections
  species['agin',] <- c('agin', 'Agrind', "Agrostistachys indica", 'Euphorbiaceae', 'Agrostistachys', 'indica', NA, 'agin', NA)
  species[which(species$Family == 'Flacourtiaceae'),4] <- 'Salicaceae'
  species[which(species$Family == 'Steruliaceae'),4] <- 'Malvaceae'
  species[which(species$Family == 'Cesaliniaceae'),4] <- 'Fabaaceae'

  return(species)
}
