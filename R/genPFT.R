#' PFT data generation
#'
#' Generates PFT data set from external data collected on the field
#'
#' @return PFT data set
#' @export
#'
#' @examples
#' summary(genPFT())
#'
genPFT <- function(){
  # Opening
  path <- system.file('extdata', 'data.csv', package = 'Uppangala')
  PFT <- read.csv(path, stringsAsFactors = F, na.strings = c('#N/A', 'NA'))
  PFT <- PFT[-c(13:14)] # Deleting SD & VD in waiting of measurements
  # Factors
  PFT$Thick <- as.numeric(PFT$Thick)
  PFT$CE <- as.factor(tolower(PFT$CE))
  PFT$SP <- as.factor(PFT$SP)
  PFT$Sp_Code <- as.factor(PFT$Sp_Code)
  # Variables modification
  PFT$CEs <- as.numeric(as.character(substr(PFT$CE, 1, 1)))
  PFT$LTD <- as.numeric(PFT$LTD)
  PFT$LDMC = 1000 * PFT$LDMC # from g.g-1 to mg.g-1
  PFT$SLA = PFT$SLA / 1000 # from mm2.g-1 to m2.kg-1
  PFT$HA = PFT$HA / PFT$LA
  names(PFT)[which(names(PFT) == 'HA')] = 'SHA' # Specific holes area
  # Errors modification
  PFT$WD[which(PFT$WD > 1)] <- NA # Wood sample too small for correct WD measurement
  # Final
  PFT <- PFT[-which(duplicated(PFT)),]
  row.names(PFT) <- PFT$ID
  PFT <- PFT[c(2:5,12,6:11)]
  summary(PFT)
  return(PFT)
}
