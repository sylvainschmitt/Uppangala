#' @importFrom FD dbFD
NULL

#' Null model
#'
#' Function to compute null models
#'
#' @param com list. List of communities
#' @param PFT data.frame. PFT values by species
#' @param formulas list. Model formulas
#' @param SpCode dta.frame. Link between species nd they're code
#' @param traits char. Choosen traits and strategy axes
#' @param env char. Choosen environmental variables
#' @param type char. Communti weighted trait metric
#' @param n int. number of repetitions
#' @param time logical. Use a timer
#' @param verbose logical. Use a counter
#'
#' @return List of communities null models
#' @export
#'
#' @examples
#'
null_model <- function(com,
                       Trees,
                       PFT,
                       formulas,
                       SpCode,
                       traits = c('LES', 'SLA', 'WES','WD', 'LDMC', 'Thick', 'LA'),
                       env = c('Slope', 'Curvature', 'Wetness', 'SW', 'Canopy', 'BA'),
                       type = c('CWM', 'CWV'),
                       n = 999,
                       time = T,
                       verbose = T){

  t0 <- Sys.time()
  nstat <- list(abundance = matrix(ncol = length(formulas[[1]]), nrow = n, dimnames = list(1:n, names(formulas[[1]]))),
                `presence-absence` = matrix(ncol = length(formulas[[2]]), nrow = n, dimnames = list(1:n, names(formulas[[2]]))),
                `basal area` = matrix(ncol = length(formulas[[3]]), nrow = n, dimnames = list(1:n, names(formulas[[3]])))
                )

  # Null Communities
  for(i in 1:n){
    # Creating random PFT data
    nPFT <- PFT
    row.names(nPFT) <- sample(row.names(nPFT))
    nPFT$Sp_Code <- SpCode[row.names(nPFT)]
    # Merging data to trees
    nTrees <- Trees[which(names(Trees) %in% c('SpCode', 'com', '2013_girth'))]
    nTrees$ID <- row.names(Trees)
    nTrees <- merge(nTrees, nPFT, by.x = 'SpCode', by.y = 'Sp_Code')
    row.names(nTrees) <- nTrees$ID
    nTrees <- nTrees[-which(names(nTrees) == 'ID')]
    # Computing CWT for trees
    metric <- lapply(as.list(type), function(t){switch (t,
                                                        'CWM' = 'mean',
                                                        'CWV' = 'variance'
    )})
    ncom <- CWT(nTrees, com[[1]], traits = traits, metric = metric, weights = names(com), env = env)
    # Addind FD values
    x <- data.frame(nPFT[-1], row.names = nPFT$Sp_Code)
    a <- data.frame(tapply(rep(1, length(nTrees[,1])), list(nTrees$com, nTrees$SpCode), sum))
    a[is.na(a)] <- 0
    x <- x[names(a),]
    FD <- as.data.frame(dbFD(x, a, corr = 'cailliez', calc.CWM = F, print.pco = F, messages = F))[-c(1:2,4)]
    names(FD) <- paste0(names(FD), '.fd')
    FD$id <- row.names(FD)
    ncom$abundance <- merge(ncom$abundance, FD)
    # Getting model deviance
    nrow <-  mapply(function(c, f){unlist(lapply(f, function(x){deviance(lm(x, c))}))}, c = ncom, f = formulas, SIMPLIFY = F)
    for (j in names(nrow)) {
      nstat[[j]][i,] <- nrow[[j]]
    }
    # Counter
    if(verbose){
      print(i)
    }
  }

  # Model deviance and p.value
  stat <- mapply(function(c, f){unlist(lapply(f, function(x){deviance(lm(x, c))}))}, c = com, f = formulas)
  pval <- mapply(function(s,z){
    mapply(function(x,y){
      (rank(c(x,y))/(n+1))[1]
    }, x = as.list(s), y = as.list(data.frame(z)))
  }, s = stat, z = nstat)
  row.names(pval) <- row.names(stat)

  # Timer
  if(time){
    print(Sys.time() - t0)
  }

  return(list(formula = lapply(formulas, function(x){lapply(x, as.character)}),
              rep = n,
              nstat = nstat,
              stat = stat,
              p.value = pval))
}
