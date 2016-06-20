#' @importFrom plyr ddply
#' @importFrom sp merge
#' @importFrom Hmisc wtd.var wtd.mean
NULL

#' Community weighted trait
#'
#' Compute a community weighted metric (mean or variance) with traits
#'
#' @param Trees data.frame. Trees data frame
#' @param com SpatialGridDataFrame. Communities with their environment (abiotic
#'   and biotic)
#' @param traits char. selected traits
#' @param metric char. selected metric to be computed on community
#' @param weights char. selected weights
#' @param env char. selected environmental variables to be kept
#'
#' @return a list with a community grid data frame for each weighting methods
#'   containing the environmental values, and the community weighted traits by
#'   defined metrics
#'
#' @export
#'
#' @examples
#'
CWT <- function(Trees, com,
                traits = c('LES', 'SLA', 'WES', 'LDMC', 'WD', 'Thick', 'LA'),
                metric = c('mean', 'variance'),
                weights = c('abundance', 'presence-absence', 'basal area'),
                env = c('Slope', 'Curvature', 'Wetness', 'SouthWesterness', 'Canopy', 'BA')){

  weights <- unlist(lapply(as.list(weights), function(x){switch (x,
                                                                 'abundance' = 'ab',
                                                                 'presence-absence' = 'pa',
                                                                 'basal area' = 'ba')}))
  com <- com[c('id', env)]
  cwt.l <- list()

  for (wgt in weights) {

    # Preparing result
    i <- (length(cwt.l) + 1)
    cwt.l[[i]] <- com
    names(cwt.l)[i] <- switch (wgt,
                                 'ab' = 'abundance',
                                 'pa' = 'presence-absence',
                                 'ba' = 'basal area'
    )

    # Trees table
    trees <- switch (wgt,
                     'ab' = Trees,
                     'pa' = Trees[-which(duplicated(cbind(Trees$SpCode, Trees$com))),],
                     'ba' = TreesBA <- Trees[-which(is.na(Trees$`2013_girth`)),]
    )

    for (m in metric) {

      # Computing CWT
      if(wgt %in% c('ab', 'pa')){

        fun <- switch (m,
                       'mean' = base::mean,
                       'variance' = stats::var
        )
        cwt <- aggregate(trees, by = list(trees$com), fun, na.rm = T)
        names(cwt)[1] <- 'id'
        cwt <- cwt[which(names(cwt) %in% c('id', traits))]

      } else if (wgt == 'ba'){

        cwt <- switch (m,
                       'mean' = data.frame(lapply(as.list(traits),
                                                  function(t){ddply(trees, ~ com,
                                                                    function(x){Hmisc::wtd.mean(x[,which(names(x) == t)], x$`2013_girth`/2*pi, na.rm = T)})[,2]})),
                       'variance' = data.frame(lapply(as.list(traits),
                                                      function(t){ddply(trees, ~ com,
                                                                        function(x){Hmisc::wtd.var(x[,which(names(x) == t)], x$`2013_girth`/2*pi, na.rm = T)})[,2]}))
        )
        cwt <- cbind(ddply(trees, ~ com, mean)[,1], cwt)
        names(cwt) <- c('id', traits)

      }

      # Adding result
      end <- switch (m,
                     'mean' = '.cwm',
                     'variance' = '.cwv'
      )
      names(cwt)[-1] <- paste0(names(cwt), end)[-1]
      cwt.l[[i]] <- sp::merge(cwt.l[[i]], cwt)
    }
  }
  return(cwt.l)
}
