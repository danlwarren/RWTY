#' Returns clade names and frequencies
#'
#' Uses ape functionality to get the frequencies and names of clades in an MCMC chain
#' or subset thereof.
#'
#' @param x A multiPhylo or rwty.chain object
#' @param start The index of the first tree to consider in calcuating frequencies
#' @param end The index of the last tree to consider in calculating frequencies
#' @param rooted (TRUE/FALSE).  Tells RWTY whether your trees are rooted or not.
#' @param ... Arguments to be passed to ape's prop.part function
#'
#' @return clade.df A data froma containing clade names and frequencies
#'
#' @keywords Clade frequencies, consensus, mcmc, phylogenetics
#'
#' @export clade.freq
#' @examples
#' \dontrun{
#' data(fungus)
#' clade.freq(fungus$Fungus.Run1, start=10, end=100)
#' }

# Modified from prop.part in APE, returning data in a more useful format
clade.freq <- function (x, start, end, rooted=FALSE, ...) {

  if(class(x) == "rwty.chain"){
    x <- x$trees
  }

   if (length(x) == 1 && class(x[[1]]) == "multiPhylo"){
     x <- x[[1]]
   }

  x <- x[start:end]

  clades <-  prop.part(x)

  # Sort tip labels alphabetically and update clade indices to match
  y <- attr(clades, "labels")
  z <- sort(y)
  for (i in seq_along(clades)) {
      clades[[i]] <- which(z %in% y[clades[[i]]])
  }
  attr(clades, "labels") <- z

  if(!rooted){
    clades <- postprocess.prop.part(clades)
  }

  cladefreqs <- as.numeric(as.character(attr(clades, which="number")[1:length(clades)] ))

  cladefreqs <- cladefreqs/length(x)

  tiplabels <- as.character(x[[1]]$tip.label)

  cladenames <- rep(NA, length(clades))

  for(i in 1:length(clades)){
    cladenames[i] <- paste(clades[[i]], collapse=" ")
  }

  # dropping split corresponding to all tips
  ind.all <- which(sapply(clades, length) == length(x[[1]]$tip.label))
  if (!(length(ind.all) == 1 && cladefreqs[ind.all] == 1)) {
      warning("unable to find trivial split of all tips")
  }

  clade.df <- data.frame(
      cladenames = cladenames[-ind.all],
      cladefreqs = cladefreqs[-ind.all]
  )

  return(clade.df)
}
