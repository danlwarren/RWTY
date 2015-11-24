#' Returns clade names and frequencies
#' 
#' Uses ape functionality to get the frequencies and names of clades in an MCMC chain
#' or subset thereof.
#'
#' @param x A multiPhylo or rwty.trees object
#' @param start The index of the first tree to consider in calcuating frequencies
#' @param end The index of the last tree to consider in calculating frequencies
#' @param check.labels See documentation for ape function prop.part
#' 
#' @return clade.df A data froma containing clade names and frequencies
#'
#' @keywords Clade frequencies, consensus, mcmc, phylogenetics
#'
#' @export clade.freq
#' @examples
#' data(fungus)
#' clade.freq(fungus$Fungus.Run1, start=10, end=100)

# Modified from prop.part in APE, returning data in a more useful format
clade.freq <- function (x, start, end, check.labels = TRUE) {
  if(class(x) == "rwty.trees"){x <- x$trees}  
  obj <- list(x[start:end])
  if (length(obj) == 1 && class(obj[[1]]) != "phylo") 
    obj <- obj[[1]]
  ntree <- length(obj)
  if (ntree == 1) 
    check.labels <- FALSE
  if (check.labels) 
    obj <- .compressTipLabel(obj)
  for (i in 1:ntree) storage.mode(obj[[i]]$Nnode) <- "integer"
  obj <- .uncompressTipLabel(obj)
  clades <- .Call("prop_part", obj, ntree, TRUE, PACKAGE = "ape")
  cladefreqs <- as.numeric(as.character(attr(clades, which="number")[1:length(clades)] ))
  cladefreqs <- cladefreqs/ntree
  tiplabels <- as.character(obj[[1]]$tip.label)
  cladenames <- rep(NA, length(clades))
  for(i in 1:length(clades)){
    cladenames[i] <- paste(clades[[i]], collapse=" ")
  }
  clade.df <- data.frame(cladenames, cladefreqs)  
  clade.df
  #output <- list("cladefreqs" = clade.df, "tiplabels" = tiplabels)
  #output
}