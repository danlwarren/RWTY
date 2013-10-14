#' A one sentence description of what your function does
#' 
#' A more detailed description of what the function is and how
#' it works. It may be a paragraph that should not be separated
#' by any spaces. 
#'
#' @param inputParameter1 A description of the input parameter \code{inputParameter1}
#' @param inputParameter2 A description of the input parameter \code{inputParameter2}
#'
#' @return output A description of the object the function outputs 
#'
#' @keywords keywords
#'
#' @export
#' 
#' @examples
#' R code here showing how your function works

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