#' Rename clades for easy recall
#' 
#' Converts a list of clades (e.g., "1 2 3 4" as a clade) and
#' returns a list of parsed clades, converting numbers to names using a set of trees.
#' Called internally by the slide and cumulative analyses, not user-facing.
#'
#' @param clades A list of clades, as in the first column of a cladetable in an rwty.slide or rwty.cumulative object
#' @param treelist A list of trees, used for getting tip names
#'
#' @return output A list of clades with parsed tip names


parse.clades <- function(clades, treelist){
    
    tipnames <- treelist$TipLabel
    
    output <- vector(mode="character", length=length(clades))
    
    for(i in 1:length(clades)){
        # Convert factor to set of indices
        nums <- as.numeric(unlist(strsplit(as.character(clades[i]), split=" ")))
        
        # Convert indices to tip names
        output[i] <- paste(treelist$tip.label[[1]][nums], collapse=", ")
    }
    
    output
}