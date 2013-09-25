# This function takes a vector of clades and a list of trees
# and uses the TipLabel attribute of the tree list to return
# clades using tip names instead of numbers.

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