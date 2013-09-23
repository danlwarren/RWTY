tree.dist.matrix <- function(x, treenames=names(x)){
    
    if(length(x) != length(treenames)){
        stop("Names and tree list must be the same length")
    }
    
    #Create an empty matrix for results
    output <- matrix(ncol=length(x), nrow=length(x))
    
    #Stepping through trees in x to create an upper diagonal matrix of
    #tree distances
    for(i in 1:length(x)){
        print(paste("Tree", i, "of", length(x)))
        for(j in i:length(x)){
            if(j <= length(x)){

                rfd <- RF.dist(x[[i]], x[[j]])
                if(rfd==0){rfd=0.0000001} # MDS can't deal with zeros, but it can deal with v. small values
                output[j,i] <- output[i,j] <- rfd
            }
        }
    }
    
    #Row and column names
    rownames(output) <- treenames
    colnames(output) <- treenames
    
    output
}