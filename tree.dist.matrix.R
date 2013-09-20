tree.dist.matrix <- function(x, treenames){
    
    if(length(x) != length(treenames)){
        stop("Names and tree list must be the same length")
    }
    
    #Create an empty matrix for results
    output <- matrix(ncol=length(x), nrow=length(x))
    
    #Stepping through trees in x to create an upper diagonal matrix of
    #tree distances
    for(i in 1:length(x)){
        for(j in i:length(x)){
            if(j <= length(x)){
                print(paste("Comparing", i, "to", j))
                output[j,i] <- output[i,j] <- output[i,j] <- RF.dist(x[[i]], x[[j]])
            }
        }
    }
    
    #Row and column names
    rownames(output) <- treenames
    colnames(output) <- treenames
    
    output
}