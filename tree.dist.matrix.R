tree.dist.matrix <- function(x, burnin=0){
    
    #Create an empty matrix for results
    output <- matrix(ncol=length(x) - burnin, nrow=length(x) - burnin)
    print(dim(output))
    #Stepping through trees in x to create an upper diagonal matrix of
    #tree distances
    for(i in (burnin+1):length(x)){
        for(j in i:length(x)){
            if(j <= length(x)){
                print(paste("Comparing", i, "to", j))
                output[i-burnin,j-burnin] <- RF.dist(x[[i]], x[[j]])
                output[j-burnin,i-burnin] <- output[i-burnin,j-burnin]
            }
        }
    }
    
    #Row and column names
    treenames <- names(x)[(burnin+1):length(x)]
    rownames(output) <- treenames
    colnames(output) <- treenames
    
    output
}