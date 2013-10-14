tree.dist.matrix <- function(trees, treenames=names(trees)){
    N <- length(trees)

    if(N != length(treenames)){
        stop("Names and tree list must be the same length")
    }
    
    #Create an empty matrix for results
    RF <- matrix(0, N, N)
        
    for(i in 1:(N-1)){
        #print(paste("Tree", i, "of", N))        
        for(j in (i+1):N){
            RFd <- RF.dist(trees[[i]],trees[[j]])
            if(RFd==0) RFd = 0.000000001
            RF[i,j]<-RF[j,i]<-RFd
        }
    }

    #Row and column names
    rownames(RF) <- treenames
    colnames(RF) <- treenames
    
    RF
}




        
