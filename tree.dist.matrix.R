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


## functions computes Robinson-Foulds distance between all trees in a list of class "multiPhylo"
## works only for unrooted trees (if trees are rooted, them will be unrooted)
## written by Liam J. Revell

multiRF<-function(trees){
    if(class(trees)!="multiPhylo") stop("trees should be an object of class \"multiPhylo\"")
    N<-length(trees)
    RF<-matrix(0,N,N)
    if(any(sapply(unclass(trees),is.rooted))){
        cat("Some trees are rooted. Unrooting all trees.\n")
        trees<-lapply(unclass(trees),unroot)
    }
    foo<-function(pp) lapply(pp,function(x,pp) sort(attr(pp,"labels")[x]),pp=pp)
    xx<-lapply(unclass(trees),function(x) foo(prop.part(x)))
    for(i in 1:(N-1)) for(j in (i+1):N) RF[i,j]<-RF[j,i]<-2*sum(!xx[[i]]%in%xx[[j]])
    RF
}

        
