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


tree.dist.matrix.liam <- function(trees){
    if(class(trees)!="multiPhylo")
        stop("trees should be an object of class \"multiPhylo\"")
    N<-length(trees)
    RF<-matrix(0,N,N)
    
    #if(any(sapply(unclass(trees),is.rooted))){
    #    cat("Some trees are rooted. Unrooting all trees.\n")
    #    trees<-lapply(unclass(trees),unroot)
    #}
    
    foo<-function(pp) lapply(pp,function(x,pp)
    sort(attr(pp,"labels")[x]),pp=pp)
    xx<-lapply(unclass(trees),function(x) foo(prop.part(x)))
    for(i in 1:(N-1)) for(j in (i+1):N)
        RF[i,j]<-RF[j,i]<-2*sum(!xx[[i]]%in%xx[[j]])
    RF
}
