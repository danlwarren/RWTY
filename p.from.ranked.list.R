p.from.ranked.list <- function(null.dist, obs, type='smaller'){
    
    null.dist <- sort(null.dist)
    
    if(obs %in% null.dist){
        pos <- mean(which(null.dist==obs))
    } 
    else if(obs<min(null.dist)){
        pos <- 1
    }
    else if(obs>max(null.dist)){
        pos <- length(null.dist)+1
    }
    else{
        pos <- min(which(null.dist>obs))
        
    }
    
    if(type=='greater'){
        pos <- length(null.dist)+1 - pos
    }
    
    p <- pos/(length(null.dist)+1)
    p
}