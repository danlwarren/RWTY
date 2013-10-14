cummean <- function(x){
    r <- (cumsum(as.numeric(x)))/seq(1:length(x))
    r
}