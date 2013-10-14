#' A one sentence description of what your function does
#' 
#' A more detailed description of what the function is and how
#' it works. It may be a paragraph that should not be separated
#' by any spaces. 
#'
#' @param inputParameter1 A description of the input parameter \code{inputParameter1}
#' @param inputParameter2 A description of the input parameter \code{inputParameter2}
#'
#' @return output A description of the object the function outputs 
#'
#' @keywords keywords
#'
#' @export
#' 
#' @examples
#' R code here showing how your function works

plot.cladevar <- function(input.table, numclades=nrow(input.table)){ 
    # plot variation in clade frequencies between windows
    
    x <- input.table[1:numclades,2:length(input.table) - 2] #Stripping off mean and SD
    
    # this is a df of absolute differences in pp variation between windows
    d <- t(apply(x, 1, function(z) abs(diff(as.numeric(z)))))
    d <- as.data.frame(d)
    colnames(d) <- colnames(x)[2:ncol(x)]
    d$clade <- rownames(d)
    d <- melt(d, id.vars="clade")
    colnames(d) <- c("Clade", "Generations", "Variation.in.posterior.probability")
    d$Clade <- as.factor(d$Clade)
    
    thisplot <- ggplot(data=d, aes(x=as.numeric(Generations), y=Variation.in.posterior.probability, group=Generations)) + 
        geom_boxplot() +
        theme(legend.position="none") +
        xlab("Generations")
    
    thisplot
}