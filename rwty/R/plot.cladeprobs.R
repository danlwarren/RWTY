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

plot.cladeprobs <- function(input.table, numclades=20){ 
    # clade probability plot over generations
    
    # TODO should put a check in here to make sure it only accepts slidetest or cumtest objects
    
    x <- input.table[1:numclades,2:length(input.table) - 2] #Stripping off mean and SD
    x$clade <- rownames(x)
    x <- melt(x, id.vars="clade")
    colnames(x) <- c("Clade", "Generations", "Posterior.Probability")
    x$Clade <- as.factor(x$Clade)
    
    thisplot <- ggplot(data=x, aes(x=as.numeric(Generations), y=Posterior.Probability, group=Clade, color=Clade)) + 
        geom_line() +
        theme(legend.position="none") +
        xlab("Generations")
    
    thisplot
}