#' Plots distribution of variance in posterior probability estimates across clades.
#' 
#' Plots the distribution of variances in posterior probability estimates
#' across clades in a chain as a function of chain length.
#'
#' @param input.table Either a rwty.cumulative or rwty.slide object, or a cumulative or slide table from one of those objects.
#' @param numclades The number of clades to calculate stats for.  Defaults to all clades in the table.
#'
#' @return thisplot A ggplot object
#'
#' @keywords mcmc, phylogenetics, convergence, uncertainty
#'
#' @export
#' 
#' @examples
#' plot.cladevar(slideresults, numclades=100)

plot.cladevar <- function(input.table, numclades=nrow(input.table)){ 
    # plot variation in clade frequencies between windows
    if(class(input.table) == "rwty.cumulative"){input.table = input.table$cumulative.table}
    if(class(input.table) == "rwty.slide"){input.table = input.table$cumulative.table}
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