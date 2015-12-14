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
#' @export makeplot.cladevar
#' @examples
#' data(fungus)
#' slide.data <- slide.freq(fungus$Fungus.Run1$trees, burnin=20, window.size=20, gens.per.tree=40000)
#' makeplot.cladevar(slide.data$slide.table, numclades=100)

makeplot.cladevar <- function(input.table, numclades=nrow(input.table)){ 
    # plot variation in clade frequencies between windows
    
    x <- input.table[1:numclades,!(names(dat) %in% c("mean", "sd"))] #Stripping off mean and SD
    
    # this is a df of absolute differences in pp variation between windows
    d <- t(apply(x, 1, function(z) abs(diff(as.numeric(z)))))
    d <- as.data.frame(d)
    colnames(d) <- colnames(x)[2:ncol(x)]
    d$clade <- rownames(d)
    d <- melt(d, id.vars="clade")
    colnames(d) <- c("Clade", "Generations", "Variation.in.posterior.probability")
    d$Clade <- as.factor(d$Clade)
    
    thisplot <- ggplot(data=d, aes(x=as.numeric(as.character(Generations)), y=Variation.in.posterior.probability, group=Generations)) + 
        geom_boxplot() +
        theme(legend.position="none") +
        xlab("Generations") + 
        ylab("Variation in posterior probability") +
        theme(axis.title.x = element_text(vjust = -.5), axis.title.y = element_text(vjust=1.5))
    
    return(thisplot)
}