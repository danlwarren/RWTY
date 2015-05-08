#' New style plotting of cumulative and slide objects
#' 
#' Takes a table from cumulative.freq or slide.freq as input.  
#' Numclades gives the number of clades to plot, starting from the
#' top.  Since cumulative.freq and slide.freq both sort by sd these
#' will by default be the most variable clades.
#'
#' @param x An rwty.slide or rwty.cumulative object \code{x}
#' @param numclades The number of clades to plot.  The clades with the highest sd in clade frequency are plotted first, so numclades = 10 will be the 10 most variable clades in the chain. \code{numclades}
#'
#' @return thisplot Returns a ggplot object.
#'
#' @keywords cumulative, sliding window, mcmc, phylogenetics, plot
#'
#' @export
#' 
#' @examples
#' data(fungus)
#' slide.data <- slide.freq(run1$trees, burnin=100, window.size=20, gens.per.tree=10000)
#' cpplot <- plot.cladeprobs(input.table = slide.data$slide.table, numclades=25)

plot.param <- function(chains, burnin = 0, parameter = "lnL", facet=TRUE){ 

    ptable = merge.ptables(chains, burnin)

    if(parameter %in% names(ptable)){

        param.plot =  ggplot(ptable, aes_string(x="generation", y=parameter)) + 
                        geom_line(aes(colour = chain)) + 
                        ggtitle(parameter)

        if(facet) param.plot = param.plot + facet_wrap(~chain, ncol=1) + theme(legend.position="none")

        return(param.plot)

    }else{
        stop(sprintf("The variable '%s' is not a column in the table of parameters you have supplied", parameter))
    }

}