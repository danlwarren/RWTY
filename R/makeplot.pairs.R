#' Plotting parameters against each other
#' 
#' Makes a plot matrix of each parameter against each other (including the topology) in your analysis. The default behaviour
#' is to just plot the first two columns of your parameter file (after removing the column for the generation number) as well
#' as the topological distance. This usually means that you see a pairs plot with the likelihood, the tree length, and the tree toppology. 
#' We do this because some parameter files contain so many columns that the plot matrix becomes too 
#' busy. To include parameters of your choice, use the 'parameters' argument. In this function, the topological distance is
#' calculate from the first tree in every chain.
#'
#' @param chains A set of rwty.trees objects.
#' @param burnin The number of trees to omit as burnin. 
#' @param treedist the type of tree distance metric to use, can be 'PD' for path distance or 'RF' for Robinson Foulds distance
#' @param params 'NA', 'all', or a vector of column names to include in the plot. 'NA' gives the default behaviour (see above). 'all' plots all columns (watch out!). Choose specific columns by name with a vector.
#' @param strip Number indicating which column to strip off (i.e., strip=1 removes first column, which is necessary for most MCMC outputs in which the first column is just the generation).
#'
#' @return pairs.plot Returns a ggplot object.
#'
#' @keywords parameter, plot, convergence, mcmc, phylogenetics
#'
#' @export makeplot.pairs
#' @examples
#' data(salamanders)
#' makeplot.pairs(salamanders[1], burnin=20)
#'
#' # plot all the variables
#' makeplot.pairs(salamanders[1], burnin=20, params = 'all')
#'
#' # plot specific the variables (note: you always get the topological distance)
#' makeplot.pairs(salamanders[1], burnin=20, params = c('pi.A.', 'pi.C.', 'pi.G.', 'pi.T.'))


makeplot.pairs <- function(chains, burnin = 0, treedist = 'PD', params = NA, strip = 1){

    chains = check.chains(chains)
    chain = chains[[1]]
    if(is.null(chain$ptable)) stop("No parameters associated with your chains")

    param.names <- names(chain$ptable)[-strip]

    if(length(params) == 1 && is.na(params)){
        param.names = param.names[1:2]
    }else if(length(params)==1 && params=='all'){
        param.names = param.names
    }else{
        if(is.null(params)){ stop("couldn't understand your 'params' argument. Please check and try again")}

        problems = setdiff(params, param.names)
        if(length(problems)>0){
            stop(paste(c("The following names you suppied are not parameters in your parameter table ", "'", problems, "'")))
        }
        param.names = params
    }

    chains = add.names(chains)


    plots = lapply(chains, do.pairs.plot, burnin = burnin, params = param.names, treedist = treedist)

    names(plots) = names(chains)

    return(plots)
}

add.names <- function(chains){

    for(i in 1:length(chains)){
        chains[[i]]$name = names(chains)[i]
    }
    return(chains)
}

do.pairs.plot <- function(chain, burnin = 0, params, treedist){

    ptable = combine.ptables(chain, burnin)
    name = chain$name
    chains = check.chains(chain)

    ptable = ptable[c(params, 'chain')]

    # we use the first tree in each chain as the focal tree, 
    # this is so that the distances look nicer in the plots
    focal.tree = chains[[1]]$trees[1]
    distances = tree.distances.from.first(chains, burnin, focal.tree = focal.tree, treedist = treedist)        

    ptable$topological.distance = distances$topological.distance

    pairs.plot = ggpairs(ptable, 
                    upper = list(continuous = "density"),
                    diag = list(continuous = "barDiag"),
                    columns = c(params, 'topological.distance'),
                    title = name
                    )

    return(pairs.plot)

}