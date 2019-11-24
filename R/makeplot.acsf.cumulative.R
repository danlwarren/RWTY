#' Plot the Change in Split Frequencies (CSF) in sliding windows over the course of an MCMC.
#' 
#' This function takes one or more rwty.chain ojects and returns a plot of CSF within each chain as the MCMC progresses.  
#' The solid line with points shows the Average Change in Split Frequencies (ACSF; it is average across the changes in split frequencies from all clades in the analysis) between this window and the previous window
#' The grey ribbon shows the upper and lower 95% quantiles of the CSFs between this window and the previuos window
#'
#' @param chains A list of rwty.chain objects. 
#' @param burnin The number of trees to eliminate as burnin. Defaults to zero. 
#' @param window.size The number of trees to include in each window (note, specified as a number of sampled trees, not a number of generations)
#' @param facet (TRUE/FALSE). TRUE: return a single plot with one facet per chain; FALSE: return a list of individual plots with one plot per chain 
#' 
#' @return output A plof of the CSF between sliding windows over all chains
#'
#' @return acsf.plot A ggplot object, or list of ggplot objects
#'
#' @keywords mcmc, phylogenetics, convergence, uncertainty
#'
#' @export makeplot.acsf.cumulative
#' @examples
#' \dontrun{
#' data(fungus)
#' makeplot.acsf.cumulative(fungus, burnin=20)
#' }

makeplot.acsf.cumulative <- function(chains, burnin = 0, window.size = 20, facet = TRUE){ 
  # plot variation in clade frequencies 
  print(sprintf("Creating cumulative ACSF plot"))
  
  chains = check.chains(chains)
  cumulative.freq.list = cumulative.freq(chains, burnin = burnin, window.size = window.size)
  
  dat.list = lapply(cumulative.freq.list, get.acsf)
  dat = do.call("rbind", dat.list)
  dat$Chain = get.dat.list.chain.names(dat.list)
  
  rownames(dat) = NULL
  title = "Cumulative Change in Split Frequencies"
  
  if(facet==TRUE){
    acsf.plot <- ggplot(dat, aes(x = "Generation")) + 
      geom_ribbon(aes_string(ymin = "min", ymax = "lower.95", fill = "Chain"), alpha = 0.10) + 
      geom_ribbon(aes_string(ymin = "lower.95", ymax = "lower.75", fill = "Chain"), alpha = 0.30) +
      geom_ribbon(aes_string(ymin = "lower.75", ymax = "upper.75", fill = "Chain"), alpha = 0.50) +
      geom_ribbon(aes_string(ymin = "upper.75", ymax = "upper.95", fill = "Chain"), alpha = 0.30) + 
      geom_ribbon(aes_string(ymin = "upper.95", ymax = "max", fill = "Chain"), alpha = 0.10) + 
      geom_line(aes_string(y = "ACSF", colour = "Chain")) + 
      geom_point(aes_string(y = "ACSF", colour = "Chain")) +
      scale_color_viridis(discrete = TRUE, end = 0.85) +
      scale_fill_viridis(discrete = TRUE, end = 0.85) +
      theme(legend.position="none") +   
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()) +              
      expand_limits(y = 0) +
      xlab("Generation") +
      ylab("Change in Split Frequency") + 
      facet_wrap(~Chain, ncol = 1) +
      ggtitle(title)
    acsf.plot <- list("acsf.cumulative.plot" = acsf.plot)
    
  }else{
    dat.list = split(dat, f = dat$Chain)
    acsf.plot = lapply(dat.list, single.acsf.plot, type = 'cumulative')
    for(i in 1:length(acsf.plot)){
      acsf.plot[[i]] = acsf.plot[[i]] + ggtitle(paste(title, "for", names(acsf.plot)[i]))
      names(acsf.plot)[i] = paste("acsf.cumulative.plot.", names(acsf.plot[i]), sep="")
    }
    
  }
  
  return(acsf.plot)
  
}

get.acsf <- function(freq.table){
  
  d = get.csf(freq.table)
  
  
  dat = ddply(d, .(Generation), summarize, 
              ACSF = mean(CSF), 
              upper.95 = quantile(CSF, c(0.975)), 
              upper.75 = quantile(CSF, c(0.875)), 
              lower.75 = quantile(CSF, c(0.125)), 
              lower.95 = quantile(CSF, c(0.025)), 
              min = min(CSF), 
              max = max(CSF))
  
  return(dat)
  
}


get.csf <- function(freq.table){
  # get a df of absolute differences in pp variation between windows
  if(class(freq.table) == "rwty.slide"){
    dat = freq.table$slide.table
  }else if(class(freq.table) == "rwty.cumulative"){
    dat = freq.table$cumulative.table
  }else{
    stop("ERROR: unknown type of frequency table passed to process.freq.table()")
  }
  
  dat = dat[,!(names(dat) %in% c("mean", "sd", "ess", "wcsf"))] #Remove mean and sd
  
  d <- t(apply(dat, 1, function(z) abs(diff(as.numeric(z)))))
  
  d <- as.data.frame(d)
  colnames(d) <- colnames(dat)[2:ncol(dat)] # differences of previous window
  d$clade <- rownames(d)
  d <- melt(d, id.vars="clade")
  colnames(d) <- c("Clade", "Generation", "CSF")
  d$Clade <- as.factor(d$Clade)
  
  return(d)
  
}