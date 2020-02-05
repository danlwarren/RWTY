#' Plot the Standard Deviation of Split Frequencies over the course of an MCMC.
#' 
#' This function takes two or more rwty.chain ojects and returns a plot of ASDSF as the run progresses.  
#' The solid line with points shows the Average Standard Deviation of Split Frequences at the current generation
#' The grey ribbon shows the upper and lower 95% quantiles of the SDSFs at the current generation
#'
#'
#'
#' @param chains A list of rwty.chain objects. 
#' @param burnin The number of trees to eliminate as burnin. Defaults to zero. 
#' @param window.size The number of trees between each point at which the ASDSFs is calculated (note, specified as a number of sampled trees, not a number of generations)
#' @param min.freq The minimum frequency for a node to be used for calculating ASDSF.
#' @param log.y Controls whether they Y axis is plotted on a log scale or not.  Which scale is more useful depends largely on the amount of disagreement between your chains.  Attempting to make an asdsf plot with a log Y axis for chains that include standard deviations of zero will result in warning messages.
#' 
#' @return output A cumulative plot of ASDSF across all chains
#'
#' @keywords MCMC, phylogenetics, ASDSF, cumulative
#'
#' @export makeplot.asdsf
#' @examples
#' \dontrun{
#' data(fungus)
#' p <- makeplot.asdsf(fungus, burnin = 20)
#' p
#' }

makeplot.asdsf <- function(chains, burnin = 0, window.size = 20, min.freq = 0.0, log.y = TRUE){
  
  print(sprintf("Creating ASDSF plot"))
  
  
  chains = check.chains(chains)
  labels = names(chains)
  slide.freq.list = slide.freq(chains, burnin, window.size)
  dat = get.asdsfs(slide.freq.list, min.freq)
  
  asdsf.plot <- ggplot(dat, aes(x = as.numeric(as.character(Generation)))) + 
    geom_line(aes(color = 14, y = min), linetype = 3) + 
    geom_line(aes(color = 13, y = lower.95), linetype = 2) +
    geom_line(aes(color = 12, y = lower.75), linetype = 7) +
    geom_line(aes(color = 12, y = upper.75), linetype = 7) +
    geom_line(aes(color = 13, y = upper.95), linetype = 2) + 
    geom_line(aes(color = 14, y = max), linetype = 3) + 
    geom_ribbon(aes(ymin = min, ymax = lower.95, fill = 14), alpha = 0.50) + 
    geom_ribbon(aes(ymin = lower.95, ymax = lower.75, fill = 13), alpha = 0.50) +
    geom_ribbon(aes(ymin = lower.75, ymax = upper.75, fill = 12), alpha = 0.50) +
    geom_ribbon(aes(ymin = upper.75, ymax = upper.95, fill = 13), alpha = 0.50) + 
    geom_ribbon(aes(ymin = upper.95, ymax = max, fill = 14), alpha = 0.50) + 
    geom_line(aes(y = ASDSF)) + 
    geom_point(aes(y = ASDSF)) +
    scale_color_viridis(begin = 0.2, end = .9, option = "D") +
    scale_fill_viridis(begin = 0.2, end = .9, option = "D") +
    expand_limits(y=0) +
    theme(legend.position="none") +   
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()) +     
    xlab("Generation") + 
    ggtitle("Average Standard Deviation of Split Frequencies") 
  
  if(log.y == TRUE){
    asdsf.plot <- asdsf.plot + scale_y_log10() + ylab("Log Standard Deviation of Split Frequencies")
  } else {
    asdsf.plot <- asdsf.plot + ylab("Log Standard Deviation of Split Frequencies")
  }
    
  
  return(list("asdsf.plot" = asdsf.plot))
}

get.asdsfs <- function(slide.freq.list, min.freq = 0.1){
  
  x = slide.freq.list
  
  # set initial values
  sets <- length(x)
  slide.wins <- ncol(x[[1]]$slide.table)-2
  
  slide.wins = length(setdiff(names(x[[1]]$slide.table), c("mean", "sd", "ess", "wcsf"))) #Remove mean and sd etc. columsn
  
  #use to label plot
  all_SDs <- list()
  
  #populate clade frequency tables, one for each window
  for(i in 1:slide.wins){
    winTable <- NULL
    
    #attach tip names to clade frequencies for the first chain(necessary because clade name/numbers may not match among chains)
    if(i==1){ #special case of first window
      winTable <- data.frame(cbind(x[[1]]$translation[,3],x[[1]]$slide.table[,i]), stringsAsFactors=FALSE)
    }
    else{ # subsequent windows
      winTable <- data.frame(cbind(x[[1]]$translation[,3],rowMeans(x[[1]]$slide.table[,1:i])), stringsAsFactors=FALSE) 
    }
    colnames(winTable) <- c("Tip names",names(x)[1])
    class(winTable[,2]) <- "numeric"
    # Populate the rest of the table, one chain at a time
    for (j in 2:sets){
      thisTable <- NULL
      
      if(i==1){ #special case of first window
        thisTable <- data.frame(cbind(x[[j]]$translation[,3],x[[j]]$slide.table[,i]), stringsAsFactors=FALSE)
      }
      else{ # subsequent windows
        thisTable <- data.frame(cbind(x[[j]]$translation[,3],rowMeans(x[[j]]$slide.table[,1:i])), stringsAsFactors=FALSE)
      }
      colnames(thisTable) <- c("Tip names",names(x)[j])
      winTable <- merge(winTable,thisTable, by="Tip names", all=TRUE)
      winTable[is.na(winTable)] <- 0
      class(winTable[,j+1]) <- "numeric"
    }
    # calculate ASDSF across all chains for each window
    # filter for clades below min freq then save ASDSF for that window
    winTable <- winTable[,-1]
    winTable <- winTable[apply(winTable, MARGIN = 1, function(x) any(x > min.freq)), ]
    all_SDs[[i]] <- apply(winTable,1, sd, na.rm = TRUE)
  }
  
  #now that we have collected SDSF for all windows, return a data frame
  generations <- as.numeric(as.character(names(x[[1]]$slide.table[1:slide.wins])))  
  d <- data.frame(split.frequency = unlist(all_SDs), 
                  Generation = rep(generations[1:length(all_SDs)],times = sapply(all_SDs,length)))
  
  
  dat = ddply(d, .(Generation), summarize, 
              ASDSF = mean(split.frequency), 
              upper.95 = quantile(split.frequency, c(0.975)), 
              lower.95 = quantile(split.frequency, c(0.025)), 
              upper.75 = quantile(split.frequency, c(0.875)), 
              lower.75 = quantile(split.frequency, c(0.125)), 
              min = min(split.frequency), 
              max = max(split.frequency))
  
  return(dat)
  
}
