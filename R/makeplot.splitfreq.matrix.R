#' Plots a matrix of split frequency comparisons between multiple MCMC chains.
#' 
#' This function takes list of rwty.chain objects, and returns a scatterplot matrix
#' in which each plot shows the split frequencies of all clades that appear in one or both 
#' MCMC chains at least once. In the upper diagonal, we show the correlation between the split
#' frequencies (Pearson's R), and the Average Standard Deviation of the split frequencies.
#'
#'
#' @param chains A list of rwty.chain objects.
#' @param burnin The number of trees to eliminate as burnin 
#'
#' @return output A list of two plots: the first is a matrix of scatterplots, where each point is a clade, and the values are the split frequencies of that clade in the post-burnin trees of each chain. The second plot is a tree of the chains clustered by their ASDSFs.
#'
#' @keywords MCMC phylogenetics consensus clade frequency convergence ASDSF
#'
#' @export makeplot.splitfreq.matrix
#' @examples
#' \dontrun{
#' data(salamanders)
#' makeplot.splitfreq.matrix(salamanders[1:4], burnin = 20)
#' }

makeplot.splitfreq.matrix <- function(chains, burnin = 0){

  print(sprintf("Creating split frequency matrix and ASDSF clustering plots"))

  dat = get.comparison.table(chains, burnin, min.freq = 0)

  asdsf = dat$asdsf
  dat = dat$cladetable

  dat = dat[,(names(dat) %in% names(chains))] #keep only the chain values 

  pdf(NULL)
  dev.control(displaylist="enable")

  pairs2(dat, lower.panel = panel.smooth, upper.panel = panel.cor, pch = 20, col = rgb(0, 0, 1, 0.2))
  title("Split frequency comparisons")
  splitfreq.matrix = recordPlot()

  invisible(dev.off())

  if(all(asdsf  == 0)){
    print("No non-zero ASDSF values, skipping ASDSF tree")  
  }
  else{

    hc <- hclust(asdsf)
    asdsf.tree <- ggdendrogram(hc, rotate=TRUE, theme_dendro = FALSE) + ylab("Pairwise ASDSF") + xlab("")

  }

  return(list("splitfreq.matrix" = splitfreq.matrix, "asdsf.tree" = asdsf.tree))

}

# this function from here: http://stackoverflow.com/questions/9680783/how-can-i-change-the-axis-position-for-pairs
pairs2 <- 
  function (x, labels, panel = points, ..., lower.panel = panel, 
            upper.panel = panel, diag.panel = NULL, text.panel = textPanel, 
            label.pos = 0.5 + has.diag/3, cex.labels = NULL, font.labels = 1, 
            row1attop = TRUE, gap = 1) 
  {
    textPanel <- function(x = 0.5, y = 0.5, txt, cex, font) text(x, 
                                                                 y, txt, cex = cex, font = font)
    localAxis <- function(side, x, y, xpd, bg, col = NULL, main, 
                          oma, ...) {
      if (side%%2 == 1) 
        Axis(x, side = side, xpd = NA, ...)
      else Axis(y, side = side, xpd = NA, ...)
    }
    localPlot <- function(..., main, oma, font.main, cex.main) plot(...)
    localLowerPanel <- function(..., main, oma, font.main, cex.main) lower.panel(...)
    localUpperPanel <- function(..., main, oma, font.main, cex.main) upper.panel(...)
    localDiagPanel <- function(..., main, oma, font.main, cex.main) diag.panel(...)
    dots <- list(...)
    nmdots <- names(dots)
    if (!is.matrix(x)) {
      x <- as.data.frame(x)
      for (i in seq_along(names(x))) {
        if (is.factor(x[[i]]) || is.logical(x[[i]])) 
          x[[i]] <- as.numeric(x[[i]])
        if (!is.numeric(unclass(x[[i]]))) 
          stop("non-numeric argument to 'pairs'")
      }
    }
    else if (!is.numeric(x)) 
      stop("non-numeric argument to 'pairs'")
    panel <- match.fun(panel)
    if ((has.lower <- !is.null(lower.panel)) && !missing(lower.panel)) 
      lower.panel <- match.fun(lower.panel)
    if ((has.upper <- !is.null(upper.panel)) && !missing(upper.panel)) 
      upper.panel <- match.fun(upper.panel)
    if ((has.diag <- !is.null(diag.panel)) && !missing(diag.panel)) 
      diag.panel <- match.fun(diag.panel)
    if (row1attop) {
      tmp <- lower.panel
      lower.panel <- upper.panel
      upper.panel <- tmp
      tmp <- has.lower
      has.lower <- has.upper
      has.upper <- tmp
    }
    nc <- ncol(x)
    if (nc < 2) 
      stop("only one column in the argument to 'pairs'")
    has.labs <- TRUE
    if (missing(labels)) {
      labels <- colnames(x)
      if (is.null(labels)) 
        labels <- paste("var", 1L:nc)
    }
    else if (is.null(labels)) 
      has.labs <- FALSE
    oma <- if ("oma" %in% nmdots) 
      dots$oma
    else NULL
    main <- if ("main" %in% nmdots) 
      dots$main
    else NULL
    if (is.null(oma)) {
      oma <- c(4, 4, 4, 4)
      if (!is.null(main)) 
        oma[3L] <- 6
    }
    opar <- par(mfrow = c(nc, nc), mar = rep.int(gap/2, 4), oma = oma)
    on.exit(par(opar))
    dev.hold()
    on.exit(dev.flush(), add = TRUE)
    for (i in if (row1attop) 
      1L:nc
         else nc:1L) for (j in 1L:nc) {
           localPlot(x[, j], x[, i], xlab = "", ylab = "", axes = FALSE, 
                     type = "n", ...)
           if (i == j || (i < j && has.lower) || (i > j && has.upper)) {
             box()
             # edited here...
             #           if (i == 1 && (!(j%%2) || !has.upper || !has.lower)) 
             #           localAxis(1 + 2 * row1attop, x[, j], x[, i], 
             #                       ...)
             # draw x-axis
             if (i == nc & j != nc) 
               localAxis(1, x[, j], x[, i], 
                         ...)
             # draw y-axis
             if (j == 1 & i != 1) 
               localAxis(2, x[, j], x[, i], ...)
             #           if (j == nc && (i%%2 || !has.upper || !has.lower)) 
             #             localAxis(4, x[, j], x[, i], ...)
             mfg <- par("mfg")
             if (i == j) {
               if (has.diag) 
                 localDiagPanel(as.vector(x[, i]), ...)
               if (has.labs) {
                 par(usr = c(0, 1, 0, 1))
                 if (is.null(cex.labels)) {
                   l.wid <- strwidth(labels, "user")
                   cex.labels <- max(0.8, min(2, 0.9/max(l.wid)))
                 }
                 text.panel(0.5, label.pos, labels[i], cex = cex.labels, 
                            font = font.labels)
               }
             }
             else if (i < j) 
               localLowerPanel(as.vector(x[, j]), as.vector(x[, 
                                                              i]), ...)
             else localUpperPanel(as.vector(x[, j]), as.vector(x[, 
                                                                 i]), ...)
             if (any(par("mfg") != mfg)) 
               stop("the 'panel' function made a new plot")
           }
           else par(new = FALSE)
         }
    if (!is.null(main)) {
      font.main <- if ("font.main" %in% nmdots) 
        dots$font.main
      else par("font.main")
      cex.main <- if ("cex.main" %in% nmdots) 
        dots$cex.main
      else par("cex.main")
      mtext(main, 3, 3, TRUE, 0.5, cex = cex.main, font = font.main)
    }
    invisible(NULL)
  }

panel.cor <- function(x, y, digits = 2, cex.cor, ...){
  # adapted from http://www.r-bloggers.com/scatter-plot-matrices-in-r/
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))

  # exclude clades where both have split freq of zero
  d = data.frame(x = x, y = y)
  keep = apply(d, 1, sum) > 0
  d = d[keep,]
  x = d$x
  y = d$y

  # correlation coefficient
  r <- cor(x, y)
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste("r = ", txt, sep = "")
  text(0.5, 0.7, txt)

  # ASDSF
  asdsf = mean(apply(d,1,sd))
  txt2 <- format(c(asdsf, 0.123456789), digits = digits)[1]
  txt2 <- paste("ASDSF = ", txt2, sep = "")
  text(0.5, 0.3, txt2)
}


get.comparison.table <- function(chains, burnin, min.freq){
  
  setnames = names(chains)

  # Get a starting table from the first chain
  clade.table <- clade.freq(chains[[1]], start=burnin, end=length(chains[[1]]$trees))
  if(is.na(setnames[1])){colnames(clade.table)[2] <- paste("set", 1, sep=".")}
  else{colnames(clade.table)[2] <- setnames[1]}
  
  # Populate the rest of the table, one chain at a time
  for(i in 2:length(chains)){
    thistable <- clade.freq(chains[[i]], start = burnin, end  = length(chains[[i]]$trees))
    clade.table <- merge(clade.table, thistable, by = "cladenames", all = TRUE) 
    
    # Either name it with the label provided or with the variable name
    if(is.na(setnames[i])){colnames(clade.table)[i+1] <- paste("set", i, sep=".")}
    else{colnames(clade.table)[i+1] <- setnames[i]}
  }
  
  # Set missing clades from each chain to zero
  clade.table[is.na(clade.table)] <- 0
  
  # Calculate the pairwise average standard deviation of split frequencies
  # (ASDSF) for clades occuring at a minimum frequency of min.freq
  # across a pair of chains.  
  d <- matrix(nrow=length(chains), ncol=length(chains))
  asdsf.clade.table <- clade.table

  for(i in 1:length(chains)){
    for(j in i+1:length(chains)){
      if(j <= length(chains)){
        temp.pair <- NULL
        temp.pair <- data.frame(cbind(asdsf.clade.table[,i+1],asdsf.clade.table[,j+1]))
        temp.pair <- temp.pair[apply(temp.pair, MARGIN = 1, function(x) any(x > min.freq)), ]
        temp.pair <- transform(temp.pair, SD=apply(temp.pair,1, sd, na.rm = TRUE))
        d[j,i] <- mean(temp.pair$SD)
      }
    }
  }
  colnames(d) <- names(clade.table)[2:(length(chains)+1)]
  rownames(d) <- names(clade.table)[2:(length(chains)+1)]
  d <- as.dist(d)
  
  # Summary stats, sort by SD
  thissd <- apply(clade.table[,2:length(clade.table[1,])], 1, sd)
  thismean <- apply(clade.table[,2:length(clade.table[1,])], 1, mean) 
  clade.table$sd <- thissd
  clade.table$mean <- thismean
  clade.table <- clade.table[order(clade.table$sd, decreasing=TRUE),]
  
  # Create a table that translates clade names
  translation.table <- cbind(as.numeric(clade.table[,1]), as.character(clade.table[,1]), parse.clades(clade.table[,1], chains[[1]]))
  clade.table[,1] <- as.numeric(clade.table[,1])
  
  output <- list("cladetable" = clade.table, "asdsf" = d, "asdsf.min.freq" = min.freq,
                 "translation" = translation.table)
  class(output) = "rwty.comparen"

  return(output)
}
