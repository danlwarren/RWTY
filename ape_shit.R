setwd("~/Dropbox/R Projects/RWTY")
library(ape)
library(ggplot2)
library(reshape2)

test1 <- read.nexus(file="PCFW.nex.run1.t")
test2 <- read.nexus(file="PCFW.nex.run2.t")

# Modified from prop.part in APE, returning data in a more useful format
clade.freq <- function (x, start, end, check.labels = TRUE) {
  obj <- list(x[start:end])
  if (length(obj) == 1 && class(obj[[1]]) != "phylo") 
    obj <- obj[[1]]
  ntree <- length(obj)
  if (ntree == 1) 
    check.labels <- FALSE
  if (check.labels) 
    obj <- .compressTipLabel(obj)
  for (i in 1:ntree) storage.mode(obj[[i]]$Nnode) <- "integer"
  obj <- .uncompressTipLabel(obj)
  clades <- .Call("prop_part", obj, ntree, TRUE, PACKAGE = "ape")
  cladefreqs <- as.numeric(as.character(attr(clades, which="number")[1:length(clades)] ))
  cladefreqs <- cladefreqs/ntree
  tiplabels <- as.character(obj[[1]]$tip.label)
  cladenames <- rep(NA, length(clades))
  for(i in 1:length(clades)){
    cladenames[i] <- paste(clades[[i]], collapse=" ")
  }
  clade.df <- data.frame(cladenames, cladefreqs)  
  output <- list("cladefreqs" = clade.df, "tiplabels" = tiplabels)
  output
}

cumulative.freq <- function(x, burnin=0, window, gens.per.tree = 1, ...){ #Specify burnin in TREES, not GENERATIONS
  start <- burnin
  n.windows <- as.integer((length(x) - burnin)/window)
  print("Populating table...")
  allnames <- clade.freq(x, start = burnin, end  = length(x))$cladefreqs[,1] #Getting all clades from chain into table
  clade.table <- as.data.frame(matrix(0, ncol=0, nrow = length(allnames)))
  clade.table$cladenames <- allnames
  for(i in 1:n.windows){
    print(paste("window", i, "of", n.windows))
    thiswindow.table <- clade.freq(x, start = burnin, end  = burnin + (i) * window)
    clade.table <- merge(clade.table, thiswindow.table$cladefreqs, by = "cladenames", all = TRUE)
    colnames(clade.table)[i+1] <- ((i-1) * window * gens.per.tree) + (burnin * gens.per.tree)
  }
  clade.table[is.na(clade.table)] <- 0
  thissd <- apply(clade.table[,2:length(clade.table[1,])], 1, sd)
  thismean <- apply(clade.table[,2:length(clade.table[1,])], 1, mean) 
  clade.table$sd <- thissd
  clade.table$mean <- thismean
  clade.table <- clade.table[order(clade.table$sd, decreasing=TRUE),]
  output <- list("cladetable" = clade.table, "plot" = plawty(clade.table, ...))
  class(output) <- "rwty.cumulative"
  output
}

slide.freq <- function(x, burnin=0, window, gens.per.tree = 1, ...){ #Specify burnin in TREES, not GENERATIONS
  start <- burnin
  n.windows <- as.integer((length(x) - burnin)/window)
  print("Populating table...")
  allnames <- clade.freq(x, start = burnin, end  = length(x))$cladefreqs[,1] #Getting all clades from chain into table
  clade.table <- as.data.frame(matrix(0, ncol=0, nrow = length(allnames)))
  clade.table$cladenames <- allnames
  for(i in 1:n.windows){
    print(paste("window", i, "of", n.windows))
    thiswindow.table <- clade.freq(x, start = burnin + (i-1) * window, end  = burnin + (i) * window)
    clade.table <- merge(clade.table, thiswindow.table$cladefreqs, by = "cladenames", all = TRUE)
    colnames(clade.table)[i+1] <- ((i-1) * window * gens.per.tree) + (burnin * gens.per.tree)
  } 
  clade.table[is.na(clade.table)] <- 0
  thissd <- apply(clade.table[,2:length(clade.table[1,])], 1, sd)
  thismean <- apply(clade.table[,2:length(clade.table[1,])], 1, mean) 
  clade.table$sd <- thissd
  clade.table$mean <- thismean
  clade.table <- clade.table[order(clade.table$sd, decreasing=TRUE),]
  output <- list("cladetable" = clade.table, "plot" = plawty(clade.table, ...))
  class(output) <- "rwty.slide"
  output
}


# Takes a table from cumulative.freq or slide.freq as input.  
# Numclades gives the number of clades to plot, starting from the
# top.  Since cumulative.freq and slide.freq both sort by sd these
# will by default be the most variable clades.
plawty <- function(x, numclades=10){ 
  x <- x[1:numclades,2:length(x) - 2] #Stripping off mean and SD
  y <- as.numeric(colnames(x)[2:length(x)])
  thismax <- max(y)
  for(i in 1:length(x[,1])){
    y <- cbind(y, as.numeric(x[i,2:length(x[1,])]))
  }
  colnames(y) <- c("gen", as.character(x[,1]))
  y <- as.data.frame(y)
  y <- melt(y, id="gen")
  thisplot <- ggplot(data=y, aes(x=gen, y=value, colour=variable)) + 
    ylab("Posterior Probability") + xlab("Generations") + geom_line() + xlim(0, thismax) +
    ylim(0,1) + theme_bw() +  theme(axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.border = element_blank()) + theme(legend.position="none", 
    panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
    axis.text.x = element_text(size=12), axis.text.y = element_text(size=12),
    axis.title.x = element_text(size=14), axis.title.y = element_text(size=14))
  thisplot
}

compare.two <- function(x, y, burnin){
  start <- burnin
  print("Populating table...")
  table1 <- clade.freq(x, start = burnin, end  = length(x))$cladefreqs 
  table2 <- clade.freq(y, start = burnin, end  = length(y))$cladefreqs 
  clade.table <- merge(table1, table2, by = "cladenames", all = TRUE) 
  clade.table[is.na(clade.table)] <- 0
  colnames(clade.table)[2] <- deparse(substitute(x))
  colnames(clade.table)[3] <- deparse(substitute(y))
  thissd <- apply(clade.table[,2:length(clade.table[1,])], 1, sd)
  thismean <- apply(clade.table[,2:length(clade.table[1,])], 1, mean) 
  clade.table$sd <- thissd
  clade.table$mean <- thismean
  clade.table <- clade.table[order(clade.table$sd, decreasing=TRUE),]
  thisplot <- ggplot(data=comp, aes(x=test1, y=test2))+ theme_bw() +
    geom_point(shape = 19) + geom_abline(mapping = NULL, data = NULL, 
    stat = "abline", position = "identity", show_guide = FALSE, colour="grey") +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.border = element_blank(), axis.line = element_line(colour = "black"),
    axis.text.x = element_text(size=12), axis.text.y = element_text(size=12),
    axis.title.x = element_text(size=14), axis.title.y = element_text(size=14))
  output <- list("cladetable" = clade.table, "plot" = thisplot, "dist" =  mean(sqrt(2) * abs(clade.table[,2] - clade.table[,3])))
  class(output) <- "rwty.comparetwo"
  output
}

compare.n <- function(x, setnames=NA, burnin){ # In this case x is a list of treefiles
  print("Populating table...")
  print(paste("Working on set", 1))
  clade.table <- clade.freq(x[[1]], start=burnin, end=length(x[[1]]))$cladefreqs
  if(is.na(setnames[1])){colnames(clade.table)[2] <- paste("set", 1, sep=".")}
  else{colnames(clade.table)[2] <- setnames[1]}
  
  for(i in 2:length(x)){
    print(paste("Working on set", i))
    thistable <- clade.freq(x[[i]], start = burnin, end  = length(x[[i]]))$cladefreqs 
    clade.table <- merge(clade.table, thistable, by = "cladenames", all = TRUE) 
    if(is.na(setnames[i])){colnames(clade.table)[i+1] <- paste("set", i, sep=".")}
    else{colnames(clade.table)[i+1] <- setnames[i]}
  }
  clade.table[is.na(clade.table)] <- 0
  d <- matrix(nrow=length(x), ncol=length(x))
  for(i in 1:length(x)){
    for(j in i+1:length(x)){
      if(j <= length(x)){d[i,j] <- mean(sqrt(2) * abs(clade.table[,i+1] - clade.table[,j+1]))}
    }
  }
  thissd <- apply(clade.table[,2:length(clade.table[1,])], 1, sd)
  thismean <- apply(clade.table[,2:length(clade.table[1,])], 1, mean) 
  clade.table$sd <- thissd
  clade.table$mean <- thismean
  clade.table <- clade.table[order(clade.table$sd, decreasing=TRUE),]
  output <- list("cladetable" = clade.table, "dist" = d)
  class(output) = "rwty.comparen"
  output
}

