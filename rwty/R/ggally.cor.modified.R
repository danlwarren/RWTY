#' Correlation from the Scatter Plot
#'
#' Estimate correlation from the given data. 
#' Modified to include pairwise Average Standard Deviation of Split Frequencies (ASDSF)
#'
#' @param data data set using
#' @param mapping aesthetics being used
#' @param corAlignPercent right align position of numbers. Default is 60 percent across the horizontal
#' @param corMethod \code{method} suppied to cor function
#' @param corUse \code{use} supplied to cor function
#' @param ... other arguments being supplied to geom_text
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @importFrom stringr str_c
#' @keywords hplot
#' @examples
#' data(tips, package = "reshape2")
#' ggally_cor(tips, mapping = ggplot2::aes_string(x = "total_bill", y = "tip"))
#' ggally_cor(
#' tips,
#' mapping = ggplot2::aes_string(x = "total_bill", y = "tip", size = 15, colour = "red")
#' )
#' ggally_cor(
#' tips,
#' mapping = ggplot2::aes_string(x = "total_bill", y = "tip", color = "sex"),
#' size = 5
#' )
ggally_cor <- function(data, mapping, corAlignPercent = 0.6, corMethod = "pearson", corUse = "complete.obs",...){
  corMethod <- as.character(substitute(corMethod))
  corUse <- as.character(substitute(corUse))
  useOptions = c("all.obs", "complete.obs", "pairwise.complete.obs", "everything", "na.or.complete")
  corUse <- pmatch(corUse, useOptions)
  
  if (is.na(corUse)) {
    corUse <- useOptions[1]
  } else {
    corUse <- useOptions[corUse]
  }
  cor_fn <- function(x, y) {
    # also do ddply below if fn is altered
    cor(x,y, method = corMethod, use = corUse)
  }
 
  xCol <- as.character(mapping$x)
  yCol <- as.character(mapping$y)
  colorCol <- as.character(mapping$colour)
  if (corUse %in% c("complete.obs", "pairwise.complete.obs", "na.or.complete")) {
    if(length(colorCol) > 0) {
      if(colorCol %in% colnames(data)) {
        rows <- complete.cases(data[,c(xCol,yCol,colorCol)])
      } else {
        rows <- complete.cases(data[,c(xCol,yCol)])
      }
    } else {
      rows <- complete.cases(data[,c(xCol,yCol)])
    }
    if(any(!rows)) {
      total <- sum(!rows)
      if (total > 1) {
        warning("Removed ", total, " rows containing missing values")
      } else if (total == 1) {
        warning("Removing 1 row that contained a missing value")
      }
    }
    data <- data[rows, ]
  }
  xVal <- data[,xCol]
  yVal <- data[,yCol]
  if(length(names(mapping)) > 0){
    for(i in length(names(mapping)):1){
      # find the last value of the aes, such as cyl of as.factor(cyl)
      tmp_map_val <- as.character(mapping[names(mapping)[i]][[1]])
      if(tmp_map_val[length(tmp_map_val)] %in% colnames(data))
        mapping[names(mapping)[i]] <- NULL
      if(length(names(mapping)) < 1){
        mapping <- NULL
        break;
      }
    }
  }

  final_text <- ""
  if(length(colorCol) < 1)
    colorCol <- "ggally_NO_EXIST"
  # browser()
  if(colorCol != "ggally_NO_EXIST" && colorCol %in% colnames(data)) {
    cord <- ddply(data, c(colorCol), function(x) {
      cor_fn(x[, xCol], x[, yCol])
    }, .parallel = FALSE)
    colnames(cord)[2] <- "ggally_cor"
    # browser()
    cord$ggally_cor <- signif(as.numeric(cord$ggally_cor), 3)
    # put in correct order
    lev <- levels(data[[colorCol]])
    ord <- rep(-1, nrow(cord))
    for(i in 1:nrow(cord)) {
      for(j in seq_along(lev)){
        if(identical(as.character(cord[i, colorCol]), as.character(lev[j]))) {
          ord[i] <- j
        }
      }
    }

    cord <- cord[order(ord[ord >= 0]), ]
    cord$label <- str_c(cord[[colorCol]], ": ", cord$ggally_cor)
    # calculate variable ranges so the gridlines line up
    xmin <- min(xVal, na.rm = TRUE)
    xmax <- max(xVal, na.rm = TRUE)
    xrange <- c(xmin-.01*(xmax-xmin),xmax+.01*(xmax-xmin))
    ymin <- min(yVal, na.rm = TRUE)
    ymax <- max(yVal, na.rm = TRUE)
    yrange <- c(ymin-.01*(ymax-ymin),ymax+.01*(ymax-ymin))

    p <- ggally_text(
      label = str_c("Cor : ", signif(cor_fn(xVal,yVal),3)),
      mapping = mapping,
      xP = 0.5,
      yP = 0.9,
      xrange = xrange,
      yrange = yrange,
      color = "black",
      ...
    ) +
      #element_bw() +
      theme(legend.position = "none")
    xPos <- rep(corAlignPercent, nrow(cord)) * diff(xrange) + min(xrange, na.rm = TRUE)
    yPos <- seq(from = 0.9, to = 0.2, length.out = nrow(cord) + 1) * diff(yrange) + min(yrange, na.rm = TRUE)
    yPos <- yPos[-1]
    
    cordf <- data.frame(xPos = xPos, yPos = yPos, labelp = cord$label)
    cordf$labelp <- factor(cordf$labelp, levels = cordf$labelp)
    
    p <- p + geom_text(
      data = cordf,
      aes(
        x = xPos,
        y = yPos,
        label = labelp,
        color = labelp
      ),
      hjust = 1,
      ...
    )
    p$type <- "continuous"
    p$subType <- "cor"
    p
  } else {
    # calculate variable ranges so the gridlines line up
    xmin <- min(xVal, na.rm = TRUE)
    xmax <- max(xVal, na.rm = TRUE)
    xrange <- c(xmin-.01*(xmax-xmin),xmax+.01*(xmax-xmin))
    ymin <- min(yVal, na.rm = TRUE)
    ymax <- max(yVal, na.rm = TRUE)
    yrange <- c(ymin-.01*(ymax-ymin),ymax+.01*(ymax-ymin))
    
    asdsf.min <- get("asdsf.min", envir = globalenv())
    asdsf <- data.frame(cbind(xVal, yVal))
    asdsf <- asdsf[apply(asdsf, MARGIN = 1, function(x) any(x > asdsf.min)), ]
    asdsf <- transform(asdsf, SD=apply(asdsf,1, sd, na.rm = TRUE))
    
    p <- ggally_text(
      label = paste("Correlation:\n",signif(cor_fn(asdsf$xVal,asdsf$yVal),3),"\nASDSF:\n",signif(mean(asdsf$SD),3),sep="",collapse=""),
      #label = paste("Correlation:\n",signif(cor_fn(asdsf$xVal,asdsf$yVal),3),"\nDiscordance:\n",signif(mean(abs(asdsf$xVal-asdsf$yVal)),3),sep="",collapse=""), # report discordance instead of ASDSF
      mapping,
      xP=0.5,
      yP=0.5,
      xrange = xrange,
      yrange = yrange,
      ...
    ) +
      #element_bw() +
      theme(legend.position = "none",
            panel.grid.major = element_blank(),
            axis.ticks = element_blank(),
            panel.border = element_rect(linetype = "dashed", colour = "black", fill=NA))
    p$type <- "continuous"
    p$subType <- "cor"
    p
  }
}