# assumes a 3 column output

library(coda)

plot.results <- function(results){
  
  ## Get info ##
  
  mcmc.res <- as.mcmc(data$Number.Unique.Sister.Taxa)
  
  hpd.res <- HPDinterval(mcmc.res)[,2]
  
  dd <- data[data$Number.Unique.Sister.Taxa > hpd.res,]
  
  dd <-  dd[order(dd$Number.Unique.Sister.Taxa, decreasing=TRUE),]
  
  
  legend.text <- paste(dd[,1],":",dd[,2])
  
  
  title.text <- paste("Upper 95% Confidence limit:",hpd.res)
  
  ## Plot out ##
  l<-matrix(c(1,1,1,2,2), ncol=5, nrow=1)
  layout(l)
  
  # Plot 1
  
  plot(seq(1,nrow(data),1), data[,2],xlab="",pch=19,col="#0000FF80" ,ylab="Number of Unique Taxa", main=title.text)
  
  abline(h=hpd.res, lty=2, lwd=2, col="red")
  
  # Plot 2
  plot(1, type="n", axes=F, xlab="", ylab="")
  
  legend("left",legend.text,bty="n", cex=1.1)
  
}