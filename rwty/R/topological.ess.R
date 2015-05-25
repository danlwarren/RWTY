tree.ess <- function(tree.list){  
  
  i <- sample(1:length(tree.list), 1)

  distances <- data.frame(matrix(unlist(lapply(tree.list, RF.dist, tree.list[[i]])), nrow=length(tree.list), byrow=T))    
  
  ESS <- apply(distances, 2, effectiveSize)
  return(as.numeric(ESS))
}


tree.ess.multi <- function(tree.list, n=20){  
  
  data <- replicate(n, tree.ess(tree.list))
  
  return(list(median.ess = median(data), ci.upper = quantile(data, probs=c(0.975)), ci.lower = quantile(data, probs=c(0.025))))
  
}


topological.ess <- function(chains, burnin = 0, n = 50){
  
  chains = check.chains(chains)
  
  chain = chains[[1]]
  
  indices = seq(from = burnin + 1, to = length(chain$trees), by = 1)   
  
  trees = lapply(chains, function(x) x[['trees']][indices])

  raw.ess = lapply(trees, tree.ess.multi, n)
  
  final.ess = data.frame(matrix(unlist(top.ess), nrow = length(chains), byrow = T))
  
  names(final.ess) = names(raw.ess[[1]])
  
  final.ess$chain = names(chains)
  
  
}