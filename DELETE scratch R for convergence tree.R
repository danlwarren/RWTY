library(rwty)
data(fungus)

freqtable <- cumulative.freq(fungus, burnin = 0, window.size = 20)

contree <- consensus(fungus$Fungus.Run1$trees, p = 0.5)

tree1 <- fungus$Fungus.Run1$trees[[1]]

strsplit(freqtable$Fungus.Run1$translation[184,3], ", ")



test <- subtrees(contree)
sapply(test, function(x) get.convergence.diagnostic(x$tip.label, freqtable$Fungus.run1, contree, "sd"))

get.convergence.diagnostic(test[[10]]$tip.label, freqtable$Fungus.Run1, contree, "sd")

sapply(test, function(x) print(x$tip.label))

# Matching a list of taxa to an element from the translation table
all(match(test3, unlist(strsplit(freqtable$Fungus.Run1$translation[184,3], ", "))))


#test4 <- rapply(freqtable$Fungus.Run1$translation, function(x), unlist(strsplit(x)))


# This function takes a vector of tip labels, a frequency table from cumulative.fre1,
# a tree, and a metric (wcsf or sd), and returns the value of that metric
# for that clade
get.convergence.diagnostic <- function(tip.labels, freqtable, tree, metric = "wcsf"){
  trans <- unlist(freqtable$translation)
  trans.ind <- which(sapply(trans[,"Tip names"], 
                            function(x) all(match(unlist(strsplit(x, ", ")), tip.labels) & 
                                              all(match(tip.labels, unlist(strsplit(x, ", ")))))))
  clade.num <- freqtable$translation[trans.ind, "Clade number"]
  
  if(identical(clade.num, character(0))){
    tip.labels <- tree$tip.label[!tree$tip.label %in% tip.labels] 
    trans.ind <- which(sapply(trans[,"Tip names"], 
                              function(x) all(match(unlist(strsplit(x, ", ")), tip.labels) & 
                                                all(match(tip.labels, unlist(strsplit(x, ", ")))))))
    clade.num <- freqtable$translation[trans.ind, "Clade number"]
  }
  
  unc <- freqtable$cumulative.table[clade.num, metric]
  return(unc)
}





# tree1.sub <- subtrees(tree1)
# 
test7 <- tree1$tip.label[!tree1$tip.label %in% tree1.sub[[83]]$tip.label]

test6 <- tree1.sub[[45]]$tip.label
get.unc(test6, freqtable$Fungus.Run1, contree, "sd")

sapply(subtrees(fungus$Fungus.Run1$trees[[50]]), function(x) get.unc(x$tip.label, freqtable$Fungus.Run1))



# 
# test4 <- unlist(freqtable$Fungus.Run1$translation)
# which(sapply(test4[,"Tip names"], 
#        function(x) all(match(unlist(strsplit(x, ", ")), test3) & 
#                          all(match(test3, unlist(strsplit(x, ", ")))))))
