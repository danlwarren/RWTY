
unique_sister_spp <- function(spp, multi.phy){

if((is.rooted(multi.phy[[1]])) == FALSE){
stop("Trees are unrooted, please root before proceeding")
}

require(phytools)
require(vegan)

if((length(spp)>1)==TRUE){
stop("Only a single species can be passed to the function 'unique_sister_spp'")
}

# Find sisters species for target species from all trees in `multi.phy'
o <-as.numeric(sapply(multi.phy, getSisters, spp))


# If o is less than or equal to the length of the number of tips, it is a terminal taxa
v <- o <= length(multi.phy[[1]]$tip.label)


# Select those values corresponding to tips
tips <- o[v==TRUE]


if(length(tips) > 0){

	phy.tips <- multi.phy[v==TRUE]

	tip.names <- mapply(get.tip.names, tips, phy.tips)

	unique.tip.names <- unique(tip.names)

	tip.length <- length(unique.tip.names)

	} else {

	tip.length <- 0

}


# Sister clades

node.locations <- o[v==FALSE]

if(length(node.locations) > 0){

	phy.sub <- multi.phy[v==FALSE]

	node.clades <- mapply(extract.clade, phy.sub, node.locations, SIMPLIFY=FALSE)

	# all.clades <- node.clades
	# num.unique.clades <- length(unique(all.clades))

	clade.tip.labels <- lapply(node.clades, get_clade_names)

	clade.tip.labels <- lapply(clade.tip.labels, sort)

	num.unique.clades <- length(unique(clade.tip.labels))

	} else {

	num.unique.clades <- 0

}

len <- tip.length + num.unique.clades




## Part 2 - Diversity of sister taxa ##

if(length(node.locations) > 0){

# Unique clades present

unique.clades <- unique(clade.tip.labels)

# Counts how many times each unique clade appears in all identified clades
clade.result <- vector()
for(i in 1:length(unique.clades)){
	clade <- unique.clades[[i]]
	check.matches <- sapply(clade.tip.labels, all.equal, clade)
	num.instances <- length(grep("TRUE",check.matches))
	clade.result[i] <- num.instances
}
} else {
clade.result <- "no.result"
}

if(length(tips) > 0){
# Number of times each tip name appears
tip.name.table <- table(tip.names)
tip.name.table.numeric <- as.numeric(tip.name.table)
} else {
tip.name.table.numeric <- "no.result"
}

# Final numeric vector of number of times each unique tip and clade appear


if(clade.result == "no.result" && tip.name.table.numeric != "no.result"){

all.counts <- tip.name.table.numeric

} else if(clade.result != "no.result" && tip.name.table.numeric == "no.result") {

all.counts <- clade.result

} else {

all.counts <- c(clade.result, tip.name.table.numeric)
}


div <- diversity(all.counts, index="simpson")

## Export results ##

output.result <- as.data.frame(t(c(len, div)))

colnames(output.result) <- c("Number.Unique.Sister.Taxa", "Diversity.of.Sister.Taxa")

return(output.result)

}

###

get.tip.names <- function(tip.numbers, phy){
names <- phy$tip.label[tip.numbers]
return(names)
}

###

get_clade_names <- function(clade){
names <- clade$tip.label
return(names)
}


