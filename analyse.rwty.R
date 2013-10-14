# This function will hopefully be the primary user interface.  
# It will take one or more tree files, a few necessary arguments,
# and then automatically run all of the relevant analyses and 
# generate the plots...

analyse.rwty <- function(chains, burnin, window.size, gens.per.tree, step=1, ...){
    
    # If a single rwty.trees object is passed, it goes to the analyse.single
    # function.  Otherwise it assumes that multiple rwty.trees objects
    # have been passed as a list.
    if(class(chains) == "rwty.trees"){
        print("Analyzing single chain...")
        analyse.single(chains, burnin, window.size,
                       gens.per.tree, step, ...)
    }
    
    else{
        print("Analyzing multiple chains...")
        analyse.multi(chains, burnin, window.size, gens.per.tree, step, ...)
    }
}