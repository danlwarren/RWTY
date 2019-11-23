#' Tree distance matrix calculation
#' 
#' This function takes a list of trees and returns a distance matrix
#' populated with Robinson-Foulds tree distances between all trees
#' in the list.
#'
#' @param trees a multiPhylo object 
#' @param treedist the type of tree distance metric to use, can be 'PD' for path distance or 'RF' (the default) for Robinson Foulds distance
#'
#' @return RF A distance matrix of RF distances 
#'
#' @keywords treespace, tree distance, robinson-foulds
#'
#' @export tree.dist.matrix
#' @examples
#' \dontrun{
#' data(fungus)
#' tree.dist.matrix(fungus$Fungus.Run1$trees)
#' }

tree.dist.matrix <- function(trees, treedist='RF'){
    if (class(trees) != "multiPhylo") 
        stop("trees should be an object of class \"multiPhylo\"")

    
    # use the RF.dist function from Phangorn. Thanks to Klaus Vigo for pointing
    # out that we were doing this all wrong before!
    # https://github.com/danlwarren/RWTY/issues/47
    # we don't need to waste time checking labels because we check them later with check.chains
    if(treedist == 'PD'){
      DM = path.dist(tree1=trees, check.labels = FALSE)
    }else if(treedist == 'RF'){
      DM = RF.dist(tree1=trees, check.labels = FALSE)
    }else{
      stop("Unknown option for treedist. Valid options are 'PD' (for path distance) or 'RF' (for Robinson Foulds distance). Please try again")
    }
  

    return(DM)
}




        
