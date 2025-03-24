#' Tree distance matrix calculation
#' 
#' This function takes a list of trees and returns a distance matrix
#' populated with distances between all trees in the list.
#' 
#' @section Recommended methods:
#' 
#' A suite of distance metrics are implemented, offering a trade-off between
#' running time and suitability of metric.  Ranked according to their running
#' time with 251 85-tip trees on a low-spec desktop computer, recommended
#' distance metrics are:
#' 
#' - \code{rf} (0.4 seconds): Robinson-Foulds distance (Robinson & Foulds, 
#' 1981): although widely used, the RF metric has a series of theoretical
#' shortcomings that give rise to bias and artefacts, translating to poor 
#' performance in a suite of practical applications. Its low resolution and 
#' rapid saturation make it particularly unsuitable for operations in tree
#' space.  Nevertheless, its speed is hard to match.
#' 
#' - \code{pd} (1 s): The path (= cladistic / nodal / patristic / tip)
#' distance (Farris 1973) can also be calculated rapidly, but is heavily
#' influenced by the shape (e.g. balanced / unbalanced) of trees, meaning that
#' similar-looking trees that nevertheless denote very different sets of 
#' relationships will have shorter distances than may be anticipated. 
#' Consequently, the path metric does a poor job of identifying clusters of
#' similar trees.
#' 
#' - \code{icrf} (1.4 s): Robinson-Foulds distance, corrected for split size
#' using information theory (Smith 2020).  This measure adjusts the 
#' Robinson-Foulds distance to account for the different significance of 
#' different partitions: partitions that evenly divide taxa contain more
#' information, and thus should contribute more to a distance score if they 
#' are not shared between trees.  This adjustment improves the resolution and
#' sensitivity of the metric, but does not correct for a number of arguably
#' more significant biases.
#' 
#' - \code{pid} (4 s); \code{msid} (8 s); \code{cid} (30 s): 
#' Phylogenetic information distance, matching split information
#' distance and clustering information distance (Smith 2020).
#' These information-theoretic methods belong to the class of 
#' Generalized Robinson-Foulds distances:
#' by recognizing similarities between non-identical splits, they overcome 
#' many of the artefacts that affect the RF distance, providing a more 
#' representative measure of tree distances; whereas their information-theoretic
#' basis affords them a natural unit (the bit), providing a measurable dimension
#' to tree space.
#' Whilst the CID performs the best against a suite of theoretical and 
#' practical criteria, the MSID comes a very close second and is somewhat
#' quicker to calculate. The PID will provide unexpectedly large distances
#' in a subset of the cases that distort the RF metric, which may result in
#' undesirable distortions of an accompanying tree space.
#' 
#' Detailed analysis of the behaviour of these and other tree distance methods
#' against a suite of criteria is available in Smith (2020); implementation 
#' details are provided in the R package 
#' '\href{https://ms609.github.io/TreeDist/index.html}{TreeDist}'.
#' 
#' @section Further methods:
#' 
#' A further set of methods that underperform methods with similar running
#' time listed above are also implemented for comparative purposes:
#' 
#' - \code{mast}, \code{masti} (30 minutes): size / information content of
#' the maximum agreement forest, subtracted from its maximum possible value
#' to create a distance. Specify `rooted = FALSE` if trees are unrooted.
#' 
#' - \code{jrf} (1 min, k = 2; 25 min, conflict-ok; 4 h, k = 4, no-conflict):
#'  Jaccard Robinson-Foulds metric (Böcker et al. 2013);
#'  specify a value of \code{k} and \code{allowConflict} using \code{\dots}.
#' 
#' - \code{ms} (5 s): Matching split distance (Bogdanowicz and Giaro 2012;
#' Lin et al. 2012.
#' 
#' - \code{nye} (65 s): The generalized RF distance of Nye et al. (2006).
#' 
#' - \code{nni} (65 s): Approximate Nearest Neighbour Interchance (rotation)
#'   distance.
#' 
#' - \code{spr} (0.4 s): Approximate Subtree Prune and Regraft distance.
#' 
#' @encoding UTF-8
#' @references 
#' Böcker S, Canzar S, Klau GW (2013). “The generalized Robinson-Foulds metric.” 
#' In Darling A, Stoye J (eds.), Algorithms in Bioinformatics. WABI 2013.
#' Lecture Notes in Computer Science, vol 8126, 156–169. Springer, Berlin,
#' Heidelberg. doi: 10.1007/978-3-642-40453-5_13.
#' 
#' Bogdanowicz D, Giaro K (2012). “Matching split distance for unrooted binary
#' phylogenetic trees.” IEEE/ACM Transactions on Computational Biology and 
#' Bioinformatics, 9(1), 150–160. doi: 10.1109/TCBB.2011.48.
#' 
#' Farris JS (1973). “On comparing the shapes of taxonomic trees.”
#' Systematic Zoology, 22(1), 50–54. doi: 10.2307/2412378.
#' 
#' Lin Y, Rajan V, Moret BME (2012). “A metric for phylogenetic trees based on
#' matching.” IEEE/ACM Transactions on Computational Biology and Bioinformatics,
#' 4(9), 1014–1022. doi: 10.1109/TCBB.2011.157.
#' 
#' Nye TMW, Liò P, Gilks WR (2006). “A novel algorithm and web-based tool for
#' comparing two alternative phylogenetic trees.” Bioinformatics, 22(1),
#' 117--119. doi: 10.1093/bioinformatics/bti720.
#' 
#' Robinson DF, Foulds LR (1981). “Comparison of phylogenetic trees.” 
#' Mathematical Biosciences, 53(1-2), 131–147. 
#' doi: 10.1016/0025-5564(81)90043-2.
#' 
#' Smith MR (2020). “Information theoretic Generalized Robinson-Foulds metrics 
#' for comparing phylogenetic trees.” Bioinformatics, in production. 
#' doi: 10.1093/bioinformatics/btaa614.
#'
#' @param trees an object of class 'multiPhylo'.
#' 
#' @param treedist acronym of distance method to employ: one of \code{cid},
#' \code{icrf}, \code{jrf}, \code{mast}, \code{masti}, \code{ms}, \code{msid},
#' \code{nni},  \code{pd}, \code{pid}, \code{rf} (default), or \code{spr}.
#' See below for details.
#' 
#' @param ... additional parameters sent to distance functions.
#'
#' @return a matrix of distances between each pair of trees
#'
#' @keywords treespace tree-distance robinson-foulds
#'
#' @importFrom TreeDist ClusteringInfoDistance InfoRobinsonFoulds 
#' JaccardRobinsonFoulds MASTSize MASTInfo MatchingSplitInfoDistance NNIDist 
#' NyeSimilarity PathDist PhylogeneticInfoDistance SPRDist
#' @export tree.dist.matrix
#' @examples
#' \dontrun{
#' data(fungus)
#' tree.dist.matrix(fungus$Fungus.Run1$trees)
#' }

tree.dist.matrix <- function(trees, treedist = 'rf', ...){
    if (!inherits(trees, "multiPhylo"))
        stop("trees should be an object of class \"multiPhylo\"")

    
    distanceMethods <- c('cid', 'icrf', 'jrf', 'mast', 'masti', 'ms', 'msid',
                         'nni', 'nye', 'pd', 'pid', 'rf', 'spr')
    how <- pmatch(tolower(treedist), distanceMethods)
    if (is.na(how)) {
        stop("`treedist` must be one of: ",
             paste0(distanceMethods, collapse = ', '))
    }
    
    NyeDist <- function(...) NyeSimilarity(..., similarity = FALSE)
    
    JRF <- function(...) JaccardRobinsonFoulds(trees, ...)
    
    MAST <- function (...) MASTSize(trees, ...)
    MASTI <- function (...) MASTInfo(trees, ...)
    
    Distance <- switch(how, 
                       ClusteringInfoDistance,
                       InfoRobinsonFoulds,
                       JRF,
                       MAST,
                       MASTI,
                       MatchingSplitDistance,
                       MatchingSplitInfoDistance,
                       NNIDist,
                       NyeDist,
                       PathDist,
                       PhylogeneticInfoDistance,
                       RF.dist,
                       SPRDist)
    
    treenames <- seq_along(trees)
    
    # Return:
    structure(as.matrix(Distance(trees, ...)),
              dimnames = list(treenames, treenames))
}
