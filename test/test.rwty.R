library(testthat)
library(rwty)

testtrees <- load.trees("~/GitHub/RWTY/test/testchain.t", skip=0)

expect_equal(names(testtrees), c("trees", "ptable", "gens.per.tree"))
expect_equal(length(names(testtrees$trees)), 8)
expect_equal(names(testtrees$trees), c("tree_100", "tree_200", "tree_300", "tree_400",
                                       "tree_500", "tree_600", "tree_700", "tree_800"))

