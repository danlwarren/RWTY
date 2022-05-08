test_that("manual check on chains with two samples", {
    # making https://github.com/danlwarren/RWTY/issues/177 a unit test
    # two chains with two samples, no burnin, window size 1

    # split index corresponds to alphabetical ordering of leaf names
    # splits 123 and 145
    t1 <- read.tree(text = "(a:2,(b:1,c:1):1,(d:1,e:1):1);", keep.multi = TRUE)
    # splits 125 and 134
    t2 <- read.tree(text = "((d:1,c:1):1,(b:1,e:1):1,a:2);", keep.multi = TRUE)
    # splits 12 and 125
    t3 <- read.tree(text = "((d:1,c:1):1,(a:1,b:1):1,e:2);", keep.multi = TRUE)

    make_rwty_chain <- function(trees) {
        f <- list(trees = trees, ptable = NULL, gens.per.tree = 1)
        class(f) <- "rwty.chain"
        return(f)
    }
    c1 <- make_rwty_chain(c(t1, t2))
    c2 <- make_rwty_chain(c(t2, t3))
    chains <- list(c1, c2)

    # in window 1, splits are disjoint so each split has average frequency 0.5
    # and SD = sqrt((1 - 0.5)^2 + (0 - 0.5)^2) = sqrt(0.5) = ASDSF
    # in cumulative window 2, the frequencies f, averages g and SDs are
    #   123: f1 = 0.5, f2 = 0,   g = 0.25, SD = sqrt(2 * 0.25^2) = sqrt(1 / 8)
    #   145: f1 = 0.5, f2 = 0,   g = 0.25, SD = sqrt(2 * 0.25^2) = sqrt(1 / 8)
    #   125: f1 = 0.5, f2 = 1,   g = 0.75, SD = sqrt(2 * 0.25^2) = sqrt(1 / 8)
    #   134: f1 = 0.5, f2 = 0.5, g = 0.5,  SD = sqrt(2 * 0^2)    = 0
    #   12:  f1 = 0,   f2 = 0.5, g = 0.25, SD = sqrt(2 * 0.25^2) = sqrt(1 / 8)
    # so the ASDSF = [0 +  4  * sqrt(1 / 8)] / 5

    obs_asdsf <- makeplot.asdsf(chains, 0, 1)$asdsf.plot$data$ASDSF
    exp_asdsf <- c(sqrt(1 / 2), sqrt(1 / 8) * 4 / 5)
    expect_equal(obs_asdsf, obs_asdsf)
})
