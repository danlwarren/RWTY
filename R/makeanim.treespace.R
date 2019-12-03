#' Plot chains in treespace.
#' 
#' This function will take list of rwty.chains objects and produce animations of chains in treespace.
#'
#' @param chains A list of one or more rwty.chain objects
#' @param burnin The number of trees to omit as burnin. The default (NA) is to use the maximum burnin from all burnins calculated automatically when loading the chains. This can be overidden by providing any integer value.  
#' @param n.points The minimum number of points on each plot. The function will automatically choose the thinning value which gets you the smallest number of trees that is at least as much as this value. The default (200) is usually sufficient to get a good idea of what is happening in your chains. 
#' @param fill.color The name of the column from the log table that that you would like to use to colour the points in the plot. The default is to colour the points by LnL.
#'
#' @return A gganimate object which has one frame for each of the sampled generations. 
#'
#' @keywords plot, treespace, rwty, animation gif
#'
#' @export makeanim.treespace
#' @examples
#' \dontrun{
#' data(fungus)
#' 
#' # Make the animation (can take a few minutes)
#' a <- makeanim.treespace(salamanders[1,2])
#'
#' 
#' # Save the animation
#' # see: https://gganimate.com/reference/anim_save.html
#' # Note that the width and height of the animation can be fiddly to set, 
#' # so using this function as below will save the last animation, in which the with and height
#' # have (hopefully) been set sensibly by default.
#' # By default RWTY produces GIFs, but you can change this using the function below
#' # and using the information from the gganimate link above.
#' anim_save("~/Desktop/animation.gif")
#' }


makeanim.treespace <- function(chains, burnin = NA, n.points = 200,  fill.color = "LnL"){

    chains = check.chains(chains)
  
    # set burnin to the maximum from across all chains
    if(is.na(burnin)){ burnin = max(unlist(lapply(chains, function(x) x[['burnin']]))) }
  
    print(sprintf("Creating treespace animation"))

    # Pre - compute checks. Since the calculations can take a while...

    if(n.points < 20) {
      stop("You need at least twenty points to make a meaningful treespace plot")
    }
    
    # now go and get the x,y coordinates from the trees
    points = treespace(chains, n.points, burnin, fill.color)

    points$generation = as.integer(points$generation)
    points$chain = as.factor(points$chain)
    p = as_tibble(points)
    
    # for each chain, copy p then add it back to itself so you get 2 points per generation
    # each generation therefore contains the current and previous generation's trees
    # meaning you can plot them with a line between them
    c = unique(p$chain)
    q = p[-c(1:nrow(p)),]
    s = NA
    for(chain_name in c){

        dat = subset(p, chain == chain_name)
      
        # remove the first row of s and the last row of dat
        s = dat[-c(1),]  
        dat = dat[-c(nrow(dat)),]
      
        # add back the sample and generation columns, offset by one
        dat$sample = s$sample
        dat$generation = s$generation
      
        # add this back to q
        r = rbind(dat, s)
        q = rbind(q, r)
    }
    
    q = arrange(q, chain, generation)
    
    # duplicate q and remove the 'generation' column, to allow us to plot densities
    q2 = q
    q2 = q2[ , !(names(q2) %in% c("generation"))]

    # number of frames set so that it's one per generation
    n.frames = length(unique(q$generation))

    # make the base plot, which we animate later
    base = ggplot(q, aes(x=x, y=y, group = chain)) + 
        geom_density2d(data = q2, aes(x=x, y=y)) +
        geom_path(alpha=1, aes(colour = generation), size=1) + 
        scale_colour_gradient(low='red', high='yellow') +
        geom_point(shape = 21, size=5, colour = 'white', aes_string(fill = fill.color)) + 
        scale_fill_gradientn(colours = viridis(256)) +
        facet_wrap(~chain, nrow=round(sqrt(length(unique(q$chain)))))
    
    treespace.animation = base +
        labs(title = 'Generation: {frame_time}', x = 'x', y = 'y') +
        transition_time(generation) +
        shadow_wake(wake_length = 50/n.frames, wrap=TRUE) # 50 generations of trees fading into the background
    
    # figure out size
    # first get the dimensions
    n = wrap_dims(length(unique(ggplot_build(base)$data[[1]]$PANEL)))
    # 200 pixels per frame
    n = n * 200
    # + 50 for the title, 100 for the legend
    n[1] = n[1] + 50
    n[2] = n[2] + 100
    
    # animate it
    animate(treespace.animation, nframes = n.frames, width = n[2], height = n[1])
  
    #return(list("treespace.animation" = treespace.animation, "width" = n[2], "height" = n[1], "frames" = n.frames))
}