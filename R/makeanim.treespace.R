#' Plot chains in treespace.
#' 
#' This function will take list of rwty.chains objects and produce animations of chains in treespace.
#'
#' @param chains A list of one or more rwty.chain objects
#' @param burnin The number of trees to omit as burnin. The default (NA) is to use the maximum burnin from all burnins calculated automatically when loading the chains. This can be overidden by providing any integer value.  
#' @param min.points The minimum number of points on each plot. The function will automatically choose the thinning value which gets you the smallest number of trees that is at least as much as this value. The default (200) is usually sufficient to get a good idea of what is happening in your chains. 
#' @param fill.color The name of the column from the log table that that you would like to use to colour the points in the plot. The default is to colour the points by LnL.
#' @param return TRUE/FALSE: whether to return nothing (FALSE, the default) or return a gganimate object and the width and height so that you can use `animate()` yourself (TRUE). 
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


makeanim.treespace <- function(chains, burnin = NA, min.points = 200,  fill.color = "LnL"){
  
  chains = check.chains(chains)
  
  # set burnin to the maximum from across all chains
  if(is.na(burnin)){ burnin = max(unlist(lapply(chains, function(x) x[['burnin']]))) }
  
  print(sprintf("Creating treespace animation"))
  
  # Pre - compute checks. Since the calculations can take a while...
  
  if(min.points < 20) {
    stop("You need at least twenty points to make a meaningful treespace plot")
  }
  
  # now go and get the x,y coordinates from the trees
  points = treespace(chains, min.points, burnin, fill.color)
  
  if(length(unique(points$x)) == 1 && length(unique(points$y)) == 1){
    stop("Only one tree in the posterior sample of trees, cannot create animation")
  } 
  
  points$generation = as.integer(points$generation)
  points$chain = as.factor(points$chain)
  p = points
  
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
  
  base = ggplot(q, aes_string(x='x', y='y', group = "chain")) + 
    geom_density2d(data = q2) + 
    geom_path(aes_string(colour = "chain"), size = 1.5) + 
    geom_point(shape = 21, size=7, colour = 'white', aes_string(fill = fill.color)) + 
    scale_fill_gradientn(colours = viridis(256)) +
    facet_wrap(~chain, nrow=round(sqrt(length(unique(q$chain))))) +
    theme(panel.background = element_blank(), axis.line = element_line(color='grey'), panel.spacing = unit(0.1, "lines")) +
    theme(axis.title.x = element_text(vjust = -.5), axis.title.y = element_text(vjust=1.5)) +
    guides(color=FALSE)
  
  space.animation = base +
    labs(title = 'Generation: {frame_time}', x = 'x', y = 'y') +
    transition_time(generation) +
    shadow_wake(wake_length = 50/n.frames, wrap=TRUE) # 50 generations of trees fading into the background
  
  
  # now we make the trace animation to go with it
  axis_label = sprintf("%s distance", chains[[1]]$tree.dist.metric)
  
  # cut the first generation off points data frame to match up with the treespace data frame (since it has two points per generation)
  points = subset(points, generation > min(points$generation))
  
  trace.animation = ggplot(points, aes(x=generation, y=topo.dist.mcc, group=chain, colour=chain)) + 
    geom_line() +
    geom_segment(aes(xend = max(generation), yend = topo.dist.mcc), linetype = 2) +
    geom_point(size = 2) + 
    geom_text(aes(x = max(generation), label = chain), hjust = 0) +
    transition_reveal(generation) + 
    coord_cartesian(clip = 'off') + 
    labs(title = 'Distance of sampled tree from MCC tree', y = axis_label) + 
    theme_minimal() + 
    theme(plot.margin = margin(5.5, 80, 5.5, 5.5)) +
    theme(legend.position="none")
  
  
  
  # figure out size
  # first get the dimensions
  n = wrap_dims(length(unique(points$chain)), nrow = round(sqrt(length(unique(points$chain)))))
  # 250 pixels per frame
  n = n * 250
  # + 50 for the title, 100 for the legend
  n[1] = n[1] + 50
  n[2] = n[2] + 100
  
  print("building animations (this may take a minute)...")
  space_gif = animate(space.animation, nframes = n.frames, width = n[2], height = n[1])
  trace_gif = animate(trace.animation, nframes = n.frames, width = n[2], height = 250)
  
  print("combining animations...")
  
  space_mgif <- image_read(space_gif)
  trace_mgif <- image_read(trace_gif)
  
  final_gif <- image_append(c(space_mgif[1], trace_mgif[1]), stack = TRUE)
  for(i in 2:n.frames){
    combined <- image_append(c(space_mgif[i], trace_mgif[i]), stack = TRUE)
    final_gif <- c(final_gif, combined)
  }
  
  # animate it
  return(final_gif)
}

