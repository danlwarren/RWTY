compare.two <- function(x, y, burnin){
    
    start <- burnin + 1
    
    print("Populating table...")
    
    table1 <- clade.freq(x, start = start, end  = length(x))
    table2 <- clade.freq(y, start = start, end  = length(y))
    
    clade.table <- merge(table1, table2, by = "cladenames", all = TRUE) 
    clade.table[is.na(clade.table)] <- 0
    
    colnames(clade.table)[2] <- deparse(substitute(x))
    colnames(clade.table)[3] <- deparse(substitute(y))
    
    thissd <- apply(clade.table[,2:length(clade.table[1,])], 1, sd)
    thismean <- apply(clade.table[,2:length(clade.table[1,])], 1, mean) 
    clade.table$sd <- thissd
    clade.table$mean <- thismean
    
    clade.table <- clade.table[order(clade.table$sd, decreasing=TRUE),]
    
    thisplot <- ggplot(data=clade.table[,2:3], aes(x=test1, y=test2)) + 
                theme_bw() +
                geom_point(shape = 19) + 
                geom_abline(mapping = NULL, data = NULL, 
                            stat = "abline", position = "identity", 
                            show_guide = FALSE, colour="grey") +
                theme(panel.grid.major = element_blank(), 
                      panel.grid.minor = element_blank(),
                      panel.border = element_blank(), 
                      axis.line = element_line(colour = "black"),
                      axis.text.x = element_text(size=12), 
                      axis.text.y = element_text(size=12),
                      axis.title.x = element_text(size=14), 
                      axis.title.y = element_text(size=14))
    output <- list("cladetable" = clade.table, "plot" = thisplot, "dist" =  mean(abs(clade.table[,2] - clade.table[,3])))
    class(output) <- "rwty.comparetwo"
    output
}