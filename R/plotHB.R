#' @title Plot Histogram and Box plot in the same graph
#'
#' @description  This function plots the histogram and the box plot for
#' quantitative variables in the same plot.
#'
#' @param var The variable that needs to be plotted. The variable needs to be 
#' a quantitative variable.
#'
#' @param title The title of the plot
#'
#' @param varname The variable name
#'
#' @param seed The seed for the color of the plot
#' 
#' @param histX Logical value to specify whether to print the X axis for the 
#' histogram
#'
#' @export
plotHB <- function(var, title = NA, varname = NA, seed = NA, histXT = F, verbT = T, titleT =T, legendT = T,
                   labXT = T, labYT = T) {
  
  if(!is.numeric(var)){
    stop(simpleError("Variable is not numeric."))
  }
  # Themes
  # Center Plot Title
  geomTheme <- theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  # Remove legend
  geomTheme2 <- theme(legend.title = element_blank(), legend.position = "none")
  # Remove X axis
  geomTheme3 <- theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )
  
  if (is.na(title)) title <- deparse(substitute(var))
  if (is.na(varname)) varname <- deparse(substitute(var))
  
  f <- factor(sample(c("a", "b", "c", "d", "e"), length(var), T))
  levels(f) <- c("a", "b", "c", "d", "e")
  
  if (is.na(seed)) seed <- runif(1, 1, 1000)
  
  data = data.frame(varname = var)
  
  plt1 <- ggplot(data, aes(x = "", y = get(colnames(data)[1]))) +
    geom_boxplot(fill = randomColor(seed), color = "black") +
    coord_flip() +
    theme_classic() +
    xlab("") +
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
    )
  if(length(var)>100){
    bins = 100
  }else{
    bins = length(var)
  }
  
  if(labXT){
    plt1 = plt1+ylab(title)
  }else{
    plt1 = plt1 + scale_y_continuous(name=element_blank())
  }
  
  plt2 <- ggplot(data, aes(x = get(colnames(data)[1]), fill = f)) +
    geom_histogram(bins = bins, color = randomColor(seed),
                   key_glyph = draw_key_blank)  +
    theme_classic() +
    xlab("") +
    geomTheme +
    scale_fill_manual(
      labels = c(
        (paste("Minimum :", round(min(var), 2))),
        (paste("Median :", round(median(var), 2))),
        (paste("Maximum :", round(max(var), 2))),
        (paste("Mean :", round(mean(var), 2))),
        (paste("Std. Dev. :", round(sd(var), 2)))
      ),
      values = c(replicate(5, randomColor(seed))),
      name = "Summary"
    ) +
    theme(
      legend.title.align = 0.75, legend.text = element_text(),
      legend.title = element_text(face = "bold")
    )
  
  if(histXT == F){
    plt2 = plt2 + geomTheme3
  }
  
  if(titleT){
    plt2 = plt2 + labs(title = title)
  }
  
  if(legendT == F){
    plt2 = plt2 + theme(legend.title = element_blank())
  }
  
  if(labYT){
    plt2 = plt2 + ylab("Frequency")
  }else{
    plt2 = plt2 + scale_y_continuous(name=element_blank())
  }
  
  if(verbT){
    cat('Random Seed: ',seed, '\n')
  }
  return (cowplot::plot_grid(plt2, plt1,
                             ncol = 1, rel_heights = c(2, 1),
                             align = "v", axis = "lr"
  ))
}
