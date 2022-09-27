#' @title Plot box plots
#' 
#' @description Plot box plots for continuous vs categorical variables.
#' 
#' @param var
#' 
#' @param res
#' 
#' 
#' @export
plotBox = function(var, res, title = NA, varname = NA, resLevels= NA, seed = NA,
                   legend = F){
  # Themes
  # Center Plot Title
  geomTheme = theme(plot.title = element_text(hjust = 0.5,face="bold"))
  # Remove legend
  geomTheme2 = theme(legend.title = element_blank(),legend.position ="none")
  # Remove X axis
  geomTheme3 = theme(axis.title.x=element_blank(),
                     axis.text.x=element_blank(),
                     axis.ticks.x=element_blank())
  
  
  if(!is.numeric(var) || !is.factor(res)){
    stop(simpleError("Var or res is/are not numeric and factor variable/s"))
  }
  
  
  if (is.na(title)) title <- deparse(substitute(var))
  if (is.na(varname)) varname <- deparse(substitute(var))
  
  if(is.na(seed)) seed = runif(1,1,100000)
  
  response = res
  if(!any(is.na(resLevels))){
    levels(response) = resLevels
  }
  
  data = data.frame(response,var)

  cat('Random Seed: ',seed, '\n')
  plt = ggplot(data, aes(x=response, y=var, fill=response)) + 
    geom_boxplot() +
    xlab(res) +
    theme(legend.position="none") +
    theme(legend.title.align = 0.75,legend.text = element_text(),
          legend.title = element_text(face="bold"))+
    scale_fill_manual(values = randomColor(seed,1000))+
    theme(axis.title.y = element_blank())+
    labs(title=title)+
    geomTheme
  
  return (plt)
}