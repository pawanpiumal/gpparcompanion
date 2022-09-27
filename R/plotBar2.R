#' @title Plot bar charts
#'
#' @describeIn This function plots stacked bar charts for 2 categorical variables.
#'
#' @param var
#' 
#' @param res
#'
#' @export
plotBar2 = function(var, res, varname = NA, title = NA, varLevels = NA, resLevels = NA,
                     seed = NA, legend = T, type = "fill"){
  
  if(!is.factor(var) || !is.factor(res)){
    stop(simpleError("Var or res is/are not factor variable/s."))
  }
  
  types = c("stack","fill","dodge",'dodge2')
  if(!(type %in% types)){
    stop(simpleError("Type in in available types."))
  }
  
  # Themes
  # Center Plot Title
  geomTheme = theme(plot.title = element_text(hjust = 0.5,face="bold"))
  # Remove legend
  geomTheme2 = theme(legend.title = element_blank(),legend.position ="none")
  # Remove X axis
  geomTheme3 = theme(axis.title.x=element_blank(),
                     axis.text.x=element_blank(),
                     axis.ticks.x=element_blank())
  
  
  if (is.na(title)) title <- deparse(substitute(var))
  if (is.na(varname)) varname <- deparse(substitute(var))
  
  if(is.na(seed)) seed = runif(1,1,1000)
  
  variable = var
  if(!all(is.na(varLevels))){
    levels(variable) = varLevels
  }
  
  response = res
  if(!all(is.na(resLevels))){
    levels(response) = resLevels
  }
  
  
  data = data.frame(variable,response)
  colnames(data) = c("group1",'group2')
  
  
  cat('Random Seed: ',seed, '\n')
  
  plt = ggplot(data, aes(fill=group1 ,x=group2)) + 
    geom_bar(position=type, stat="count")+
    geomTheme+
    labs(title=title,fill=var)+
    theme(legend.title.align = 0.75,legend.text = element_text(),
          legend.title = element_text(face="bold"),
          axis.title.x=element_blank())+
    scale_fill_manual(values = randomColor(seed,length(levels(variable))),name="Group")
  
  
  
  if(type == "fill"){
    plt = plt+ ylab("Proportion")
  }
  
  if(!legend){
    plt = plt + geomTheme2
  }
  
  return (plt)
}