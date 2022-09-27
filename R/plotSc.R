#' @title Plot Scatter plots
#' 
#' @description Plot scatter plots for 2 continuous variables with or without transformations
#' 
#' 
#' @export
plotSc = function(var, res, pName=NA, resName= NA ,seed=NA, title = T, method = "auto", 
                  transX = NA, transY = NA){
  
  if(!is.numeric(var) || !is.numeric(res)){
    stop(simpleError("Var or res is/are not numeric variable/s"))
  }
  
  if (is.na(title)) title <- deparse(substitute(var))
  
  if (is.na(pName)) pName <- deparse(substitute(var))
  if (is.na(resName)) resName <- deparse(substitute(res))
  
  if(is.na(seed)) seed = runif(1,1,1000)
  
  
  # Themes
  # Center Plot Title
  geomTheme = theme(plot.title = element_text(hjust = 0.5,face="bold"))
  # Remove legend
  geomTheme2 = theme(legend.title = element_blank(),legend.position ="none")
  # Remove X axis
  geomTheme3 = theme(axis.title.x=element_blank(),
                     axis.text.x=element_blank(),
                     axis.ticks.x=element_blank())
  
  none = function(x) return (x)
  
  if(is.na(transX)){
    transX = "none"
    xName = pName
  }else{
    xName = paste(str_to_title(transX), " transformation of ",pName)
  }
  
  if(is.na(transY)){
    transY = "none"
    yName = resName
  }else{
    yName = paste(str_to_title(transY), " transformation of ",resName)
  }
  
  funcX = get(transX)
  funcY = get(transY)
  
  data = data.frame(var=var,res=res)
  
  cat('Random Seed: ',seed, '\n')
  plt = ggplot(data,aes(x=funcX(var),y=funcY(res)))+
    geom_point(size=2,color=randomColor(seed))+
    geom_smooth(method=method, se=T, color = "black", size=1.4, formula = y~x)+
    labs(title=paste(pName," vs ",resName), 
         y = yName,
         x = xName)+
    geomTheme+
    ggpmisc::stat_poly_eq(formula = y~x, 
                 aes(label = paste(..rr.label.., sep = "~~~")), 
                 parse = TRUE)
  
  
  return (plt)
}
