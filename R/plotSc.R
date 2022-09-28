#' @title Plot Scatter plots
#' 
#' @description Plot scatter plots for 2 continuous variables with or without transformations
#' 
#' 
#' @export
plotSc = function(var, res, pName=NA, resName= NA ,seed=NA,title = NA, titleT = T, method = "auto", 
                  transX = NA, transY = NA, verbT = T, labXT = T, labYT = T, r2T = T){
  
  if(!is.numeric(var) || !is.numeric(res)){
    stop(simpleError("Var or res is/are not numeric variable/s"))
  }
  
  
  
  if (is.na(pName)) pName <- deparse(substitute(var))
  if (is.na(resName)) resName <- deparse(substitute(res))
  
  if (is.na(title)) title <- paste(pName," vs ",resName)
  
  
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
  
  if(verbT){
    cat('Random Seed: ',seed, '\n')
  }
  plt = ggplot(data,aes(x=funcX(var),y=funcY(res)))+
    geom_point(size=2,color=randomColor(seed))+
    geom_smooth(method=method, se=T, color = "black", size=1.4, formula = y~x)+
    labs(title=title, 
         y = yName,
         x = xName)+
    geomTheme
  
  if(!titleT){
    plt = plt + theme(plot.title = element_blank())
  }
  
  if(!labXT){
    plt = plt + scale_x_continuous(name = element_blank())
  }
  
  if(!labYT){
    plt = plt + scale_y_continuous(name = element_blank())
  }
  
  if(r2T){
    plt = plt +
      ggpmisc::stat_poly_eq(formula = y~x, 
                            aes(label = paste(..rr.label.., sep = "~~~")), 
                            parse = TRUE)
  }
  
  return (plt)
}
