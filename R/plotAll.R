#' @title Plot Data frame
#'
#' @description  Plot all variables in a data frame
#'
#' @export
plotAll = function(data){
  ncol = ncol(data)
  
  for(i in 1:ncol){
    col = data[,i]
    if(!(is.factor(col) || is.numeric(col))){
      stop(simpleError("One or more variables are not factor or numerical type."))
    }
  }
  
  theme = theme(plot.title = element_blank(),
                legend.position = 'none')
  
  
  plList = list()
  
  pb = txtProgressBar(min = 0, max =ncol*ncol, initial = 0)
  stepi = 0
  for(i in 1:ncol){
    for(j in 1:ncol){
      x = data[,i]
      y = data[,j]
      
      
      if(identical(x,y)){
        if(is.numeric(x)){
          plt = plotHB(x, verb = F)+theme
        }else{
          plt = plotPie(x, verb = F)+theme
        }
      }
      
      if(is.numeric(x)){
        if(is.factor(y)){
          plt = plotBox(x,y, verb = F)+theme
        }else{
          plt = plotSc(x,y, verb = F)+theme
        }
      }else{
        if(is.factor(y)){
          plt = plotBar(x,y, verb = F)+theme
        }else{
          plt = plotBox(y,x, verb = F)+theme
        }
      }
      
      stepi = stepi+1
      plList[[stepi]] = plt
      setTxtProgressBar(pb,stepi)
    }
  }
  close(pb)
  # cowplot::plot_grid(plList)
  # ggpubr::ggarrange(plotlist = plList)
  return(plList)
}
