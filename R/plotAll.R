#' @title Plot Data frame
#'
#' @description  Plot all variables in a data frame
#'
#' @export
plotAll = function(data, methodSc ='glm'){
  ncol = ncol(data)
  
  for(i in 1:ncol){
    col = data[,i]
    if(!(is.factor(col) || is.numeric(col))){
      stop(simpleError("One or more variables are not factor or numerical type."))
    }
  }
  
  plList = list()
  
  pb = txtProgressBar(min = 0, max =ncol*ncol, initial = 0)
  stepi = 0
  for(i in 1:ncol){
    for(j in 1:ncol){
      x = data[,j]
      y = data[,i]
      
      
      if(i==j){
        if(is.numeric(x)){
          plt = plotHB(x, verb = F,legendT= F, titleT = F, labXT = F, labYT = F)
        }else{
          plt = plotPie(x, verb = F,legend = F , titleT = F, textT = T, percentageT = T)
        }
      }else if(is.numeric(x)){
        if(is.factor(y)){
          plt = plotBox(x,y, verb = F,titleT = F,labXT = F,labYT = F)
        }else{
          plt = plotSc(x,y, verb = F,titleT = F , labXT = F, labYT = F, method = methodSc)
        }
      }else{
        if(is.factor(y)){
          plt = plotBar(x,y, verbT = F, legendT = F, numberT = T, textT = T, outT= F,
                        labXT = F, labYT = F, titleT =F)
        }else{
          plt = plotBox(y,x, verb = F,titleT = F,labXT = F,labYT = F)
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
