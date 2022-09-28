#' @title Plot Data frame
#'
#' @description  Plot all variables in a data frame
#'
#' @export
plotAll = function(data, methodSc ='glm', labels= NA, seed = NA){
  ncol = ncol(data)
  
  if(is.na(labels)){
    labels = colnames(data)
  }
  
  if(is.na(seed)){
    seed = runif(1,0,1000)
  }
  
  set.seed(seed)
  seeds = runif(ncol, 0, 1000) 
  
  for(i in 1:ncol){
    col = data[,i]
    if(!(is.factor(col) || is.numeric(col))){
      stop(simpleError("One or more variables are not factor or numerical type."))
    }
  }
  
  plList = list()
  labelTheme = theme_classic()+
    theme(axis.line = element_blank())
  
  theme2 = theme(title = element_text(size= 20, face = "bold"))
  theme2 = calc_element('title',theme)
  
  pb = txtProgressBar(min = 0, max =ncol*ncol, initial = 0)
  stepi = 0
  for(i in 1:ncol){
    y = data[,i]
    yLabel = ggplot()+draw_label(label=labels[i], angle = 90,fontface = theme2$face,
                                 size = theme2$size)+
      labelTheme
    
    plList = append(plList, list(yLabel))
    for(j in 1:ncol){
      x = data[,j]
      
      
      if(i==j){
        if(is.numeric(x)){
          plt = plotHB(x, verb = F,legendT= F, titleT = F, labXT = F, labYT = F, seed = seeds[i])
        }else{
          plt = plotPie(x, verb = F,legend = F , titleT = F, textT = T, percentageT = T, seed = seeds[i])
        }
      }else if(is.numeric(x)){
        if(is.factor(y)){
          plt = plotBox(x,y, verb = F,titleT = F,labXT = F,labYT = F, seed = seeds[i]*seeds[j])+coord_flip()
        }else{
          plt = plotSc(x,y, verb = F,titleT = F , labXT = F, labYT = F, method = methodSc, seed = seeds[i]*seeds[j])
        }
      }else{
        if(is.factor(y)){
          plt = plotBar(x,y, verbT = F, legendT = F, numberT = T, textT = T, outT= F,
                        labXT = F, labYT = F, titleT =F, seed = seeds[i]*seeds[j])+coord_flip()
        }else{
          plt = plotBox(y,x, verb = F,titleT = F,labXT = F,labYT = F, seed = seeds[i]*seeds[j])
        }
      }
      plList = append(plList, list(plt))
      stepi = stepi+1
      setTxtProgressBar(pb,stepi)
    }
  }
  
  blank = ggplot()+labelTheme
  plList = append(plList, list(blank))
  
  for(k in 1:(ncol)){
    xlabel = ggplot()+draw_label(label=labels[k],fontface = theme2$face,
                                 size = theme2$size)+
      labelTheme
    
    plList = append(plList, list(xlabel))
  }
  
  close(pb)
  # cowplot::plot_grid(plList)
  # ggpubr::ggarrange(plotlist = plList)
  return(plList)
}

#' @export
savePlotList = function(list, filename, open = F, title=NA){
  labelTheme = theme_classic()+
    theme(axis.line = element_blank())
  
  theme2 = theme(title = element_text(size= 30, face = "bold"))
  theme2 = calc_element('title',theme)
  
  plot = cowplot::plot_grid(plotlist = list, rel_widths = c(1,rep(10,sqrt(length(list))-1)), 
                            rel_heights = c(rep(10,sqrt(length(list))-1),1))
  if(!is.na(title)){
    titlePlot = ggplot()+
      draw_label(label=title,
                 fontface = theme2$face,
                 size = theme2$size)+
      labelTheme
    plot = cowplot::plot_grid(titlePlot, plot, ncol = 1, rel_heights = c(1,sqrt(length(list))*5))
  }
  png(filename, units="in", width=sqrt(length(list))*5, height=sqrt(length(list))*5, res = 300)
  print(plot)
  dev.off()
  if(open){
    img = png::readPNG(filename)
    grid::grid.raster(img)
  }
  print('Completed')
}