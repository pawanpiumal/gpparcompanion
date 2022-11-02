#' @title Plot Data frame
#'
#' @description  Plot all variables in a data frame
#'
#' @export
plotAll = function(data, methodSc ='glm', labels= NA, seed = NA, font = 20, labelsT = F){
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
  
  theme2 = theme(title = element_text(size= font, face = "bold"))
  theme2 = calc_element('title',theme2)
  
  pb = txtProgressBar(min = 0, max =ncol*ncol, initial = 0)
  stepi = 0
  for(i in 1:ncol){
    y = data[,i]
    yname = colnames(data)[i]
    yLabel = ggplot()+cowplot::draw_label(label=labels[i], angle = 90,fontface = theme2$face,
                                 size = theme2$size)+
      labelTheme
    
    plList = append(plList, list(yLabel))
    for(j in 1:ncol){
      x = data[,j]
      xname = colnames(data)[j]
      
      if(i==j){
        if(is.numeric(x)){
          plt = plotHB(x, verb = F,legendT= F, titleT = labelsT, labXT = F, labYT = F, seed = seeds[i],
                      varname = xname)
        }else{
          plt = plotPie(x, verb = F,legend = F , titleT = labelsT, textT = T, percentageT = T, seed = seeds[i],
                        varname = xname)
        }
      }else if(is.numeric(x)){
        if(is.factor(y)){
          plt = plotBox(x,y, verb = F,titleT = F,labXT = labelsT,labYT = labelsT, seed = seeds[i]*seeds[j], varname = xname, resname = yname)+coord_flip()
        }else{
          plt = plotSc(x,y, verb = F,titleT = F , labXT = labelsT, labYT = labelsT, method = methodSc, seed = seeds[i]*seeds[j], p = xname, resName = yname)
        }
      }else{
        if(is.factor(y)){
          plt = plotBar(x,y, verbT = F, legendT = F, numberT = T, textT = T, outT= F,
                        labXT = labelsT, labYT = labelsT, titleT =F, seed = seeds[i]*seeds[j], varname = xname, resname = yname)+coord_flip()
        }else{
          plt = plotBox(y,x, verb = F,titleT = F,labXT = labelsT,labYT = labelsT, seed = seeds[i]*seeds[j], varname = yname, resname = xname)
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
    xlabel = ggplot()+cowplot::draw_label(label=labels[k],fontface = theme2$face,
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
savePlotList = function(list, filename, openT = F, title=NA, size = 77, side = "upper", font=40, 
                        borderT = T,returnT = F, res = 72){
  labelTheme = theme_classic()+
    theme(axis.line = element_blank())
  
  blank = ggplot()+labelTheme
  
  theme2 = theme(title = element_text(size= font, face = "bold"))
  theme2 = calc_element('title',theme2)
  
  if(!(side %in% c('upper','lower','both'))){
    stop(simpleError("Side should be upper, lower or both."))
  }
  
  mat = matrix(seq(1,length(list),1), nrow = sqrt(length(list)), ncol = sqrt(length(list)),byrow = T)
  if(side == 'upper'){
    for(i in 1:nrow(mat)){
      if(i==ncol(mat)) next
      for(j in 1:ncol(mat)){
        if(j==1) next
        if(j %in% seq(1,i)){
          mat[i,j] = 0
        }
      }
    }
  }else if(side == 'lower'){
    for(i in 1:nrow(mat)){
      if(i==ncol(mat)) next
      for(j in 1:ncol(mat)){
        if(j==1) next
        if(j %in% seq(i+2,ncol(mat)+1)){
          mat[i,j] = 0
        }
      }
    }
  }
  for(i in 1:length(c(t(mat)))){
    if(c(t(mat))[i]==0)  list[[i]] = blank
  }
  
  if(borderT){
    for(i in 1:length(list)){
      list[[i]] = list[[i]]+theme(plot.background = element_rect(color = "black"))
    }
  }
  
  print("Start")
  timer = Sys.time()
  plot = cowplot::plot_grid(plotlist = list, rel_widths = c(1,rep(10,sqrt(length(list))-1)), 
                            rel_heights = c(rep(10,sqrt(length(list))-1),1))
  if(!is.na(title)){
    titlePlot = ggplot()+
      cowplot::draw_label(label=title,
                 fontface = theme2$face,
                 size = theme2$size)+
      labelTheme
    plot = cowplot::plot_grid(titlePlot, plot, ncol = 1, rel_heights = c(1,sqrt(length(list))*5))
  }
  png(filename, units="in", width=size, height=size, res = res)
  
  print(plot)
  dev.off()
  if(openT){
    img = png::readPNG(filename)
    grid::grid.raster(img)
  }
  print('Completed')
  print(Sys.time()-timer)
  if(returnT){
    return(plot)
  }
}