#' @title Effect size matrices
#' 
#' @description  Create effect size matrices for all type of variables
#' Effect sizes - Cramer's V, ETA coefficient, Pearson's r
#' 
#' @export
cramerEtaMatrix = function(data){
  mat = matrix(NA, nrow = ncol(data), ncol = ncol(data))
  colnames(mat) = colnames(data)
  rownames(mat) = colnames(data)
  for(i in 1:ncol(data)){
    for(j in 1:ncol(data)){
      if(is.numeric(data[,i])){
        if(is.numeric(data[,j])){
          mat[i,j] = cor(data[,i], data[,j])
        }else if(is.factor(data[,j])){
          mat[i,j] = ryouready::eta(data[,j],data[,i])
        }
      }else if(is.factor(data[,i])){
        if(is.numeric(data[,j])){
          mat[i,j] = ryouready::eta(data[,i],data[,j])
        }else if(is.factor(data[,j])){
          mat[i,j] = rcompanion::cramerV(data[,i],data[,j])
        }
      }
    }
  }
  return (mat)
}

#' @export
CramersMatrix = function(data){
  cols = 0
  colnames = c()
  for(i in 1:ncol(data)){
    if(is.factor(data[1,i])){
      cols = cols+1
      colnames[cols]=colnames(data)[i]
    }
  }
  mat = matrix(NA, nrow = cols, ncol = cols)
  colnames(mat) = colnames
  rownames(mat) = colnames
  
  for(i in colnames){
    for(j in colnames){      
      mat[i,j] = rcompanion::cramerV(data[,i],data[,j])
    }
  }
  return (mat)
}

#' @export
ETAMatrix = function(data){
  cols = 0
  rows = 0
  colnames = c()
  rownames = c()
  
  for(i in 1:ncol(data)){
    if(is.numeric(data[1,i])){
      cols = cols+1
      colnames[cols]=colnames(data)[i]
    }
  }
  for(i in 1:ncol(data)){
    if(is.factor(data[1,i])){
      rows = rows+1
      rownames[rows]=colnames(data)[i]
    }
  }
  mat = matrix(NA, nrow = rows, ncol = cols)
  colnames(mat) = colnames
  rownames(mat) = rownames
  
  for(i in rownames){
    for(j in colnames){      
      mat[i,j] = ryouready::eta(data[,i],data[,j])
    }
  }
  return (mat)
}


#' @export
pearsonMatrix = function(data){
  cols = 0
  colnames = c()
  for(i in 1:ncol(data)){
    if(is.numeric(data[1,i])){
      cols = cols+1
      colnames[cols]=colnames(data)[i]
    }
  }
  mat = matrix(NA, nrow = cols, ncol = cols)
  colnames(mat) = colnames
  rownames(mat) = colnames
  
  for(i in colnames){
    for(j in colnames){      
      mat[i,j] = cor(data[,i],data[,j])
    }
  }
  return (mat)
}

#' @export
effectSizeGraph = function(matrix, eta = F, ...){
  if(eta){
    plt = matrix %>% ggcorrplot::ggcorrplot(type="full", lab=TRUE, lab_size=3.3, ...) 
  }else{
    plt = matrix %>% ggcorrplot::ggcorrplot(show.diag = F, type="lower", lab=TRUE, lab_size=3.3, ...) 
  }
  
  return(plt)
}