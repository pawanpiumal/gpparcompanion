#' Get random colors
#' 
#' This function gives random colors using the RGB function.
#' 
#' @param seed Seed for the random color. Use this to get the same color each time
#' @param count Number of colors that need to be returned. Defaults to 1.
#' @return A vector of randomly generated colors.
#' @export
randomColor = function(seed=NA,count=1){
  if(!is.na(seed)){
    set.seed(seed)
  }
  output = c()
  for(i in 1:count){
    output[i]=rgb(runif(1,0,1),runif(1,0,1),runif(1,0,1))
  }
  return(output)
}