#' @title Plot pie chart
#'
#' @description  This function plots the pie plot for
#' qualitative variables.
#'
#' @param var The variable that needs to be plotted. The variable needs to be 
#' a qualitative variable.
#'
#' @param title The title of the plot
#'
#' @param varname The variable name
#' 
#' @param levels Vector of strings for the factor levels
#'
#' @param seed The seed for the color of the plot
#' 
#' @param text Print category name on plot
#' 
#' @param frequency Print frequency values on plot
#' 
#' @param percentage Print percentage values on plot
#' 
#' @param legend Print legend
#' 
#' @param out Show legend outside
#' 
#' @param groupOut Show the group name outside the chart
#'
#' @export
plotPie = function(var,title = NA,varname = NA, levels=NA, seed = NA, text = F, 
                   frequency = F, percentage = F, legend = T, out = F, groupOut = F){
  
  if(!is.factor(var)){
    stop(simpleError("Variable is not factor."))
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
  data = data.frame(table(variable))
  colnames(data) = c("group","freq")
  data = data %>% mutate(value = freq/sum(data$freq))
  
  if(!all(is.na(levels))){
    data$group = levels
  }
  
  # Compute the position of labels
  data <- data %>% 
    arrange(desc(group)) %>%
    mutate(prop = value / sum(data$value) *100) %>%
    mutate(ypos = cumsum(prop)- 0.5*prop )
  
  # Basic pie chart
  plt = ggplot(data, aes(x="", y=prop, fill=group)) +
    geom_bar(stat="identity", width=1, color="white") +
    coord_polar("y", start=0) +
    theme_void() + 
    #theme(legend.position="none") +
    scale_fill_manual(values = randomColor(seed,length(variable)))+
    labs(title=title)+
    geomTheme+
    theme(plot.title = element_text(face="bold",size=17))+
    theme(legend.title.align = 0.75,legend.text = element_text(),
          legend.title = element_text(face="bold"))+
    labs(fill="Group")
  
  cat('Random Seed: ',seed, '\n')
  
  Str = ""
  
  if(text){
    Str = "%text"
  }
  
  if(frequency){
    Str = if(Str!="") 
      paste(Str,"%frequency", sep="\n") 
    else 
      "%frequency"
  }
  
  if(percentage){
    Str = if(Str!="") 
      paste(Str,"%percentage%", sep="\n") 
    else 
      "%percentage"
  }
  

  if(Str != "" && out == F){
    plt = plt +
      geom_text(aes(y = ypos, 
                    # label = paste(group, freq, round(value,3)*100, sep="\n")),
                    label = stringr::str_replace_all(pattern = '%text',replacement = paste(group),
                                            string = stringr::str_replace_all(pattern = '%frequency', replacement = paste(freq),
                                                           string = stringr::str_replace_all(pattern = '%percentage',
                                                                                    replacement = paste(round(value,3)*100), 
                                                                                    string = Str)))),
                color = "black", 
                size=5)
  }else if(Str != "" && out == T){
    plt = plt +
      ggrepel::geom_label_repel(aes(y = ypos,
                    # label = paste(group, freq, round(value,3)*100, sep="\n")),
                    label = stringr::str_replace_all(pattern = '%text',replacement = paste(group),
                                            string = stringr::str_replace_all(pattern = '%frequency', replacement = paste(freq),
                                                                     string = stringr::str_replace_all(pattern = '%percentage',
                                                                                              replacement = paste(round(value,3)*100),
                                                                                              string = Str)))),
                color = "black",
                size=4.5,
                nudge_x = 1,
                show.legend = F)
      
  }
  
  if(groupOut){
    plt = plt+
      scale_y_continuous(breaks = data$ypos, labels = data$group)+
      theme(axis.text = element_text(size = 15))
  }
  
  if(!legend){
    plt = plt + geomTheme2
  }
  
  return(plt)
}