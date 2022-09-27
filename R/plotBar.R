#' @title Plot bar charts
#'
#' @describeIn This function plots stacked bar charts for 2 categorical variables.
#'
#' @param var
#' 
#' @param res
#' 
#' @param group
#'
#' @export
plotBar = function(var, res, group = NA, varname = NA, title = NA, varLevels = NA, resLevels = NA,
                   groupLevels = NA, seed = NA, legend = T, number = T, type = "fill"){
  
  if(!is.factor(var) || !is.factor(res)){
    stop(simpleError("Var or res is/are not factor variable/s."))
  }
  
  if(!any(is.na(group)) && !is.factor(group)){
    stop(simpleError("Group variable is not a factor variable."))
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
  
  if(!all(is.na(groupLevels))){
    levels(group) = groupLevels
  }
  
  
  if(!any(is.na(group))){
    data = data.frame(table(variable,response, group))
    colnames(data) = c("group1",'group2','group3',"Frequency")
    data2 = data %>% select(group1, group2, group3,Frequency) %>% 
      group_by(group3, group2) %>% summarise(Proportion = Frequency/sum(Frequency))
  }else{
    data = data.frame(table(variable,response))
    colnames(data) = c("group1",'group2',"Frequency")
    data2 = data %>% select(group1, group2,Frequency) %>% 
      group_by(group2) %>% summarise(Proportion = Frequency/sum(Frequency))
  }
  
  cat('Random Seed: ',seed, '\n')
  if(any(is.na(group))){
    plt = ggplot(data, aes(fill=group1, y=Frequency ,x=group2)) + 
      geom_bar(position=type, stat="identity")+
      geomTheme+
      labs(title=title,fill=var)+
      theme(legend.title.align = 0.75,legend.text = element_text(),
            legend.title = element_text(face="bold"),
            axis.title.x=element_blank())+
      scale_fill_manual(values = randomColor(seed,length(levels(variable))),name="Group")
    
  }else{
    plt = ggplot(data, aes(fill=group1, y=Frequency ,x=group2)) + 
      geom_bar(position=type, stat="identity")+
      geomTheme+
      labs(title=title,fill=var)+
      theme(legend.title.align = 0.75,legend.text = element_text(),
            legend.title = element_text(face="bold"),
            axis.title.x=element_blank())+
      scale_fill_manual(values = randomColor(seed,length(levels(variable))),name="Group")+
      facet_wrap(~ group3, strip.position = "bottom") +
      theme(strip.placement = "outside")
  }

  if(type %in% c("dodge","dodge2") && number){
    plt = plt+
      geom_text(position = position_dodge(width = .9),    # move to center of bars
                vjust = -0.5,    # nudge above top of bar
                size = 4,label = (data$Frequency))
    
  }else if(type == "fill" && number){
    plt = plt+
      geom_text(position = position_fill(vjust = 0.5),
                size = 4,label = paste0(round(data2$Proportion,4)*100, "%"))
    
  }else if(type == "stack" && number){
    plt = plt+
      geom_text(position = position_stack(vjust = 0.5),
                size = 4,label = data$Frequency)
  }
  
  if(type == "fill"){
    plt = plt+ ylab("Proportion")
  }
  
  if(!legend){
    plt = plt + geomTheme2
  }
  
  return (plt)
}