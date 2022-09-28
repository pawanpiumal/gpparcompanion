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
                   groupLevels = NA, seed = NA, legendT = T, numberT = T, type = "fill", verbT = T, textT = T, outT= F,
                   titleT = T, labXT= T , labYT = T){
  
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
  if(verbT){
    cat('Random Seed: ',seed, '\n')
  }
  
  if(any(is.na(group))){
    plt = ggplot(data, aes(fill=group1, y=Frequency ,x=group2)) + 
      geom_bar(position=type, stat="identity")+
      geomTheme+
      labs(title=title,fill=var)+
      theme(legend.title.align = 0.75,legend.text = element_text(),
            legend.title = element_text(face="bold"))+
      scale_fill_manual(values = randomColor(seed,length(levels(variable))),name="Group")
    
  }else{
    plt = ggplot(data, aes(fill=group1, y=Frequency ,x=group2)) + 
      geom_bar(position=type, stat="identity")+
      geomTheme+
      labs(title=title,fill=var)+
      theme(legend.title.align = 0.75,legend.text = element_text(),
            legend.title = element_text(face="bold"))+
      scale_fill_manual(values = randomColor(seed,length(levels(variable))),name="Group")+
      facet_wrap(~ group3, strip.position = "bottom") +
      theme(strip.placement = "outside")
  }
  
  Str = ""
  
  if(numberT){
    Str = "%number"
  }
  
  if(textT){
    Str = if(Str == "")
      Str = "%text"
    else
      Str = paste("%text",Str,sep="\n")
  }
  if(outT==F){
    if(type %in% c("dodge","dodge2") && Str!= ""){
      plt = plt+
        geom_text(position = position_dodge(width = 0.9),
                  vjust = 0.5,
                  size = 4,
                  label = stringr::str_replace_all(pattern="%number", 
                                                   replacement = paste(data$Frequency),
                                                   string = stringr::str_replace_all(pattern="%text",
                                                                                     replacement = paste(data$group1),
                                                                                     string = Str)))
      
    }else if(type == "fill" && Str!= ""){
      plt = plt+
        geom_text(position = position_fill(vjust = 0.5),
                  size = 4,
                  label = stringr::str_replace_all(pattern="%number", 
                                                   replacement = paste0(round(data2$Proportion,4)*100, "%"),
                                                   string = stringr::str_replace_all(pattern="%text",
                                                                                     replacement = paste(data$group1),
                                                                                     string = Str)))
      
    }else if(type == "stack" && Str!= ""){
      plt = plt+
        geom_text(position = position_stack(vjust = 0.5),
                  size = 4,
                  label = stringr::str_replace_all(pattern="%number", 
                                                   replacement = paste(data$Frequency),
                                                   string = stringr::str_replace_all(pattern="%text",
                                                                                     replacement = paste(data$group1),
                                                                                     string = Str)))
    }
  }else{
    if(type %in% c("dodge","dodge2") && Str!= ""){
      plt = plt+
        ggrepel::geom_label_repel(position = position_dodge(width = 0.9),
                                  # fill = alpha(rep(randomColor(seed,length(levels(variable))),length(levels(response))),0.5),
                                  vjust = 0.5,
                                  show.legend = F,
                                  size = 4,
                                  label = stringr::str_replace_all(pattern="%number", 
                                                                   replacement = paste(data$Frequency),
                                                                   string = stringr::str_replace_all(pattern="%text",
                                                                                                     replacement = paste(data$group1),
                                                                                                     string = Str)))
      
    }else if(type == "fill" && Str!= ""){
      plt = plt+
        ggrepel::geom_label_repel(position = position_fill(vjust = 0.5),
                                  size = 4,
                                  show.legend = F,
                                  label = stringr::str_replace_all(pattern="%number", 
                                                                   replacement = paste0(round(data2$Proportion,4)*100, "%"),
                                                                   string = stringr::str_replace_all(pattern="%text",
                                                                                                     replacement = paste(data$group1),
                                                                                                     string = Str)))
      
    }else if(type == "stack" && Str!= ""){
      plt = plt+
        ggrepel::geom_label_repel(position = position_stack(vjust = 0.5),
                                  size = 4,
                                  show.legend = F,
                                  label = stringr::str_replace_all(pattern="%number", 
                                                                   replacement = paste(data$Frequency),
                                                                   string = stringr::str_replace_all(pattern="%text",
                                                                                                     replacement = paste(data$group1),
                                                                                                     string = Str)))
    }
  }
  
  if(type == "fill"){
    plt = plt+ ylab("Proportion")
  }
  
  if(!legendT){
    plt = plt + geomTheme2
  }
  
  
  if(labXT){
    plt = plt + labs(x=varname)
  }else{
    plt = plt + theme(axis.title.x=element_blank())
  }
  
  if(!labYT){
    plt = plt + theme(axis.title.y=element_blank())
  }
  
  if(!titleT){
    plt = plt + theme(plot.title=element_blank())
  }
  
  
  return (plt)
}