plot_scatter <- function(data, scatter_x="", scatter_y="", sc_label="",
                         set_size_num=5, set_size_var="",
                         set_color_num="red", set_color_var="",
                         filter_name="", filter_name_value="",
                         filter_num_value=-100000){
  
  selection = c(scatter_x,scatter_y)
  
  renaming = c('scatter_x','scatter_y')
  
  total=2
  
  if(sc_label != ""){
    selection = append(selection, sc_label)
    renaming = append(renaming, 'sc_label')
    total = total+1
  }
  
  if(set_size_var != ""){
    selection = append(selection, set_size_var)
    renaming = append(renaming, 'set_size_var')
    total = total+1
  }
  
  if(set_color_var != ""){
    selection = append(selection, set_color_var)
    renaming = append(renaming, 'set_color_var')
    total = total+1
  }
  if(filter_name != ""){
    if(!(filter_name %in% selection)){
      selection = append(selection, filter_name)
      renaming = append(renaming, 'filter_name')
      total = total+1
    }
  }
  
  data = data %>%
    select(c(selection))
  
  print(names(data))
  
  for(i in 1:total){
    names(data)[i] = renaming[i]
  }
  
  if(filter_name != ""){
    if(filter_name_value != ""){
      data = data %>%
        filter(filter_name == filter_value)
    }
    
  }
  
  plot = data %>%
    ggplot(aes(x=scatter_x,y=scatter_y))
  
  if(set_size_var != "" && set_color_var != ""){
    plot = plot+
      geom_point(aes(size=set_size_var, color=set_color_var))
  }
  else if(set_size_var == "" && set_color_var != ""){
    plot = plot+
      geom_point(aes(color=set_color_var),size=set_size_num)
  }
  else if(set_size_var != "" && set_color_var == ""){
    plot = plot+
      geom_point(aes(size=set_size_var),color=set_color_num)
  }
  else if(set_size_var == "" && set_color_var == ""){
    plot = plot+
      geom_point(size=set_size_num,color=set_color_num)
  }
  
  if(sc_label != ""){
    plot =  plot + 
      geom_label_repel(aes(x=scatter_x,y=scatter_y, label=sc_label))
  }
  
  plot
}