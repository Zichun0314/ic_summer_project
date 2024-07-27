
#Result

result_table = function(...){
  
  ## For Length table #########################
  
  table_len = function(input){
    
    LENGTH = input$LENGTH_R
    beta0 = input$beta0_R
    beta0[beta0>0 & beta0<0.1] = '0-0.1'
    
    table = data.frame(beta=beta0, length=LENGTH)
    count <<- count + 1
    
    Length_table = table %>% group_by(beta) %>% 
      summarise(!! "median_length" := format(round(median(length),2), nsmall=2),
                !! "sd_length" := format(round(sd(length),2), nsmall = 1))
    
    return(Length_table)
  }
  
  count = 0
  table_R = lapply(...,table_len)
  table_R = Reduce(function(...) merge(..., by="beta"),table_R)
  
  header_names = c("",setNames(2, "Estimated $\\sigma^{2}$"),setNames(2, "Fixed $\\sigma^{2}$"))
  colNames = c("$|{\\beta}_i|$", rep(c("median_Length","sd_length"), 2))
  
  table_R_final = kbl(table_R,align = 'c', booktabs = T, escape = F, caption = "Length of the Confidence Interval", 
                      col.names = colNames) %>%
    add_header_above(header_names, bold = T) %>%
    # {if(!grep("normal", all_args)) add_header_above(header_names2, bold = T)} %>%
    column_spec(1, width="5em") %>%
    column_spec(2:5, width = "5em") %>%
    kable_styling(position = "center", latex_options = c("hold_position", "scale_down"), font_size = 12)
  
  
  
  ## For coverage table #########################
  
  table_cov = function(input){
    
    beta0 = input$beta0_R
    coverage = input$COV_R_HD
    beta0[beta0>0 & beta0<0.1] = '0-0.1'
    
    table = data.frame(beta=beta0, coverage=coverage)
    count <<- count + 1
    
    Coverage_table = table %>% group_by(beta==0) %>% 
      summarise(!! "Mean coverage" := format(round(mean(coverage)*100,2), nsmall = 1))
    
    return(Coverage_table)
  }
  
  count = 0
  table_R2 = lapply(...,table_cov)
  table_R2 = Reduce(function(...) merge(..., by="beta == 0"),table_R2)
  
  header_names = c("",setNames(1, "Estimated $\\sigma^{2}$"),setNames(1, "Fixed $\\sigma^{2}$"))
  colNames = c("$|{\\beta}| == 0$", "Coverage (%)","Coverage (%)")
  
  table_R2_final = kbl(table_R2,align = 'c', booktabs = T, escape = F, caption = "Averaged Coverage probability of the Confidence Interval", 
                       col.names = colNames) %>%
    add_header_above(header_names, bold = T) %>%
    # {if(!grep("normal", all_args)) add_header_above(header_names2, bold = T)} %>%
    column_spec(1, width="5em") %>%
    column_spec(2:3, width = "5em") %>%
    kable_styling(position = "center", latex_options = c("hold_position", "scale_down"), font_size = 12)
  
  
  
  ## For FCR table ##############
  table_FCR = function(input){
    
  
    table = data.frame(median_FCR = median(input$FCR),sd_FCR = sd(input$FCR))
    count <<- count + 1
    
    return(table)
  }
  
  count = 0
  table_R3 = lapply(...,table_FCR)
  
  
  ## final return
  return(list(table1 = table_R_final,table2 = table_R2_final, table3 = table_R3))
  
}



#########
result_plot = function(...){
  
  sigma_status = c("est", "fixed")
  
  ## Combine results for all beta
  all_beta = function(input){
    df = data.frame(beta=factor(), length=numeric(), type=NULL)
    LENGTH = input$LENGTH_R
    beta0 = input$beta0_R
    beta0[beta0>0 & beta0<0.1] = "0-0.1"
    
    abs_beta = factor(unique(beta0))
    
    
    count <<- count + 1
    for(i in abs_beta){
      
      tmp_df = try({rbind.data.frame(df, data.frame(beta=i,length=LENGTH[which(beta0 == i)], type=sigma_status[count]))}, silent=TRUE)
      if('try-error' %in% class(tmp_df)){ next } else{df = tmp_df}
      
    }
    return(df)
  }
  
  ## For plotting #########################
  
  count = 0
  df_list_R = lapply(..., all_beta)
  df_R = rbindlist(df_list_R) 
  
  unique_beta0 = unique(df_R$beta)
  var_names <- paste0("bgroup('|', beta[i], '|') == ", unique_beta0)
  names(var_names) <- unique_beta0
  
  tmp = df_R %>% split(.$beta)%>%map(function(x){
    ggplot(data = x , aes(x=type, y=length, fill=type))+
      geom_boxplot(position=position_dodge(0.7), linetype = "dashed")+
      stat_boxplot(aes(ymin = ..lower.., ymax = ..upper.., fill=type),outlier.shape = 1) +
      stat_boxplot(geom = "errorbar", aes(ymin = ..ymax..), width=0.2)+
      stat_boxplot(geom = "errorbar", aes(ymax = ..ymin..), width=0.2) +
      stat_summary(fun = "mean", geom = "point", shape=18, size=2, color="cornsilk2")+
      theme_bw()+
      facet_nested(. ~ beta,labeller = labeller(beta = as_labeller(var_names, label_parsed)), scales = "free_y")+
      scale_fill_discrete(labels = c(bquote(Estimated~sigma^2), bquote(Fixed~sigma^2)))+
      theme(strip.text.x = element_text(margin = margin(b = 0, t = 0), face =  "bold.italic"),
            axis.title = element_blank(),
            legend.position = "bottom",
            panel.spacing=unit(0.1,"lines"),
            panel.spacing.y = unit(0.9, "lines"),
            strip.background = element_blank(),
            legend.spacing.x = unit(1.0, 'cm'),
            axis.ticks.x=element_blank(), axis.text.x = element_blank(), axis.title.x=element_blank())+
      guides(fill=guide_legend(title="Type",ncol = 1))
  })
  
  out_p = wrap_plots(tmp, nrow = 1) + plot_layout(guides = "collect")+ theme(legend.position = "bottom")
  return(out_p)
}

