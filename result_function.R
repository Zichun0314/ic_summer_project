
### Generate full data frame of results ###########################

generatedf = function(rhos, fs, sigma, estimateVar=FALSE){
  list_cov = list()
  count = 1
  
  for(j in rhos){
    for (k in fs){
      
      ############################# Assign object  ###########################
      if(estimateVar){
        result_obj = paste0("cov_pj", count, '_est_sigma_', sigma)  ## e.g. cov_pj1_est_sigma_1
      }else{
        result_obj = paste0("cov_pj", count, '_fixed_sigma_', sigma)  ## e.g. cov_pj1_fixed_sigma_1
      }
      
      
      result = eval(parse(text = result_obj))
      list_cov = append(list_cov, result)
      count = count + 1
      
    }
  }
  
  list_cov_final=with(stack(list_cov), split(values, ind))   ## stack all combination of outputs into one as summary
  return(list_cov_final)
}


################
sigma_status = c("est", "fixed")



### Generate output function ###########################

generateResult = function(...){
  
  
  abs_beta = factor(c(0, 0.2, 0.5, 1))
  
  ## Combine results for all beta
  all_beta = function(input){
    df = data.frame(beta=factor(), length=numeric(), type=NULL)
    LENGTH = input$LENGTH_R
    beta0 = input$beta0_R
    
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
  df_R = rbindlist(df_list_R) %>% mutate("split" = "(U, V)")
  
  
  var_names = c(`0` = paste0("bgroup('|', beta[i], '|')", "== 0"),
                `0.2` = paste0("bgroup('|', beta[i], '|')", "== 0.2"),
                `0.5` = paste0("bgroup('|', beta[i], '|')", "== 0.5"),
                `1` = paste0("bgroup('|', beta[i], '|')", "== 1"))
  
  
  tmp = df_R %>% split(., .$beta) %>% map(function(x){
    ggplot(data = x , aes(x=type, y=length, fill=type))+
      geom_boxplot(position=position_dodge(0.7), linetype = "dashed")+
      stat_boxplot(aes(ymin = ..lower.., ymax = ..upper.., fill=type),outlier.shape = 1) +
      stat_boxplot(geom = "errorbar", aes(ymin = ..ymax..), width=0.2)+
      stat_boxplot(geom = "errorbar", aes(ymax = ..ymin..), width=0.2) +
      stat_summary(fun = "mean", geom = "point", shape=18, size=2, color="cornsilk2")+
      theme_bw()+
      facet_nested(. ~ beta + split, labeller = labeller(beta = as_labeller(var_names, label_parsed)), scales = "free_y")+
      scale_fill_discrete(labels = c(bquote(Estimated~sigma^2), bquote(Fixed~sigma^2)))+
      theme(strip.text.x = element_text(margin = margin(b = 0, t = 0), face =  "bold.italic"),
            axis.title = element_blank(),
            legend.position = "bottom",
            panel.spacing=unit(0.1,"lines"),
            panel.spacing.y = unit(0.9, "lines"),
            strip.background = element_blank(),
            legend.spacing.x = unit(1.0, 'cm'),
            axis.ticks.x=element_blank(), axis.text.x = element_blank(), axis.title.x=element_blank())+
      guides(fill=guide_legend(title="Type"))
  })
  out_p = wrap_plots(tmp, nrow = 1) + plot_layout(guides = "collect")
  
  
  
  ## For table #########################
  
  test = function(input){
    
    LENGTH = input$LENGTH_R
    beta0 = input$beta0_R
    coverage = input$COV_R_HD
    
    table = data.frame(beta=beta0, length=LENGTH, coverage=coverage)
    
    
    count <<- count + 1
    
    table = table %>% group_by(beta) %>% 
      summarise(!! "length" := format(round(median(length),2), nsmall=2),
                !! "coverage" := format(round(mean(coverage)*100,2), nsmall = 1))
    
    
    return(table)
  }
  
  
  count = 0
  table_R = lapply(...,test)
  table_R = Reduce(function(...) merge(..., by="beta"),table_R)
  
  header_names = c("",setNames(2, "Estimated $\\sigma^{2}$"),setNames(2, "Fixed $\\sigma^{2}$"))
  colNames = c("$|{\\beta}_i|$", rep(c("Length", "Coverage (%)"), 2))
  
  table_R_final = kbl(table_R,align = 'c', booktabs = T, escape = F, caption = "(U, V)", 
                      col.names = colNames) %>%
    add_header_above(header_names, bold = T) %>%
    # {if(!grep("normal", all_args)) add_header_above(header_names2, bold = T)} %>%
    column_spec(1, width="5em") %>%
    column_spec(2:5, width = "5em") %>%
    kable_styling(position = "center", latex_options = c("hold_position", "scale_down"), font_size = 12)
  
  return(list(out_p=out_p,  table_R_final = table_R_final))

}







