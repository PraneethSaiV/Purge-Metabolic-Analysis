get_values_TimeOnly = function(dataf){
  n = rep(6,3)
  group = rep(1:3,n)
  
  for (i in 1:nrow(dataf)){
    day9 = as.numeric(as.vector(dataf[i,2:7]))
    day16 = as.numeric(as.vector(dataf[i,8:13]))
    day23 = as.numeric(as.vector(dataf[i,14:19]))
    
    tie = c(day9,day16,day23)
    
    anova_result = anova(lm(tie ~ group))
    dataf$p_anova[i] = anova_result$`Pr(>F)`
    dataf$FC_day16_by_day9[i] = mean(day16)/mean(day9)
    dataf$FC_day9_by_day23[i] = mean(day9)/mean(day23)
    dataf$FC_day16_by_day23[i] = mean(day16)/mean(day23)
    aov_object = aov(tie ~ as.factor(group))
    tukey_result = TukeyHSD(aov_object)
    dataf$Tukey_day16_day9[i] = tukey_result$`as.factor(group)`[10]
    dataf$Tukey_day23_day9[i] = tukey_result$`as.factor(group)`[11]
    dataf$Tukey_day23_day16[i] = tukey_result$`as.factor(group)`[12]
  }
  return(dataf)
}


get_values_SampleOnly = function(dataf){
  n = rep(3,6)
  group = rep(1:6,n)
  
  for (i in 1:nrow(dataf)){
    sample1 = as.numeric(as.vector(dataf[i,c(2,8,14)]))
    sample3 = as.numeric(as.vector(dataf[i,c(3,9,15)]))
    sample4 = as.numeric(as.vector(dataf[i,c(4,10,16)]))
    sample5 = as.numeric(as.vector(dataf[i,c(5,11,17)]))
    sample7 = as.numeric(as.vector(dataf[i,c(6,12,18)]))
    sample8 = as.numeric(as.vector(dataf[i,c(7,13,19)]))
    
    tie = c(sample1,sample3,sample4,sample5,sample7,sample8)
    
    anova_result = anova(lm(tie ~ group))
    dataf$p_anova[i] = anova_result$`Pr(>F)`[1]
    
    aov_object = aov(tie ~ as.factor(group))
    tukey_result = TukeyHSD(aov_object)
    
    for(j in 1:nrow(tukey_result$`as.factor(group)`)){
      dataf[i,paste0('Tukey_',rownames(tukey_result$`as.factor(group)`)[j])] = tukey_result$`as.factor(group)`[45+j]
    }
    
    #dataf$Tukey_2_1[i] = tukey_result$`as.factor(group)`[10]
    #dataf$Tukey_day23_day9[i] = tukey_result$`as.factor(group)`[11]
    #dataf$Tukey_day23_day16[i] = tukey_result$`as.factor(group)`[12]
  }
  return(dataf)
}


cplot = function(dataframe, name){         
  png(paste0(name,'.png'), width = 1080, height = 1080)
  chart.Correlation(data.frame(dataframe))
  dev.off()
}

pcaplots = function(dataframe, name, confidence){
  dataframe = subset(dataframe,dataframe$p_anova < confidence)
  reference = gsub('@.*$','',dataframe$Compound)
  print(reference)
  selection = names(dataframe)[2:16]
  dataframe = t(dataframe[,selection])
  Principle_Components = prcomp(dataframe, center = T, scale. = T)
  Principle_Components$rotation # This
  temp_df = data.frame(Principle_Components$rotation[,1:2])
  colnames(temp_df)[1] = 'Dimension_1'
  colnames(temp_df)[2] = 'Dimension_2'
  rownames(temp_df) = reference
  temp_df = temp_df[order(abs(temp_df$Dimension_1)),][1:10,]
  png(paste(name,'_dim1.png'), width = 1200, height = 900)
  m = ggplot(temp_df, aes(x = rownames(temp_df),y = Dimension_1, fill = abs(Dimension_1)))
  m = m + geom_bar(stat = 'identity') + labs(x = 'Dimension 1', y = 'Contribution', title = 'Dimension Contributions')
  print(m)
  dev.off()
  temp_df = data.frame(Principle_Components$rotation[,1:2])
  colnames(temp_df)[1] = 'Dimension_1'
  colnames(temp_df)[2] = 'Dimension_2'
  rownames(temp_df) = reference
  temp_df = temp_df[order(abs(temp_df$Dimension_2)),][1:10,]
  png(paste(name,'_dim2.png'), width = 1200, height = 900)
  m = ggplot(temp_df, aes(x = rownames(temp_df),y = Dimension_2, fill = abs(Dimension_2))) 
  m = m + geom_bar(stat = 'identity') + labs(x = 'Dimension 2', y = 'Contribution', title = 'Dimension Contributions')
  print(m)
  dev.off()
  png(paste(name,'_scree.png'), width = 900, height = 900)
  plot(Principle_Components, type = 'l')
  dev.off()
  sample = data.frame(c('DA','DA','DA','DA','DA','DW','DW','DW','DW','DW','WA','WA','WA','WA','WA'))
  names(sample) = 'samples'
  png(paste(name,'_pca.png'), width = 900, height = 900)
  g <- ggbiplot(Principle_Components, obs.scale =0.5, var.scale = confidence, groups = as.factor(sample$samples),
                ellipse = TRUE, varname.abbrev = T, var.axes = F)
  g <- g + scale_color_discrete(name = '')
  g <- g + theme(legend.direction = 'horizontal', 
                 legend.position = 'top')
  print(g)
  dev.off()
}