
#TEST EFFECT OF LIGHT TREATMENT ON MALE EMERGENCE TIME

#Get data
    d1 = merge(treat[,.(box, ID, treatment, put_out, removed)], em, by=c('box','ID'))
    d1[, exclude := ifelse((date_>'2021-04-07' & date_<='2021-05-03' & date_>removed) |
                              (date_>'2021-04-07' & date_<='2021-05-03' & date_<put_out),1,0)  ]
    d1 = d1[exclude==0]
    d1[, treatment := as.factor(treatment)]
    d1 = merge(d1, age, by=c('ID'), all.x=TRUE)
    d1[, age := ifelse(ID=='B5A9457',1,age)]
      
    
#descriptive statistics
  #Number of individuals  
    length(unique(d1$ID[d1$date_>d1$put_out & d1$date_<=d1$removed]))
    length(unique(d1$ID[d1$date_>d1$put_out & d1$date_<=d1$removed & d1$treatment==1]))
    length(unique(d1$ID[d1$date_>d1$put_out & d1$date_<=d1$removed & d1$treatment==0]))
  #Number of nights treated  
    length((d1$ID[d1$date_>d1$put_out & d1$date_<=d1$removed]))
    length((d1$ID[d1$date_>d1$put_out & d1$date_<=d1$removed & d1$treatment==1]))
    length((d1$ID[d1$date_>d1$put_out & d1$date_<=d1$removed & d1$treatment==0]))
  #Emergence time summary control males  
    mean(d1$em_time[d1$date_>d1$put_out & d1$date_<=d1$removed & d1$treatment==0])
    sd(d1$em_time[d1$date_>d1$put_out & d1$date_<=d1$removed & d1$treatment==0])
    min(d1$em_time[d1$date_>d1$put_out & d1$date_<=d1$removed & d1$treatment==0])
    max(d1$em_time[d1$date_>d1$put_out & d1$date_<=d1$removed & d1$treatment==0])
  #Emergence time summary experimental males    
    mean(d1$em_time[d1$date_>d1$put_out & d1$date_<=d1$removed & d1$treatment==1])
    sd(d1$em_time[d1$date_>d1$put_out & d1$date_<=d1$removed & d1$treatment==1])
    min(d1$em_time[d1$date_>d1$put_out & d1$date_<=d1$removed & d1$treatment==1])
    max(d1$em_time[d1$date_>d1$put_out & d1$date_<=d1$removed & d1$treatment==1])
    

#Run models    
    m1 = lmer(em_time ~ treatment + (1|ID), data=d1[date_>=put_out & date_<removed])
    summary(m1)  
    
    m2 = lmer(em_time ~ treatment + (1|ID), data=d1[date_<put_out])
    summary(m2)  

    
#Get number of new nests with eggs for each day (for figure)
    foo = firstEgg[,.(nests=.N), by=date_ ]
    foo[, aprilDay := yday(date_)-90]
    ff = data.table(aprilDay=seq(3,33))
    ff = merge(ff, foo, by=c('aprilDay'), all.x=TRUE)
    ff[, nests := ifelse(is.na(nests),0,nests)]
  
    
#Plot data    
    ggplot() + 
      geom_boxplot(data=d1, aes(x = april_day, y=em_time, fill=treatment, group=interaction(treatment, april_day)), alpha=.25, outlier.shape = NA) +
      geom_point(data=d1, aes(x = april_day, y=em_time, fill=treatment), position=position_dodge(0.75), pch=21, alpha=.75) + 
      xlab("Date (1 = 1 April)") + ylab("Emergence from the roost (min. relative to sunrise)") +
      scale_y_continuous(limits = c(-70,0), breaks=seq(-70,0,5), labels=c('-70','','-60','','-50','','-40','','-30','','-20','','-10','','0')) +
      scale_x_continuous(limits = c(.60,33.40), breaks=c(1:33)) +
      annotate("text", size=3.5, x = ff$aprilDay, y = -1, label = ff$nests) +
      annotate("text", size=4, x = 0.9, y = 0, label = 'new') +
      annotate("text", size=4, x = 0.95, y = -2.5, label = 'nests:') +
      geom_vline(xintercept=7.5, lty=2) +
      scale_fill_manual(values=c("red","blue")) +
      scale_colour_manual(values=c("red","blue")) +
      theme_classic() +
      theme(legend.position = "none") +
      theme(axis.text.y=element_text(size=12),
            axis.text.x=element_text(size=10),
            axis.title=element_text(size=14),
            plot.tag=element_text(size=19)) 
    
    ggsave(filename="fig1.png", width = 9, height = 6)
    
