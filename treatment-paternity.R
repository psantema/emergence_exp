
#TEST EFFECT OF LIGHT TREATMENT ON EXTRA-PAIR SIRING SUCCESS

#Get data  
  #Get table with treated males and other breeding males  
    d2 = rbind(males[ID!='' & !(ID %in% treat2$ID),.(ID, treatment=2, score1=NA, score2=NA, days=NA, females1=NA, females2=NA)], treat2[,.(ID, treatment, score1, score2, days, females1, females2)])
  #Add paternity data  
    d2 = merge(d2, pat[,.(ID, epp)], by=c('ID'), all.x=TRUE)
    d2[, epp := ifelse(is.na(epp),0,epp)]
  #Add age  
    d2 = merge(d2, age, by=c('ID'), all.x=TRUE)
  #Add age of new individuals based on field observations  
    d2[, age := ifelse(ID=="B4X8963" | ID=="B4X8969" | ID=="B4X8991" | ID=="B5A9457" | ID=="B5A9459" | ID=="B5A9503" | ID=="B5A9520" |ID=="B5A9524", 1, age)]
    d2[, age := ifelse(ID=="B5A9527", 2, age)]
    d2[, age := as.factor(age)]
  #Set light-treatment as referecne level  
    d2[, treatment := relevel(treatment, ref = "1")]
  
    
#Descriptive statistics
  #Number of males that sired extra-pair young (excluding untreated males)  
    nrow(d2[treatment!=2 & epp>0])  
  #Proportion of males that sires extra-pair young in each treatment group  
    nrow(d2[treatment==0 & epp==1])/nrow(d2[treatment==0])
    nrow(d2[treatment==1 & epp==1])/nrow(d2[treatment==1])
    nrow(d2[treatment==2 & epp==1])/nrow(d2[treatment==2])
    

#Run model        
    model = glm(epp ~ treatment + age,  data=d2, family = binomial)
    summary(model)
    

#Plot data
    model = glm(epp ~ treatment ,  data=d2, family = binomial)
    ilink <- family(model)$linkinv
    dd = data.table(treatment=as.factor(c(0,1,2)))
    dd = add_column(dd, fit=predict(model, newdata=dd, type='response'))
    dd <- bind_cols(dd, setNames(as_tibble(predict(model, dd, se.fit=TRUE)[1:2]), c('fit_link','se_link')))
    dd <- mutate(dd, fit=ilink(fit_link),
                 upper=ilink(fit_link + (2*se_link)),
                 lower=ilink(fit_link - (2*se_link)))
    n=as.data.table(d2[, .N, by=treatment])
    dd=merge(dd,n, by=c('treatment'))
    ggplot() +
      geom_errorbar(data=dd, aes(x=treatment, ymin=upper, ymax=lower), width=.5) +
      geom_point(data=dd, aes(x=treatment, y=fit)) +
      ylab("Proportion siring EPY") + xlab("") +
      #scale_x_discrete(labels = c('Control','Treatment', 'Untreated')) +
      scale_x_discrete(labels = c('Control\nn = 15','Treatment\nn = 31', 'Untreated\nn = 92')) +
      #annotate("text", size=4, x=dd$treatment, y=dd$lower-.03, label=dd$N) +
      ylim(0,1) +
      theme_classic() +
      theme(text=element_text(family="arial", face = "plain"),
            axis.title=element_text(size=14),
            axis.text.y=element_text(size=12),
            axis.text.x=element_text(size=14))
    
    ggsave(filename="fig2.tiff", width = 6, height = 5)
    
    
    
