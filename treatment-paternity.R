
#TEST EFFECT OF LIGHT TREATMENT ON EXTRA-PAIR SIRING SUCCESS

#Get data  
  #Get table with treated males and other breeding males  
    d2 = rbind(males[ID!='' & !(ID %in% treat2$ID),.(ID, treatment=2, score1=NA, score2=NA)], treat2[,.(ID, treatment, score1, score2)])
  #Add paternity data  
    d2 = merge(d2, pat[,.(ID, epp)], by=c('ID'), all.x=TRUE)
    d2[, epp := ifelse(is.na(epp),0,epp)]
  #Add age  
    d2 = merge(d2, age, by=c('ID'), all.x=TRUE)
  #Add age of new individuals based on field observations  
    d2[, age := ifelse(ID=="B4X8963" | ID=="B4X8969" | ID=="B4X8991" | ID=="B5A9457" | ID=="B5A9459" | ID=="B5A9503" | ID=="B5A9520" |ID=="B5A9524", 1, age)]
    d2[, age := ifelse(ID=="B5A9527", 2, age)]
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
      ylab("Proportion siring epp") + xlab("") +
      scale_x_discrete(labels = c('Control\nn = 15','Treatment\nn = 31', 'Untreated\nn = 92')) +
      #annotate("text", size=4, x=dd$treatment, y=dd$lower-.03, label=dd$N) +
      ylim(0,1) +
      theme_classic() +
      theme(text=element_text(family="arial", face = "plain"),
            axis.title=element_text(size=14),
            axis.text.y=element_text(size=12),
            axis.text.x=element_text(size=14))
    
    ggsave(filename="plot.png", width = 6, height = 5)
    
    
    
#REMOVE: NOT USED IN PAPER
    
#Plot data for age classes separately
    model1 = glm(epp ~ treatment ,  data=d2[age==1], family = binomial)
    ilink <- family(model1)$linkinv
    dd1 = data.table(treatment=as.factor(c(0,1,2)))
    dd1 = add_column(dd1, fit=predict(model1, newdata=dd1, type='response'))
    dd1 <- bind_cols(dd1, setNames(as_tibble(predict(model1, dd1, se.fit=TRUE)[1:2]), c('fit_link','se_link')))
    dd1 <- mutate(dd1, fit=ilink(fit_link),
                 upper=ilink(fit_link + (2*se_link)),
                 lower=ilink(fit_link - (2*se_link)))
    n1=as.data.table(d2[age==1, .N, by=treatment])
    dd1=merge(dd1,n1, by=c('treatment'))
    dd1[, age := 1]
    model2 = glm(epp ~ treatment ,  data=d2[age==2], family = binomial)
    ilink <- family(model2)$linkinv
    dd2 = data.table(treatment=as.factor(c(0,1,2)))
    dd2 = add_column(dd2, fit=predict(model2, newdata=dd2, type='response'))
    dd2 <- bind_cols(dd2, setNames(as_tibble(predict(model2, dd2, se.fit=TRUE)[1:2]), c('fit_link','se_link')))
    dd2 <- mutate(dd2, fit=ilink(fit_link),
                  upper=ilink(fit_link + (2*se_link)),
                  lower=ilink(fit_link - (2*se_link)))
    n2=as.data.table(d2[age==2, .N, by=treatment])
    dd2=merge(dd2,n2, by=c('treatment'))
    dd2[, age := 2]
    dd = rbind(dd1, dd2)
    dd[, age := as.factor(age)]
    ggplot() +
      geom_errorbar(data=dd, aes(x=treatment, ymin=upper, ymax=lower, group=age, colour=age), position=position_dodge(0.5), width=.5) +
      geom_point(data=dd, aes(x=treatment, y=fit, group=age, colour=age), position=position_dodge(0.5)) +
      ylab("Proportion siring epp") + xlab("") +
      scale_x_discrete(labels = c('Control','Treatment', 'Untreated')) +
      annotate("text", size=4, x=c(0.87,1.87,2.87), y=dd$lower[dd$age==1]-.03, label=dd$N[dd$age==1]) +
      annotate("text", size=4, x=c(1.13,2.13,3.13), y=dd$lower[dd$age==2]-.03, label=dd$N[dd$age==2]) +
      ylim(-.03,1) +
      theme_classic() +
      theme(text=element_text(family="arial", face = "plain"),
            axis.title=element_text(size=14),
            axis.text.y=element_text(size=12),
            axis.text.x=element_text(size=14))#Relation between strength of treatment and paternity within light-treated group       
      ggsave(filename="plot.png", width = 6, height = 5)
    
    
      
