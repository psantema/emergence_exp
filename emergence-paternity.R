

#TEST EFFECT OF EMERGENCE TIME ON EXTRA-PAIR SIRING SUCCESS   

#Get data
  #Get table with treated males   
    d3 = treat2[,.(ID, treatment, em_time, nights)]
  #Merge with paternity data  
    d3 = merge(d3, pat[,.(ID, epp)], by=c('ID'), all.x=TRUE)
    d3[, epp := ifelse(is.na(epp),0,epp)]
    d3[, treatment := factor(treatment)] 
    d3 = merge(d3, age, by=c('ID'), all.x=TRUE)
    d3[, age := ifelse(ID=='B5A9457',1,age)]

    
#Run models
    model1 = glm(epp ~ em_time + age,  data=d3[treatment==1], family = binomial)
    summary(model1)
    
    model2 = glm(epp ~ em_time + age,  data=d3[treatment==0], family = binomial)
    summary(model2)

    
#Plot data        
    model1 = glm(epp ~ em_time,  data=d3[treatment==1], family = binomial)
    summary(model1)
    ilink <- family(model1)$linkinv
    dd1 = data.table(em_time=c(min(d3$em_time[d3$treatment==1]):max(d3$em_time[d3$treatment==1])))
    dd1 = add_column(dd1, fit=predict(model1, newdata=dd1, type='response'))
    dd1 <- bind_cols(dd1, setNames(as_tibble(predict(model1, dd1, se.fit=TRUE)[1:2]), c('fit_link','se_link')))
    dd1 <- mutate(dd1, fit=ilink(fit_link),
                  upper=ilink(fit_link + (2*se_link)),
                  lower=ilink(fit_link - (2*se_link)))
    model2 = glm(epp ~ em_time,  data=d3[treatment==0], family = binomial)
    summary(model2)
    ilink <- family(model2)$linkinv
    dd2 = data.table(em_time=c(min(d3$em_time[d3$treatment==0]):max(d3$em_time[d3$treatment==0])))
    dd2 = add_column(dd2, fit=predict(model2, newdata=dd2, type='response'))
    dd2 <- bind_cols(dd2, setNames(as_tibble(predict(model2, dd2, se.fit=TRUE)[1:2]), c('fit_link','se_link')))
    dd2 <- mutate(dd2, fit=ilink(fit_link),
                  upper=ilink(fit_link + (2*se_link)),
                  lower=ilink(fit_link - (2*se_link)))
    
    ggplot() +
      geom_line(data=dd1, aes(x=em_time, y=fit)) +
      geom_ribbon(data=dd1, aes(x=em_time, ymin=lower, ymax=upper), fill="blue", alpha=0.25) +
      geom_point(data=d3[epp==1 & treatment==1], aes(y=1.005, x=em_time), pch = 21, size=2.5, fill="blue", alpha=.5) +
      geom_point(data=d3[epp==0 & treatment==1], aes(y=0.005, x=em_time), pch = 21, size=2.5, fill="blue", alpha=.5) +
      geom_line(data=dd2, aes(x=em_time, y=fit)) +
      geom_ribbon(data=dd2, aes(x=em_time, ymin=lower, ymax=upper), fill="red", alpha=0.25) +
      geom_point(data=d3[epp==1 & treatment==0], aes(y=.995, x=em_time), pch = 21, size=2.5, fill="red", alpha=.5) +
      geom_point(data=d3[epp==0 & treatment==0], aes(y=-0.005, x=em_time), pch = 21, size=2.5, fill="red", alpha=.5) +
      ylab("Proportion siring EPY") + xlab("Average emergence time (min. from sunrise)") +
      ylim(-0.01,1.01) + xlim(min(d3$em_time),max(d3$em_time)) +
      ggtitle("") +
      theme_classic() + 
      theme(axis.title=element_text(size=14),
            axis.text=element_text(size=12))
    
    ggsave(filename="plot.png", width = 9, height = 6)
