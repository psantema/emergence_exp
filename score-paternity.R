

#TEST EFFECT OF TREATMENT SCORE ON EXTRA-PAIR SIRING SUCCESS       

#1ST ORDER NEIGHBOURS ONLY

#Run model        
    model = glm(epp ~ score1 + age,  data=d2[treatment==1], family = binomial)
    summary(model)

#Plot data
    model = glm(epp ~ score1,  data=d2[treatment==1], family = binomial)
    ilink <- family(model)$linkinv
    dd = data.table(score1=seq(0,max(d2$score1[d2$treatment==1])))
    dd = add_column(dd, fit=predict(model, newdata=dd, type='response'))
    dd <- bind_cols(dd, setNames(as_tibble(predict(model, dd, se.fit=TRUE)[1:2]), c('fit_link','se_link')))
    dd <- mutate(dd, fit=ilink(fit_link),
                 upper=ilink(fit_link + (2*se_link)),
                 lower=ilink(fit_link - (2*se_link)))
    p1 = ggplot() +
      geom_line(data=dd, aes(x=score1, y=fit)) +
      geom_ribbon(data=dd, aes(x=score1, ymin=lower, ymax=upper), alpha=0.25) +
      geom_point(data=d2[epp==1 & treatment==1], aes(y=1, x=score1), size=2.5, alpha=.5) +
      geom_point(data=d2[epp==0 & treatment==1], aes(y=0, x=score1), size=2.5, alpha=.5) +
      ylab("Proportion siring EPY") + xlab("1st order neighborhood") +
      labs(tag = "a)") +
      ylim(0,1) +
      theme_classic() +
      theme(text=element_text(family="arial", face = "plain"),
            axis.title=element_text(size=14),
            axis.text.y=element_text(size=12),
            axis.text.x=element_text(size=14),
            plot.tag = element_text(size=16))
    p1


#1ST AND 2ND ORDER NEIGHBOURS

#Run model        
    model = glm(epp ~ score2 + age,  data=d2[treatment==1], family = binomial)
    summary(model)

#Plot data
    model = glm(epp ~ score2,  data=d2[treatment==1], family = binomial)
    ilink <- family(model)$linkinv
    dd = data.table(score2=seq(0,max(d2$score2[d2$treatment==1])))
    dd = add_column(dd, fit=predict(model, newdata=dd, type='response'))
    dd <- bind_cols(dd, setNames(as_tibble(predict(model, dd, se.fit=TRUE)[1:2]), c('fit_link','se_link')))
    dd <- mutate(dd, fit=ilink(fit_link),
                 upper=ilink(fit_link + (2*se_link)),
                 lower=ilink(fit_link - (2*se_link)))
    p2 = ggplot() +
      geom_line(data=dd, aes(x=score2, y=fit)) +
      geom_ribbon(data=dd, aes(x=score2, ymin=lower, ymax=upper), alpha=0.25) +
      geom_point(data=d2[epp==1 & treatment==1], aes(y=1, x=score2), size=2.5, alpha=.5) +
      geom_point(data=d2[epp==0 & treatment==1], aes(y=0, x=score2), size=2.5, alpha=.5) +
      ylab("") + xlab("1st + 2nd order neighborhood") +
      labs(tag = "b)") +
      ylim(0,1) + 
      scale_x_continuous(limits=c(0,120), breaks =c(0,20,40,60,80,100,120)) +
      theme_classic() +
      theme(text=element_text(family="arial", face = "plain"),
            axis.title=element_text(size=14),
            axis.text.y=element_text(size=12),
            axis.text.x=element_text(size=14),
            plot.tag = element_text(size=16))
    p2


#Combine figures and save plot    
    p = grid.arrange(p1, p2, ncol=2, bottom=textGrob("Treatment score", gp=gpar(fontsize=16)))
    ggsave(p, filename="fig3.tiff", width = 10, height = 5)
    

