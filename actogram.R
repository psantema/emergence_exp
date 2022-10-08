

    transponders[ID=='B4X1473']
    transponders[ID=='B4X8613']
    transponders[ID=='B4X8954']
    transponders[ID=='B4H0223']
    transponders[ID=='B4P9170']
    transponders[ID=='B4P9692']
    transponders[ID=='B4X1213']
    transponders[ID=='B4X1306']
    transponders[ID=='B4X8979']
    transponders[ID=='B5A9100']
    transponders[ID=='B5A9457']
    

    readings = dbq(con, 'SELECT site box, date(datetime_) date_, datetime_, transponder FROM BTatWESTERHOLZ.transponders 
                             WHERE datetime_>= "2021-03-01" and datetime_<= "2021-06-04" AND transponder="6730000000008001" ') 

#Create variables
    x = copy(readings)
    x$yday = x$date_
    x$date_ = as.Date(x$datetime_)
    x$time_ = strftime(x$datetime_, format="%H:%M:%S")
    x$time_ = chron(times=x$time_)
    x$time_ = as.numeric(x$time_)*24
    x[, box := as.factor(box)]
    setorder(x, datetime_)
    max(x$date_)

#Plot all        
    ggplot()  +
      geom_point(data=x, aes(x=time_, y=date_, colour=box), size = 2, pch = "|") + 
      scale_x_continuous(limits = c(4,22), breaks = seq(4,22,1), labels=c("04:00","","06:00","","08:00","","10:00","","12:00","","14:00","","16:00","","18:00","","20:00","","22:00")) +
      xlab("Time of day") + ylab("") +
      theme_classic() +
      theme(axis.title = element_text(size = 16),
            axis.text = element_text(size = 14)) 
    
