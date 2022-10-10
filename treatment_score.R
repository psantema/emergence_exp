
#CREATE SCORE FOR STRENGTH OF TREATMENT FOR EACH LIGHT-TREATED MALE 
#Determined as the total number of females in the neighbourhood (1st or 2nd order) that was fertile on the days the male was manipulated


#Get fertile dates (5 to 1 days before laying) for all boxes 
    ff1 = firstEgg[,.(box, date_=as.Date(date_)-1)]
    ff2 = firstEgg[,.(box, date_=as.Date(date_)-2)]
    ff3 = firstEgg[,.(box, date_=as.Date(date_)-3)]
    ff4 = firstEgg[,.(box, date_=as.Date(date_)-4)]
    ff5 = firstEgg[,.(box, date_=as.Date(date_)-5)]
    ff = rbind(ff1,ff2,ff3,ff4,ff5)
    ff[, fertile := 1]
    
    
#Get neighbourhood data  
    neighbour = data.table(read.csv(file = 'data/neighbour.csv', sep = ",", stringsAsFactors = FALSE))  
    
    
#Combine data
  #Get manipulated males
    mm = d1[treatment=='light' & date_>put_out & date_<=removed, .(ID, box, date_=as.Date(date_))]
  #Add neighbouring boxes for each males for each night
    mm = merge(mm, neighbour, by.x=c('box'), by.y=c('box1'))
  #Add whether or not female was fertile for each neighbouring box   
    mm = merge(mm, ff, by.x=c('box2','date_'), by.y=c('box','date_'), all.x=TRUE)
    mm[, fertile := ifelse(is.na(fertile),0,1)]
    
    
#Get summaries for each male  
  #Get summary socre for 1st order neighbourhood  
    score1 = mm[no<=1,.(score1 = sum(fertile)), by=c('ID')]
  #Get summary socre for 1st and 2nd order neighbourhood   
    score2 = mm[no<=2,.(score2 = sum(fertile)), by=c('ID')]
  #Combine summary scores
    scoreb = merge(score1, score2, by=c('ID'))

#save file
    write.csv(score, file='./data/score.csv', row.names=FALSE)  
       
    
    
    
    
   