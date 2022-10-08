
#CREATE SCORE FOR LIGHT-TREATMENT MALES FOR STRENGTH OF TREATMENT
  #DETERMINED AS THE TOTLA NUMBER OF NEIGHBOURING FEMALES THAT WAS FERTIOLE ON THE DAYS THE MALE WAS MANUPILATED


#Get all fertile dates for all boxes  #Get first egg  
  #Get first egg of each box  
    firstEgg = as.data.table(dbq(con, 'SELECT box, Date(date_time) date_ FROM FIELD_2021_BTatWESTERHOLZ.NESTS
                                     WHERE eggs=1'))    
    firstEgg[, min := min(date_), by=c('box')]    
    firstEgg = firstEgg[min==date_]
    firstEgg[, min := NULL]
  #Get fertile dates (5 to 1 days before laying) for each box  
    ff1 = firstEgg[,.(box, date_=as.Date(date_)-1)]
    ff2 = firstEgg[,.(box, date_=as.Date(date_)-2)]
    ff3 = firstEgg[,.(box, date_=as.Date(date_)-3)]
    ff4 = firstEgg[,.(box, date_=as.Date(date_)-4)]
    ff5 = firstEgg[,.(box, date_=as.Date(date_)-5)]
    ff = rbind(ff1,ff2,ff3,ff4,ff5)
    ff[, fertile := 1]
    
    
#Get neighbours  
    neighbour = data.table(read.csv(file = 'data/neighbour.csv', sep = ",", stringsAsFactors = FALSE))  
    
    
#Get all neighbouring boxes for manipulated males
  #Get manipulated males
    mm = treat2[treatment==1, .(ID, box, date_=as.Date(date_))]
  #Get neighbouring boxes for each males for each night
    mm = merge(mm, neighbour, by.x=c('box'), by.y=c('box1'))
  #Add fertile females   
    mm = merge(mm, ff, by.x=c('box2','date_'), by.y=c('box','date_'), all.x=TRUE)
    mm[, fertile := ifelse(is.na(fertile),0,1)]
    
    
#Get summaries for each male    
    score1 = mm[no<=1,.(score1 = sum(fertile)), by=c('ID')]
    score2 = mm[no<=2,.(score2 = sum(fertile)), by=c('ID')]
    score = merge(score1, score2, by=c('ID'))

#save file
    write.csv(score, file='./data/score.csv', row.names=FALSE)  
       
    
    
    
    
   