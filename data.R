

#Load packages
    require(data.table)
    require(ggplot2)
    require(gridExtra)
    require(sdb)
    require(chron)
    require(suncalc)
    require(tibble)
    require(dplyr)
    require(gridExtra)
    require(lme4)
    require(lmerTest)
    require(grid)
    
    
#Set connection to local database    
    Sys.setenv(TZ='CEST')
    con= dbcon(user = 'psantema', pwd = 'psantema2405', host = 'scidb.mpio.orn.mpg.de')
    
    
#Get table with all IDs and transponder numbers
    transponders_adults = as.data.table(dbq(con, 'SELECT ID, transponder FROM BTatWESTERHOLZ.ADULTS 
                       	                                    WHERE transponder is not NULL'))
    transponders_field = as.data.table(dbq(con, 'SELECT ID, transponder FROM FIELD_2021_BTatWESTERHOLZ.ADULTS 
                       	                                    WHERE transponder is not NULL'))
    transponders_field[, transponder := ifelse(nchar(transponder)==16,transponder, paste(transponder, '0000008001', sep=''))]
    transponders_chicks = as.data.table(dbq(con, 'SELECT ID, transponder FROM BTatWESTERHOLZ.CHICKS 
                       	                                    WHERE transponder is not NULL'))
    transponders_new = data.table(read.csv(file = 'data/transponders_new.csv', sep = ",", stringsAsFactors = FALSE))
    transponders_new[, transponder := ifelse(transponder=='68800', '068800', transponder)]
    transponders_new[, transponder := paste(transponder, '0000008001', sep='')]
    transponders_new = transponders_new[,.(ID, transponder)]
    transponders = rbind(transponders_adults, transponders_field, transponders_chicks, transponders_new)
    transponders = as.data.table(apply(transponders, 2, toupper))
    transponders = unique(transponders)
    write.csv(transponders, file='data/transponders.csv')
    
    
#Get table with age of each ID    
    age = dbq(con, 'SELECT ID, min(dt) firstCap , 
				IF ( month(min(dt)) > 7, year(min(dt))+1, year(min(dt)) ) season, year(min(dt)) `year`  , min(age) ageFirstCap from 
			     (SELECT DISTINCT ID, 0 age, date_time dt from BTatWESTERHOLZ.CHICKS 
			      UNION SELECT DISTINCT ID, age, capture_date_time dt from BTatWESTERHOLZ.ADULTS
			     where ID is not null and ID not like "X%" ) x where dt > "0000-00-00" group by ID', enhance = TRUE)
    age = age[, .(ID, season, ageFirstCap)]
    age = age[, .(year_ = season: 2021), by = .(ID, ageFirstCap)]
    age[, k := 1:.N, ID]
    age[ageFirstCap == 0 & k > 2, age := 2]
    age[ageFirstCap == 0 & k ==2, age := 1]
    age[ ageFirstCap == 1 & k == 1 , age := 1]
    age[ ageFirstCap == 1 & k > 1 , age := 2]
    age[ ageFirstCap == 2  , age := 2]
    age = age[,.(ID, year_, age)]
    age = age[year_==2021 & !is.na(ID)]
    age[, year_ := NULL]
    write.csv(age, file='data/age.csv')

    
#Get sex of each ID  
    sex = as.data.table(dbq(con, 'SELECT ID, sex FROM BTatWESTERHOLZ.SEX'))
    write.csv(sex, file='data/sex.csv')
    
    
#Get table with all breeding males 
    males = data.table(read.csv(file = 'data/paternity2021.csv', sep = ",", stringsAsFactors = FALSE))
    males = unique(males[epy==0,.(ID=father)])
    males[, ID := toupper(ID)]
    write.csv(males, file='data/males.csv')


#Get males who sired epp    
    pat = data.table(read.csv(file = 'data/paternity2021.csv', sep = ",", stringsAsFactors = FALSE))
    pat = pat[epy==1,. (females=length(unique(mother)), young=.N, epp=max(epy)), by=father]
    names(pat)[names(pat)=="father"] <- "ID"
    pat[, ID := toupper(ID)]
    pat = pat[ID!=""]
    write.csv(pat, file='data/pat.csv')

    
#Get sunrise times     
    sunrise = data.table(read.csv(file = 'data/sunrise.csv', sep = ",", stringsAsFactors = FALSE))  

    
#Get datae of first egg of each box  
    firstEgg = as.data.table(dbq(con, 'SELECT box, Date(date_time) date_ FROM FIELD_2021_BTatWESTERHOLZ.NESTS
                                     WHERE eggs=1'))    
    firstEgg[, min := min(date_), by=c('box')]    
    firstEgg = firstEgg[min==date_]
    firstEgg[, min := NULL]
    write.csv(firstEgg, file='data/firstEgg.csv')
    
    
#Get emergence times
  #get all transponder readings for relevent period   
    em = dbq(con, 'SELECT site box, date(datetime_) date_, time(datetime_) time_, transponder FROM 
                                                BTatWESTERHOLZ.transponders WHERE 
                                                datetime_>= "2021-04-01" and datetime_<= "2021-05-04" ') 
    em = unique(em)
  #convert time to numeric  
    em[, time_ := chron(times=time_)]
    em[, time_ := chron(times=time_)]
    em[, time_ := as.numeric(time_)*24]
  #convert date to April day   
    em[, yday := yday(date_)]
    em[, april_day := yday-90]
  #add surrise times
    em = merge(em, sunrise[,.(sunrise,yday)], by='yday')
  #exclude recordings after sunrise  
    em = em[time_<sunrise]
  #convert time to minutes relative to sunrise      
    em[, em_time := (time_-sunrise)*60]
  #keep the last recording before sunrise (measure of emergence time)  
    em[, min_time := max(time_), by=c('box', 'date_', 'transponder')]
    em = em[time_==min_time]
    em[, min_time:=NULL]
  #add transponder  
    em = merge(em, transponders, by='transponder', all.x=TRUE, allow.cartesian=TRUE)
  #save table  
    write.csv(em, file='data/em.csv')
    
     
#Get treatment table
  #Load table  
    treat = data.table(read.csv(file = 'data/treat.csv', sep = ",", stringsAsFactors = FALSE))

    
#Get treatment summary for each individual    
  #add emergence time data  
    treat2 = merge(treat[,.(box, ID, treatment, put_out, removed)], em, by=c('box','ID'))
  #add age  
    treat2 = merge(treat2, age, by=c('ID'), all.x=TRUE)
    treat2[, age := ifelse(is.na(age),1,age)]    
    treat2[, age := as.factor(age)]
  #exclude data from before treatment started or after treatment stopped  
    treat2 = treat2[date_ <= removed & date_ > put_out  ]
  #make tretament factor  
    treat2[, treatment := as.factor(treatment)]
  #summarise data for each ID  
    treat2 = treat2[, .(em_time=mean(em_time), age=first(age), treatment=first(treatment), nights=.N), by=ID]
  #add treat2ment score
    score = data.table(read.csv(file = 'data/score.csv', sep = ",", stringsAsFactors = FALSE))  
    treat2 = merge(treat2, score, by=c('ID'), all.x=TRUE)
    write.csv(treat2, file='data/treat2.csv')
    
