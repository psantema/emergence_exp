
#Load packages and connect to databse
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
    
    Sys.setenv(TZ='CEST')
    con= dbcon(user = 'psantema', pwd = 'psantema2405', host = 'scidb.mpio.orn.mpg.de')
    
    
#Get transponders table
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
    
    
#Get age table    
    age = dbq(con, 'SELECT ID, min(dt) firstCap , 
				IF ( month(min(dt)) > 7, year(min(dt))+1, year(min(dt)) ) season, year(min(dt)) `year`  , min(age) ageFirstCap from 
			     (SELECT DISTINCT ID, 0 age, date_time dt from BTatWESTERHOLZ.CHICKS 
			      UNION SELECT DISTINCT ID, age, capture_date_time dt from BTatWESTERHOLZ.ADULTS
			      UNION SELECT DISTINCT ID, age, date_time_caught dt from FIELD_BTatWESTERHOLZ.ADULTS 
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
   

#Get sex table    
    sex = as.data.table(dbq(con, 'SELECT ID, sex FROM BTatWESTERHOLZ.SEX'))
    sex_lab = data.table(read.csv(file = 'data/sex_lab.csv', sep = ",", stringsAsFactors = FALSE))
    sex = rbind(sex, sex_lab)
    sex = merge(transponders, sex, by = 'ID', all.x = TRUE, allow.cartesian = TRUE)
    sex = unique(sex)
    sex = sex[transponder!='AEB0000000008001']
    sex[, n := .N, by=transponder]
    sex = sex[n==1]
    sex[, n:=NULL]
    sex = sex[,.(ID, sex)]
    sex = unique(sex)
    
    
#Get table with all breeding males 
    males = data.table(read.csv(file = 'data/paternity2021.csv', sep = ",", stringsAsFactors = FALSE))
    males = unique(males[epy==0,.(ID=father)])
    males[, ID := toupper(ID)]


#Get treated males
    treat = data.table(read.csv(file = 'data/treatment.csv', sep = ",", stringsAsFactors = FALSE))
    treat[, treatment := ifelse(treatment=='light',1,0)]
    
    
#Get males who sired epp    
    pat = data.table(read.csv(file = 'data/paternity2021.csv', sep = ",", stringsAsFactors = FALSE))
    pat = pat[epy==1,. (females=length(unique(mother)), young=.N, epp=max(epy)), by=father]
    names(pat)[names(pat)=="father"] <- "ID"
    pat[, ID := toupper(ID)]
    pat = pat[ID!=""]
    

    
#Get sunrise times     
    sunrise = data.table(read.csv(file = 'data/sunrise.csv', sep = ",", stringsAsFactors = FALSE))  


#Get emergence times
    em = dbq(con, 'SELECT site box, date(datetime_) date_, time(datetime_) time_, transponder FROM 
                                                BTatWESTERHOLZ.transponders WHERE 
                                                datetime_>= "2021-04-01" and datetime_<= "2021-05-04" ') 
    em = unique(em)
    em[, time_ := chron(times=time_)]
    em[, time_ := chron(times=time_)]
    em[, time_ := as.numeric(time_)*24]
    em[, yday := yday(date_)]
    em[, april_day := yday-90]
    em = merge(em, sunrise[,.(sunrise,yday)], by='yday')
    em = em[time_<sunrise]
    em[, em_time := (time_-sunrise)*60]
    em[, min_time := max(time_), by=c('box', 'date_', 'transponder')]
    em = em[time_==min_time]
    em[, min_time:=NULL]
    em = merge(em, transponders, by='transponder', all.x=TRUE, allow.cartesian=TRUE)
   
     
#get treatment summary    
    treat2 = merge(treat[,.(box, ID, treatment, put_out, removed)], em, by=c('box','ID'))
    treat2 = merge(treat2, age, by=c('ID'), all.x=TRUE)
    treat2[, age := ifelse(is.na(age),1,age)]
    treat2[, exclude := ifelse(date_ > removed |
                               date_ <= put_out,1,0)  ]
    treat2 = treat2[exclude==0]
    treat2[, mean_em_time := mean(em_time), by=yday]
    treat2[, rel_em_time := em_time-mean_em_time]
    treat2[, treatment := as.factor(treatment)]
    treat2 = treat2[date_>=put_out & date_<removed, .(em_time=mean(em_time), rel_em_time=mean(rel_em_time), age=mean(age), treatment=first(treatment), nights=.N, fertile=sum(fertile)), by=ID]
  #add treatment score
    score = data.table(read.csv(file = 'data/score.csv', sep = ",", stringsAsFactors = FALSE))  
    treat2 = merge(treat2, score, by=c('ID'), all.x=TRUE)
    
    
