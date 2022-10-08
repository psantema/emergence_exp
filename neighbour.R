




######## LOAD PACKAGES ########

require(sdb)               # data base access; installation: require(devtools); install_github( "mpio-be/sdb")
require(DBI)               # for baseline data base functions (disconnect)
require(data.table)        # easier data handling
require(spdep)             # spatial dependencies
require(expp)              # extra-pair paternity over space with dirichlet and neighbourhood functions
require(here)


######## LOAD DATA ########

    con= dbcon(user = 'psantema', pwd = 'psantema2405', host = 'scidb.mpio.orn.mpg.de')

#Get box locations for 2021
    locations = data.table(read.csv(file = 'StudyArea/new_locations.csv', sep = ",", stringsAsFactors = FALSE))
    
#get box coordinates
    coor = data.table(dbq(con,"SELECT * FROM BTatWESTERHOLZ.BOX_geoCoordinates order by box"))
    coor = coor[box %in% locations$new]
    
#get breeding boxes        
    breeding = data.table(read.csv(file = 'data/paternity2021.csv', sep = ",", stringsAsFactors = FALSE))
  #replace box with new location  
    
    breeding = merge(breeding, locations, by.x=c('box'), by.y=('old'))
    breeding[, box := NULL]
    setnames(breeding, "new", "box")
  #only keep one line per box  
    breeding = unique(breeding[epy==0,. (box, IDfemale=mother, IDmale=father, year_=2021)])
    breeding[, n := 1:.N, by=box]
    breeding = breeding[n==1]
    breeding[, n := NULL]
    



yourpath <- paste(here(),"/StudyArea/",sep="") # path where you save the folder with the spatial files
borders <- data.table(read.table(paste(yourpath,"AreaBorders.txt",sep="")))
setnames(borders,c("V1","V2"),c("x","y"))
studyarea <- SpatialPolygons(list(Polygons(list(Polygon(borders)),ID="border")))



######## COMPUTE DIRICHLET TERRITORIES AND NEIGHBOURS ########

brter <- copy(breeding)
brter[,IDmale:=as.character(as.vector(IDmale))]
brter[,IDfemale:=as.character(as.vector(IDfemale))]


yyy <- sort(unique(brter[,year_]))
DIRI <- as.list(rep(NA,length(yyy))) # produce empty list to store territory data for each year
names(DIRI) <- yyy
NBS <- as.list(rep(NA,length(yyy))) # produce empty list to store neighbour data for each year
names(NBS) <- yyy
# calculate territories and neighbourhoods for each year (one run of loop = one year)
for (i in yyy) {
  brbx <- merge(coor,brter[year_==i],by.x="box",by.y="box",all.x=FALSE,all.y=TRUE)
  temp  <- DirichletPolygons(SpatialPointsBreeding(data=brbx,coords=~x+y,breeding=~IDmale+IDfemale,id="box"),boundary=studyarea)
  DIRI[[which(names(DIRI)==i)]] <- temp
  temp <- data.table(neighborsDataFrame(poly2nb(temp)))  # from here onwards the loop calculates neighbours
  temp[,year_:=i]
  temp[,id:=as.integer(as.vector(id))]
  temp[,id_neigh:=as.integer(as.vector(id_neigh))]
  NBS[[which(names(NBS)==i)]] <- temp 
  rm(temp)
}; rm(i)
rm(yyy)
neigh <- rbindlist(NBS); rm(NBS)


######## FIND OUT IN WHICH TERRITORY BOX IS LOCATED ########

coor.spp <- SpatialPoints(coor[,.(x,y)])
yyy <- sort(unique(brter[,year_]))
TERRBOX <-  as.list(rep(NA,length(yyy))) # empty list to place results in
names(TERRBOX) <- yyy
for (i in yyy) { # one run of loop locates boxes in territories of one year 
  ontile <- as.data.table(over(x=coor.spp,y=DIRI[[as.character(i)]]))[,.(territory=ID)]
  ontile[,box:=unique(coor$box)]
  ontile[,year_:=i]
  setcolorder(ontile,c("year_","box","territory"))
  TERRBOX[[which(names(TERRBOX)==i)]] <- ontile
  rm(ontile)
}; rm(i)
terrbox <- rbindlist(TERRBOX)
#save file
write.csv(terrbox, file='./data/terrbox.csv', row.names=FALSE)


#### Calculate neighbourhood order
neigh1 = copy(neigh)
neigh1[, no := 1]

neigh2 <- merge(neigh[,.(year_,id,id_neigh)],neigh[,.(year_,id_neigh2=id,id_neigh)],by=c("year_","id_neigh"),allow.cartesian=TRUE)
neigh2 <- neigh2[id!=id_neigh2]
neigh2 <- neigh2[!paste(year_,id,id_neigh2) %in% neigh[,paste(year_,id,id_neigh)]]
neigh2[,id_neigh:=NULL]
neigh2 <- unique(neigh2)
neigh2[, no := 2]

neigh3 <- merge(neigh[,.(year_,id,id_neigh)],neigh2[,.(year_,id_neigh3=id,id_neigh=id_neigh2)],by=c("year_","id_neigh"),allow.cartesian=TRUE)
neigh3 <- neigh3[id!=id_neigh3]
neigh3 <- neigh3[!paste(year_,id,id_neigh3) %in% neigh[,paste(year_,id,id_neigh)] & 
                   !paste(year_,id,id_neigh3) %in% neigh2[,paste(year_,id,id_neigh2)]]
neigh3[,id_neigh:=NULL]
neigh3 <- unique(neigh3)
neigh3[, no := 3]

neigh4 <- merge(neigh[,.(year_,id,id_neigh)],neigh3[,.(year_,id_neigh4=id,id_neigh=id_neigh3)],by=c("year_","id_neigh"),allow.cartesian=TRUE)
neigh4 <- neigh4[id!=id_neigh4]
neigh4 <- neigh4[!paste(year_,id,id_neigh4) %in% neigh[,paste(year_,id,id_neigh)] & 
                   !paste(year_,id,id_neigh4) %in% neigh2[,paste(year_,id,id_neigh2)] & 
                   !paste(year_,id,id_neigh4) %in% neigh3[,paste(year_,id,id_neigh3)]]
neigh4[,id_neigh:=NULL]
neigh4 <- unique(neigh4)
neigh4[, no := 4]

neigh5 <- merge(neigh[,.(year_,id,id_neigh)],neigh4[,.(year_,id_neigh5=id,id_neigh=id_neigh4)],by=c("year_","id_neigh"),allow.cartesian=TRUE)
neigh5 <- neigh5[id!=id_neigh5]
neigh5 <- neigh5[!paste(year_,id,id_neigh5) %in% neigh[,paste(year_,id,id_neigh)] & 
                   !paste(year_,id,id_neigh5) %in% neigh2[,paste(year_,id,id_neigh2)] & 
                   !paste(year_,id,id_neigh5) %in% neigh3[,paste(year_,id,id_neigh3)] & 
                   !paste(year_,id,id_neigh5) %in% neigh4[,paste(year_,id,id_neigh4)]]
neigh5[,id_neigh:=NULL]
neigh5 <- unique(neigh5)
neigh5[, no := 5]

neigh6 <- merge(neigh[,.(year_,id,id_neigh)],neigh5[,.(year_,id_neigh6=id,id_neigh=id_neigh5)],by=c("year_","id_neigh"),allow.cartesian=TRUE)
neigh6 <- neigh6[id!=id_neigh6]
neigh6 <- neigh6[!paste(year_,id,id_neigh6) %in% neigh[,paste(year_,id,id_neigh)] & 
                   !paste(year_,id,id_neigh6) %in% neigh2[,paste(year_,id,id_neigh2)] & 
                   !paste(year_,id,id_neigh6) %in% neigh3[,paste(year_,id,id_neigh3)] & 
                   !paste(year_,id,id_neigh6) %in% neigh4[,paste(year_,id,id_neigh4)] & 
                   !paste(year_,id,id_neigh6) %in% neigh5[,paste(year_,id,id_neigh5)]]
neigh6[,id_neigh:=NULL]
neigh6 <- unique(neigh6)
neigh6[, no := 6]

neigh7 <- merge(neigh[,.(year_,id,id_neigh)],neigh6[,.(year_,id_neigh7=id,id_neigh=id_neigh6)],by=c("year_","id_neigh"),allow.cartesian=TRUE)
neigh7 <- neigh7[id!=id_neigh7]
neigh7 <- neigh7[!paste(year_,id,id_neigh7) %in% neigh[,paste(year_,id,id_neigh)] & 
                   !paste(year_,id,id_neigh7) %in% neigh2[,paste(year_,id,id_neigh2)] & 
                   !paste(year_,id,id_neigh7) %in% neigh3[,paste(year_,id,id_neigh3)] & 
                   !paste(year_,id,id_neigh7) %in% neigh4[,paste(year_,id,id_neigh4)] & 
                   !paste(year_,id,id_neigh7) %in% neigh5[,paste(year_,id,id_neigh5)] & 
                   !paste(year_,id,id_neigh7) %in% neigh6[,paste(year_,id,id_neigh6)]]
neigh7[,id_neigh:=NULL]
neigh7 <- unique(neigh7)
neigh7[, no := 7]

neigh8 <- merge(neigh[,.(year_,id,id_neigh)],neigh7[,.(year_,id_neigh8=id,id_neigh=id_neigh7)],by=c("year_","id_neigh"),allow.cartesian=TRUE)
neigh8 <- neigh8[id!=id_neigh8]
neigh8 <- neigh8[!paste(year_,id,id_neigh8) %in% neigh[,paste(year_,id,id_neigh)] & 
                   !paste(year_,id,id_neigh8) %in% neigh2[,paste(year_,id,id_neigh2)] & 
                   !paste(year_,id,id_neigh8) %in% neigh3[,paste(year_,id,id_neigh3)] & 
                   !paste(year_,id,id_neigh8) %in% neigh4[,paste(year_,id,id_neigh4)] & 
                   !paste(year_,id,id_neigh8) %in% neigh5[,paste(year_,id,id_neigh5)] & 
                   !paste(year_,id,id_neigh8) %in% neigh6[,paste(year_,id,id_neigh6)] & 
                   !paste(year_,id,id_neigh8) %in% neigh7[,paste(year_,id,id_neigh7)]]
neigh8[,id_neigh:=NULL]
neigh8 <- unique(neigh8)
neigh8[, no := 8]

neigh9 <- merge(neigh[,.(year_,id,id_neigh)],neigh8[,.(year_,id_neigh9=id,id_neigh=id_neigh8)],by=c("year_","id_neigh"),allow.cartesian=TRUE)
neigh9 <- neigh9[id!=id_neigh9]
neigh9 <- neigh9[!paste(year_,id,id_neigh9) %in% neigh[,paste(year_,id,id_neigh)] & 
                   !paste(year_,id,id_neigh9) %in% neigh2[,paste(year_,id,id_neigh2)] & 
                   !paste(year_,id,id_neigh9) %in% neigh3[,paste(year_,id,id_neigh3)] & 
                   !paste(year_,id,id_neigh9) %in% neigh4[,paste(year_,id,id_neigh4)] & 
                   !paste(year_,id,id_neigh9) %in% neigh5[,paste(year_,id,id_neigh5)] & 
                   !paste(year_,id,id_neigh9) %in% neigh6[,paste(year_,id,id_neigh6)] & 
                   !paste(year_,id,id_neigh9) %in% neigh7[,paste(year_,id,id_neigh7)] & 
                   !paste(year_,id,id_neigh9) %in% neigh8[,paste(year_,id,id_neigh8)]]
neigh9[,id_neigh:=NULL]
neigh9 <- unique(neigh9)
neigh9[, no := 9]

neigh10 <- merge(neigh[,.(year_,id,id_neigh)],neigh9[,.(year_,id_neigh10=id,id_neigh=id_neigh9)],by=c("year_","id_neigh"),allow.cartesian=TRUE)
neigh10 <- neigh10[id!=id_neigh10]
neigh10 <- neigh10[!paste(year_,id,id_neigh10) %in% neigh[,paste(year_,id,id_neigh)] & 
                     !paste(year_,id,id_neigh10) %in% neigh2[,paste(year_,id,id_neigh2)] & 
                     !paste(year_,id,id_neigh10) %in% neigh3[,paste(year_,id,id_neigh3)] & 
                     !paste(year_,id,id_neigh10) %in% neigh4[,paste(year_,id,id_neigh4)] & 
                     !paste(year_,id,id_neigh10) %in% neigh5[,paste(year_,id,id_neigh5)] & 
                     !paste(year_,id,id_neigh10) %in% neigh6[,paste(year_,id,id_neigh6)] & 
                     !paste(year_,id,id_neigh10) %in% neigh7[,paste(year_,id,id_neigh7)] & 
                     !paste(year_,id,id_neigh10) %in% neigh8[,paste(year_,id,id_neigh8)] & 
                     !paste(year_,id,id_neigh10) %in% neigh9[,paste(year_,id,id_neigh9)]]
neigh10[,id_neigh:=NULL]
neigh10 <- unique(neigh10)
neigh10[, no := 10]

neigh11 <- merge(neigh[,.(year_,id,id_neigh)],neigh10[,.(year_,id_neigh11=id,id_neigh=id_neigh10)],by=c("year_","id_neigh"),allow.cartesian=TRUE)
neigh11 <- neigh11[id!=id_neigh11]
neigh11 <- neigh11[!paste(year_,id,id_neigh11) %in% neigh[,paste(year_,id,id_neigh)] & 
                     !paste(year_,id,id_neigh11) %in% neigh2[,paste(year_,id,id_neigh2)] & 
                     !paste(year_,id,id_neigh11) %in% neigh3[,paste(year_,id,id_neigh3)] & 
                     !paste(year_,id,id_neigh11) %in% neigh4[,paste(year_,id,id_neigh4)] & 
                     !paste(year_,id,id_neigh11) %in% neigh5[,paste(year_,id,id_neigh5)] & 
                     !paste(year_,id,id_neigh11) %in% neigh6[,paste(year_,id,id_neigh6)] & 
                     !paste(year_,id,id_neigh11) %in% neigh7[,paste(year_,id,id_neigh7)] & 
                     !paste(year_,id,id_neigh11) %in% neigh8[,paste(year_,id,id_neigh8)] & 
                     !paste(year_,id,id_neigh11) %in% neigh9[,paste(year_,id,id_neigh9)] & 
                     !paste(year_,id,id_neigh11) %in% neigh10[,paste(year_,id,id_neigh10)]]
neigh11[,id_neigh:=NULL]
neigh11 <- unique(neigh11)
neigh11[, no := 11]

neigh12 <- merge(neigh[,.(year_,id,id_neigh)],neigh11[,.(year_,id_neigh12=id,id_neigh=id_neigh11)],by=c("year_","id_neigh"),allow.cartesian=TRUE)
neigh12 <- neigh12[id!=id_neigh12]
neigh12 <- neigh12[!paste(year_,id,id_neigh12) %in% neigh[,paste(year_,id,id_neigh)] & 
                     !paste(year_,id,id_neigh12) %in% neigh2[,paste(year_,id,id_neigh2)] & 
                     !paste(year_,id,id_neigh12) %in% neigh3[,paste(year_,id,id_neigh3)] & 
                     !paste(year_,id,id_neigh12) %in% neigh4[,paste(year_,id,id_neigh4)] & 
                     !paste(year_,id,id_neigh12) %in% neigh5[,paste(year_,id,id_neigh5)] & 
                     !paste(year_,id,id_neigh12) %in% neigh6[,paste(year_,id,id_neigh6)] & 
                     !paste(year_,id,id_neigh12) %in% neigh7[,paste(year_,id,id_neigh7)] & 
                     !paste(year_,id,id_neigh12) %in% neigh8[,paste(year_,id,id_neigh8)] & 
                     !paste(year_,id,id_neigh12) %in% neigh9[,paste(year_,id,id_neigh9)] & 
                     !paste(year_,id,id_neigh12) %in% neigh10[,paste(year_,id,id_neigh10)] & 
                     !paste(year_,id,id_neigh12) %in% neigh11[,paste(year_,id,id_neigh11)]]
neigh12[,id_neigh:=NULL]
neigh12 <- unique(neigh12)
neigh12[, no := 12]

neigh13 <- merge(neigh[,.(year_,id,id_neigh)],neigh12[,.(year_,id_neigh13=id,id_neigh=id_neigh12)],by=c("year_","id_neigh"),allow.cartesian=TRUE)
neigh13 <- neigh13[id!=id_neigh13]
neigh13 <- neigh13[!paste(year_,id,id_neigh13) %in% neigh[,paste(year_,id,id_neigh)] & 
                     !paste(year_,id,id_neigh13) %in% neigh2[,paste(year_,id,id_neigh2)] & 
                     !paste(year_,id,id_neigh13) %in% neigh3[,paste(year_,id,id_neigh3)] & 
                     !paste(year_,id,id_neigh13) %in% neigh4[,paste(year_,id,id_neigh4)] & 
                     !paste(year_,id,id_neigh13) %in% neigh5[,paste(year_,id,id_neigh5)] & 
                     !paste(year_,id,id_neigh13) %in% neigh6[,paste(year_,id,id_neigh6)] & 
                     !paste(year_,id,id_neigh13) %in% neigh7[,paste(year_,id,id_neigh7)] & 
                     !paste(year_,id,id_neigh13) %in% neigh8[,paste(year_,id,id_neigh8)] & 
                     !paste(year_,id,id_neigh13) %in% neigh9[,paste(year_,id,id_neigh9)] & 
                     !paste(year_,id,id_neigh13) %in% neigh10[,paste(year_,id,id_neigh10)] & 
                     !paste(year_,id,id_neigh13) %in% neigh11[,paste(year_,id,id_neigh11)] & 
                     !paste(year_,id,id_neigh13) %in% neigh12[,paste(year_,id,id_neigh12)]]
neigh13[,id_neigh:=NULL]
neigh13 <- unique(neigh13)
neigh13[, no := 13]

neigh14 <- merge(neigh[,.(year_,id,id_neigh)],neigh13[,.(year_,id_neigh14=id,id_neigh=id_neigh13)],by=c("year_","id_neigh"),allow.cartesian=TRUE)
neigh14 <- neigh14[id!=id_neigh14]
neigh14 <- neigh14[!paste(year_,id,id_neigh14) %in% neigh[,paste(year_,id,id_neigh)] & 
                     !paste(year_,id,id_neigh14) %in% neigh2[,paste(year_,id,id_neigh2)] & 
                     !paste(year_,id,id_neigh14) %in% neigh3[,paste(year_,id,id_neigh3)] & 
                     !paste(year_,id,id_neigh14) %in% neigh4[,paste(year_,id,id_neigh4)] & 
                     !paste(year_,id,id_neigh14) %in% neigh5[,paste(year_,id,id_neigh5)] & 
                     !paste(year_,id,id_neigh14) %in% neigh6[,paste(year_,id,id_neigh6)] & 
                     !paste(year_,id,id_neigh14) %in% neigh7[,paste(year_,id,id_neigh7)] & 
                     !paste(year_,id,id_neigh14) %in% neigh8[,paste(year_,id,id_neigh8)] & 
                     !paste(year_,id,id_neigh14) %in% neigh9[,paste(year_,id,id_neigh9)] & 
                     !paste(year_,id,id_neigh14) %in% neigh10[,paste(year_,id,id_neigh10)] & 
                     !paste(year_,id,id_neigh14) %in% neigh11[,paste(year_,id,id_neigh11)] & 
                     !paste(year_,id,id_neigh14) %in% neigh12[,paste(year_,id,id_neigh12)] & 
                     !paste(year_,id,id_neigh14) %in% neigh13[,paste(year_,id,id_neigh13)]]
neigh14[,id_neigh:=NULL]
neigh14 <- unique(neigh14)
neigh14[, no := 14]



foo = as.data.table(expand.grid(box1=unique(coor$box), box2=unique(coor$box), year_=c(2021)))
#foo = foo[box1!=box2]
foo = merge(foo, terrbox, by.x=c('box1', 'year_'), by.y=c('box', 'year_'))
foo = merge(foo, terrbox, by.x=c('box2', 'year_'), by.y=c('box', 'year_'), suffixes=c('1','2'))


foo1 = merge(foo, neigh1, by.x=c('territory1','territory2','year_'), by.y=c('id','id_neigh','year_'), all.x=TRUE)
foo1 = foo1[!is.na(no)]

foo2 = merge(foo, neigh2, by.x=c('territory1','territory2','year_'), by.y=c('id','id_neigh2','year_'), all.x=TRUE)
foo2 = foo2[!is.na(no)]

foo3 = merge(foo, neigh3, by.x=c('territory1','territory2','year_'), by.y=c('id','id_neigh3','year_'), all.x=TRUE)
foo3 = foo3[!is.na(no)]

foo4 = merge(foo, neigh4, by.x=c('territory1','territory2','year_'), by.y=c('id','id_neigh4','year_'), all.x=TRUE)
foo4 = foo4[!is.na(no)]

foo5 = merge(foo, neigh5, by.x=c('territory1','territory2','year_'), by.y=c('id','id_neigh5','year_'), all.x=TRUE)
foo5 = foo5[!is.na(no)]

foo6 = merge(foo, neigh6, by.x=c('territory1','territory2','year_'), by.y=c('id','id_neigh6','year_'), all.x=TRUE)
foo6 = foo6[!is.na(no)]

foo7 = merge(foo, neigh7, by.x=c('territory1','territory2','year_'), by.y=c('id','id_neigh7','year_'), all.x=TRUE)
foo7 = foo7[!is.na(no)]

foo8 = merge(foo, neigh8, by.x=c('territory1','territory2','year_'), by.y=c('id','id_neigh8','year_'), all.x=TRUE)
foo8 = foo8[!is.na(no)]

foo9 = merge(foo, neigh9, by.x=c('territory1','territory2','year_'), by.y=c('id','id_neigh9','year_'), all.x=TRUE)
foo9 = foo9[!is.na(no)]

foo10 = merge(foo, neigh10, by.x=c('territory1','territory2','year_'), by.y=c('id','id_neigh10','year_'), all.x=TRUE)
foo10 = foo10[!is.na(no)]

foo11 = merge(foo, neigh11, by.x=c('territory1','territory2','year_'), by.y=c('id','id_neigh11','year_'), all.x=TRUE)
foo11 = foo11[!is.na(no)]

foo12 = merge(foo, neigh12, by.x=c('territory1','territory2','year_'), by.y=c('id','id_neigh12','year_'), all.x=TRUE)
foo12 = foo12[!is.na(no)]

foo13 = merge(foo, neigh13, by.x=c('territory1','territory2','year_'), by.y=c('id','id_neigh13','year_'), all.x=TRUE)
foo13 = foo13[!is.na(no)]

foo14 = merge(foo, neigh14, by.x=c('territory1','territory2','year_'), by.y=c('id','id_neigh14','year_'), all.x=TRUE)
foo14 = foo14[!is.na(no)]



#Combine data
    neighbour = rbind(foo1, foo2, foo3, foo4, foo5, foo6, foo7, foo8, foo9, foo10, foo11, foo12, foo13, foo14, foo15, foo16, foo17, foo18, foo19)
    neighbour = neighbour[,.(year_, box1, box2, no)]
    self <- data.frame (year_=2021, box1=unique(coor$box), box2=unique(coor$box), no=0)
    neighbour = rbind(neighbour, self)
  

#adjust to new box locations
    foo = merge(neighbour, locations, by.x=c('box1'), by.y=c('new'), allow.cartesian=TRUE)
    foo[, box1 := NULL]
    setnames(foo, "old", "box1")
    
    foo = merge(foo, locations, by.x=c('box2'), by.y=c('new'), allow.cartesian=TRUE)
    foo[, box2 := NULL]
    setnames(foo, "old", "box2")
    

#safe file
    write.csv(foo, file='./data/neighbour.csv', row.names=FALSE)

