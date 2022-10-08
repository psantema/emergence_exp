


#Get breeders
   breeders = data.table(read.csv(file = 'data/paternity2021.csv', sep = ",", stringsAsFactors = FALSE))
   breeders = unique(breeders[epy==0, .(father)])

#Get males with emergence data 
  foo = d1[date_>=put_out & date_< removed]
  foo[, breeding := ID %in% breeders$father]
  foo = foo[breeding=='TRUE']
  nrow(foo)/length(unique(foo$ID))
  
  unique(foo$ID[foo$treatment==0])
  
