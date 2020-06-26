library(dplyr)

County_Pop = read.csv('Data/co-est2019-alldata.csv', stringsAsFactors = F)

glimpse(County_Pop)

County_Pop['CountyMIPS']= paste('0',County_Pop['CountyMIPS'])

County_Pop= County_Pop[, c('CountyMIPS','STATE','STNAME','POPESTIMATE2019','RNETMIG2019')]

County_Pop