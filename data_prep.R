library(dplyr)

County_Pop = read.csv('co-est2019-alldata.csv', stringsAsFactors = F)

County_Covid_Counts= read.csv('https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv', stringsAsFactors = F)

glimpse(County_Pop)

County_Pop= County_Pop[, c('CountyMIPS','STATE','STNAME','POPESTIMATE2019','RNETMIG2019')]

County_Covid_Counts_agg= County_Covid_Counts %>% 
  group_by(fips) %>% 
  mutate(totalcases= sum(cases), totaldeaths= sum(deaths)) %>%
  arrange(date) %>% 
  mutate( rrate= mean(cases/lag(cases), na.rm=T )) %>% 
  distinct (fips, totalcases,totaldeaths,rrate)

View(County_Covid_Counts_agg)
