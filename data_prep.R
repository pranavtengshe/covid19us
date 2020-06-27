library(dplyr)

County_Pop = read.csv('Data/co-est2019-alldata.csv', stringsAsFactors = F)

NYT_County_Covid_Counts= read.csv('https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv', stringsAsFactors = F)

County_Pop= County_Pop %>% 
  group_by(STATE) %>% 
  mutate(STATE_POPESTIMATE2019= sum(POPESTIMATE2019))

County_Pop= County_Pop[, c('CountyMIPS','STATE','STNAME','POPESTIMATE2019','STATE_POPESTIMATE2019','RNETMIG2019')]

NYT_County_Covid_Counts_agg= NYT_County_Covid_Counts %>% 
  group_by(fips) %>% 
  mutate(totalcases= max(cases), totaldeaths= max(deaths)) %>%
  arrange(date) %>% 
  

  mutate( rrate= mean(cases/lag(cases), na.rm=T )) %>% 
  distinct (fips, totalcases,totaldeaths,rrate)

glimpse(County_Covid_Counts_agg)

County_Covid_Summary= NYT_County_Covid_Counts_agg %>% 
  inner_join(County_Pop,by = c("fips"="CountyMIPS"))

County_Covid_Summary['DeathRatio']= County_Covid_Summary$totaldeaths/County_Covid_Summary$totalcases

County_Covid_Summary['InfectionRatiop1000']= County_Covid_Summary$totalcases*1000/County_Covid_Summary$POPESTIMATE2019

glimpse(County_Covid_Summary)

State_Testing= read.csv('https://covidtracking.com/api/v1/states/current.csv', stringsAsFactors = F)

State_Testing= State_Testing[,c('fips','total','positive')]

State_Testing= State_Testing %>% 
  rename('testingtotal'='total') %>% 
  rename('testedpositive'='positive')


View(County_Covid_Summary)

County_Covid_Summary= County_Covid_Summary %>% 
  inner_join(State_Testing, by= c('STATE'='fips')) %>% 
  mutate(TestingRatio=  testingtotal/STATE_POPESTIMATE2019) %>% 
  mutate(PositivityRatio= testedpositive/testingtotal)

glimpse(County_Covid_Summary)

svi_data= read.csv('Data/socialvulnerability.csv', stringsAsFactors = F)

svi_data= svi_data[,c('countyFIPS','Value')] %>% 
  rename('svi'='Value')

County_Covid_Summary= County_Covid_Summary %>% 
  left_join(svi_data, by =c('fips'='countyFIPS'))


copd_data= read.csv('Data/Copd.csv', stringsAsFactors = F)

copd_data$Value= as.numeric(ifelse(copd_data$Value=='Suppressed',NA,copd_data$Value ))

copd_data$Value= ifelse(is.na(copd_data$Value), mean(as.numeric(copd_data$Value), na.rm = T),copd_data$Value)

County_Covid_Summary= County_Covid_Summary %>% 
  left_join(copd_data, by= c('fips'='countyFIPS')) %>% 
  rename('copd_rate'='Value')

County_Covid_Summary['Data.Comment']= NULL
    


View(Heart_Attack_data)= read.csv('Data/HeartAttack.csv')

Heart_Attack_data$Value= as.numeric(na_if(Heart_Attack_data$Value,'Suppressed'))

Heart_Attack_data$Value= ifelse(is.na(Heart_Attack_data$Value),mean(Heart_Attack_data$Value,na.rm=T),Heart_Attack_data$Value)


Heart_Attack_data <- reshape(Heart_Attack_data, v.names = "Value", idvar = "countyFIPS",
                timevar = "Gender", direction = "wide")

County_Covid_Summary= County_Covid_Summary %>% 
  left_join(Heart_Attack_data, by= c('fips'='countyFIPS')) %>% 
  rename(c('Heart_Disease_Male'='Value.Male','Heart_Disease_Female'= 'Value.Female'))


Obesity_Data= read.csv('Data/Obesity.csv', stringsAsFactors = F)

Obesity_Data= Obesity_Data[,c('stateFIPS','Value')]

Obesity_Data$Value= gsub('%','',Obesity_Data$Value)


County_Covid_Summary= County_Covid_Summary %>% 
  left_join(Obesity_Data, by=c('STATE'='stateFIPS')) %>% 
  rename('Obesity_percent'='Value')
  


