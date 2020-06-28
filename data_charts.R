source('data_prep.R')

library(ggplot2)

library(randomForest)

library(rpart)

library(rpart.plot)

glimpse(County_Covid_Summary)

County_Covid_Summary = County_Covid_Summary[, c('fips'
                                                , 'rrate'
                                                ,'RNETMIG2019'
                                                ,'DeathRatio'
                                                ,'InfectionRatiop1000'
                                                ,'TestingRatio'
                                                ,'PositivityRatio'
                                                ,'svi'
                                                ,'copd_rate'
                                                ,'Heart_Disease_Male'
                                                ,'Heart_Disease_Female'
                                                ,'Obesity_percent'
                                                ,'CommuteTime_Mins'
                                                ,'Smoking_Men_Percentage'
                                                ,'Smoking_Women_Percentage'
                                                ,'Air_quality_Index'
                                                ,'Mobility_Percent')]



mtree=rpart((DeathRatio)~. -fips, data=County_Covid_Summary)

rpart.plot(mtree)

itree= rpart(InfectionRatiop1000~.-fips-rrate-DeathRatio, data=County_Covid_Summary)

rpart.plot(itree)

rtree= rpart(rrate~.-fips-InfectionRatiop1000-DeathRatio, data=County_Covid_Summary)

rpart.plot(rtree)

Rffit= randomForest(DeathRatio~.-fips, data= County_Covid_Summary)

varImpPlot(Rffit,type=2)

mean(County_Covid_Summary$rrate)
 
 