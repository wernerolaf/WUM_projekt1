library(tidyverse)
library(DataExplorer)
library(mlr)
library(vtreat)


heloc_dataset_v1<-read.csv("heloc_dataset_v1.csv")
heloc_no9 <- heloc_dataset_v1[heloc_dataset_v1$MSinceMostRecentTradeOpen != -9, ]

heloc_no9 %>% filter(RiskPerformance== "Good") %>% select(-RiskPerformance) -> good
heloc_no9 %>% filter(RiskPerformance== "Bad") %>% select(-RiskPerformance)-> bad

no7<-apply(heloc_no9,1,function(x){all(x!=-7)})
no8<-apply(heloc_no9,1,function(x){all(x!=-8)})
no87<-no7 & no8
heloc7<-heloc_no9[!no7,]
heloc8<-heloc_no9[!no8,]
helocno987<-heloc_no9[no87,]
heloc_clean<-heloc_no9
heloc_clean$ConditionNotMet<-!no7
heloc_clean$NoTrades<-!no8
heloc_clean[-1]<-apply(heloc_clean[-1],c(1,2),function(x){ ifelse(x %in% c(-7,-8),NA,x)})
heloc_clean$MaxDelq2PublicRecLast12M<-as.factor(heloc_clean$MaxDelq2PublicRecLast12M)
heloc_clean$MaxDelqEver<-as.factor(heloc_clean$MaxDelqEver)
xd<-factor(sapply(as.numeric(heloc_clean$MaxDelqEver),function(x){ifelse(x %in% c(1,2,7,9),NA,x)}))


treat<-vtreat::designTreatmentsC(heloc_clean,names(heloc_clean)[-1],"RiskPerformance","Good")
scols <- c('varName','sig','extraModelDegrees','origName','code')
print(treat$scoreFrame[, scols])
test<-prepare(treat,heloc_clean,pruneSig = 1/length(treat$scoreFrame$varName))


#create_report(good)
#create_report(bad)




#Z powyższego raportu zauważyliśmy znaczące róznice w istorgamach odpowiadających RiskPerformance "Bad" i  "Good"", są to chechy:
"MSinceOldestTradeOpen"  
"MSinceOldestTradeOpen"  
"PercentTradesNeverDelq"  
"MSinceMostRecentDelq" 
"MaxDelq2PublicRecLast12M" 
"MaxDelqEver" 
"NetFractionRevolvingBurden" !!! 
  "NetFractionInstallBurden" !!
  "NumRevolvingTradesWBalance"
"NumBank2NatlTradesWHighUtilization"
"PercentTradesWBalance"  



















