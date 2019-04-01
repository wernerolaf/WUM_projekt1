library(DataExplorer)
library(mlr)
library(vtreat)
library(rpart)
library(ggplot2)
library(dataMaid)
library(gridExtra)
library(dplyr)
heloc_dataset_v1<-read.csv("heloc_dataset_v1.csv")
heloc_clean<- heloc_dataset_v1[heloc_dataset_v1$MSinceMostRecentTradeOpen != -9, ]
heloc_clean[-1]<-apply(heloc_clean[-1],c(1,2),function(x){ ifelse(x %in% c(-7,-8,-9),NA,x)})
heloc_clean$MaxDelq2PublicRecLast12M<-as.factor(heloc_clean$MaxDelq2PublicRecLast12M)
heloc_clean$MaxDelqEver<-as.factor(heloc_clean$MaxDelqEver)

nazwy<-names(heloc_clean)[!names(heloc_clean) %in% c("RiskPerformance","MaxDelqEver","MaxDelq2PublicRecLast12M")]
wykresy<-list()

#problem z nazwami
for(i in nazwy){
  wykres<-ggplot(heloc_clean,aes(x=as.name(i),color=RiskPerformance))+geom_density()+theme_minimal()
  print(wykres)
  wykresy[[i]]<-wykres
}

apply(heloc_clean, 2, function(x){length(unique(x))})

grid.arrange(wykresy$ExternalRiskEstimate,wykresy$MSinceOldestTradeOpen,nrow=2)

ggplot(heloc_clean,aes(x=PercentTradesWBalance,color=RiskPerformance))+geom_density()+theme_minimal()
ggplot(heloc_clean,aes(x=MaxDelqEver,fill=RiskPerformance))+geom_bar()+theme_minimal()
ggplot(heloc_clean,aes(x=MaxDelq2PublicRecLast12M,fill=RiskPerformance))+geom_bar(position = "dodge",width = 0.7)+theme_minimal()+theme(axis.text.x = element_text(angle = 45))
levels(x) <- list(A="alpha", B="beta", C="gamma")

levels(heloc_clean$MaxDelq2PublicRecLast12M)<-c("derogatory comment","120+ days delq","90 days delq","60 days delq",
                                                "30 days delq","unknown delq","current and never delq","all other","all other")
interaction(names(heloc_clean),c("RiskPerformance","MaxDelqEver","MaxDelq2PublicRecLast12M"))

names(heloc_clean)[!names(heloc_clean) %in% c("RiskPerformance","MaxDelqEver","MaxDelq2PublicRecLast12M")]

dataMaid::basicVisual(heloc_clean$NetFractionInstallBurden,"xd")

ocena<-vtreat::value_variables_C(heloc_clean,names(heloc_clean)[-1],outcomename ="RiskPerformance",outcometarget = "Good")
ocena %>% arrange(sig)

set.seed(1)
treat<-vtreat::designTreatmentsC(heloc_clean,names(heloc_clean)[-1],"RiskPerformance","Good",rareCount=dim(heloc_clean)[1]/20)
scols <- c('varName','sig','extraModelDegrees','origName','code')
print(treat$scoreFrame[, scols])
test<-prepare(treat,heloc_clean,pruneSig = 1/length(treat$scoreFrame$varName))
