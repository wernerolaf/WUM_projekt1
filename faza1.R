library(DataExplorer)
library(dplyr)
library(mlr)
library(rpart)
library(vtreat)

heloc_dataset_v1<-read.csv(file = "heloc_dataset_v1.csv")

DataExplorer::plot_missing(heloc_dataset_v1)
DataExplorer::plot_boxplot(heloc_dataset_v1, by="RiskPerformance")

heloc_no9 <- heloc_dataset_v1[heloc_dataset_v1$MSinceMostRecentTradeOpen != -9, ]

heloc_no9 %>% filter(RiskPerformance== "Good") %>% select(-RiskPerformance) -> good
heloc_no9 %>% filter(RiskPerformance== "Bad") %>% select(-RiskPerformance)-> bad

#create_report(good)
#create_report(bad)
a<-2*max(heloc_no9$MSinceMostRecentDelq); a


heloc_no9$MSinceMostRecentDelq[heloc_no9$MSinceMostRecentDelq== -8  ]<-a
heloc_no9$MSinceMostRecentDelq[heloc_no9$MSinceMostRecentDelq == -7]<-a

hist(heloc_no9$MSinceMostRecentDelq)

levels(as.factor(heloc_no9$MSinceMostRecentDelq))


heloc_no9$MSinceMostRecentDelq[heloc_no9$MSinceMostRecentDelq== -8]<-a
heloc_no9$MSinceMostRecentDelq[heloc_no9$MSinceMostRecentDelq == -7]<-a

heloc_no9$MSinceMostRecentInqexcl7days[heloc_no9$MSinceMostRecentInqexcl7days== -8]<-2*max(heloc_no9$MSinceMostRecentInqexcl7days)
heloc_no9$MSinceMostRecentInqexcl7days[heloc_no9$MSinceMostRecentInqexcl7days== -7]<-2*max(heloc_no9$MSinceMostRecentInqexcl7days)
hist(heloc_no9$MSinceMostRecentInqexcl7days)

heloc_no9$NetFractionRevolvingBurden[heloc_no9$NetFractionRevolvingBurden == -8]<-0
heloc_no9$NetFractionRevolvingBurden[heloc_no9$NetFractionRevolvingBurden == -7]<-0
hist(heloc_no9$NetFractionRevolvingBurden)

heloc_no9$NetFractionInstallBurden[heloc_no9$NetFractionInstallBurden== -8]<-0
heloc_no9$NetFractionInstallBurden[heloc_no9$NetFractionInstallBurden== -7]<-0
hist(heloc_no9$NetFractionInstallBurden)

b<-median(heloc_no9$NumRevolvingTradesWBalance)
heloc_no9$NumRevolvingTradesWBalance[heloc_no9$NumRevolvingTradesWBalance == -8]<-b
heloc_no9$NumRevolvingTradesWBalance[heloc_no9$NumRevolvingTradesWBalance == -7]<-b
hist(heloc_no9$NumRevolvingTradesWBalance)

c<-median(heloc_no9$NumInstallTradesWBalance)
heloc_no9$NumInstallTradesWBalance[heloc_no9$NumInstallTradesWBalance == -8]<-c
heloc_no9$NumInstallTradesWBalance[heloc_no9$NumInstallTradesWBalance == -7]<-c
hist(heloc_no9$NumInstallTradesWBalance)

heloc_no9$NumBank2NatlTradesWHighUtilization[heloc_no9$NumBank2NatlTradesWHighUtilization == -8]<-0
heloc_no9$NumBank2NatlTradesWHighUtilization[heloc_no9$NumBank2NatlTradesWHighUtilization == -7]<-0
hist(heloc_no9$NumBank2NatlTradesWHighUtilization)

d<-median(heloc_no9$PercentTradesWBalance)
heloc_no9$PercentTradesWBalance[heloc_no9$PercentTradesWBalance == -8]<-d
heloc_no9$PercentTradesWBalance[heloc_no9$PercentTradesWBalance == -7]<-d
hist(heloc_no9$PercentTradesWBalance)


tree<-rpart(RiskPerformance~ . ,
            heloc_no9[1:7000,]);tree


pred<-predict(tree,heloc_no9[7001:9871,-1],type="class");pred
conf<-table(heloc_no9[7001:9871,]$RiskPerformance,pred);conf

sum(diag(conf))/sum(conf)

task<-mlr::makeClassifTask(data=heloc_no9,target = "RiskPerformance");task

learner<-makeLearner("classif.randomForest", predict.type = "response")
moz<-listLearners(check.packages = TRUE)


cv <- makeResampleDesc("CV", iters = 5)
r <- resample(learner, task, cv,measures=list(tpr))
heloc_no9 %>% filter(RiskPerformance== "Good") %>% plot_histogram()
heloc_no9 %>% filter(RiskPerformance== "Bad") %>% plot_histogram()

