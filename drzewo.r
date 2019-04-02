library(DataExplorer)
heloc_dataset_v1<-read.csv(file = "heloc_dataset_v1.csv")

DataExplorer::plot_missing(heloc_dataset_v1)
DataExplorer::plot_boxplot(heloc_dataset_v1, by="RiskPerformance")
library(mlr)
library(rpart)


tree<-rpart(RiskPerformance~NetFractionRevolvingBurden+
              MSinceOldestTradeOpen+
              MSinceMostRecentTradeOpen+
              PercentTradesNeverDelq+
              MSinceMostRecentDelq+
              MaxDelq2PublicRecLast12M+
              MaxDelqEver+
              NetFractionInstallBurden+
              NumRevolvingTradesWBalance+
              NumBank2NatlTradesWHighUtilization+
              PercentTradesWBalance,
              heloc_no9[1:7000,]);tree
pred<-predict(tree,heloc_no9[7001:9871,-1],type="class");pred
conf<-table(heloc_no9[7001:9871,]$RiskPerformance,pred);conf

sum(diag(conf))/sum(conf)

task<-mlr::makeClassifTask(data=heloc_no9,target = "RiskPerformance");task
learner<-makeLearner("classif.svm", predict.type = "response")
moz<-listLearners(check.packages = TRUE)


cv <- makeResampleDesc("CV", iters = 5)
r <- resample(learner, task, cv,measures=list(tpr))
MSE <- r$aggr

MSE


# MaxDelqEver

unique(heloc_dataset_v1$MaxDelqEver)

sum(heloc_dataset_v1$MaxDelqEver == -9)


heloc_no9 <- heloc_dataset_v1[heloc_dataset_v1$MSinceMostRecentTradeOpen != -9, ]
heloc_9 <- heloc_dataset_v1[heloc_dataset_v1$MSinceMostRecentTradeOpen == -9, ]
heloc_9$RiskPerformance

# MaxDelq2PublicRecLast12M

unique(heloc_no9$MaxDelq2PublicRecLast12M)

sum(heloc_dataset_v1$MaxDelqEver == -9)

heloc_no9 <- heloc_dataset_v1[heloc_dataset_v1$MaxDelqEver != -9, ]

plot_boxplot(heloc_no9, by="RiskPerformance")

library(dplyr)

newcomers <- heloc_no9 %>% filter(NumInstallTradesWBalance < 0)


l = list()
for(col in 1:ncol(heloc_dataset_v1)) {
  l[col] = list(sort(unique(heloc_dataset_v1[,col])))
}
l

colnames(heloc_dataset_v1
         )
4 %in% c(12,3,4)

l = list()
for(col in 1:ncol(heloc_no9)) {
  l[col] = list(sort(unique(heloc_no9[,col])))
}
l



heloc_no9 %>% filter(MaxDelqEver != -9) %>% filter(ExternalRiskEstimate == -9)


# Podział danych

heloc_dataset_v1 %>% filter(PercentTradesWBalance == -8)

heloc_no9 %>% filter(MSinceMostRecentDelq != -7, MSinceMostRecentInqexcl7days != -7, PercentTradesWBalance != -8)
heloc_dataset_v1 %>% filter(MSinceMostRecentInqexcl7days == -7)

d <- heloc_dataset_v1
for(col in 1:ncol(d)){
  d <- d[!(d[,col] %in% -9:-7),]
}
d

plot_boxplot(d, by="RiskPerformance")
plot_correlation(d)
heloc_9
plot_boxplot(heloc_9, by="RiskPerformance")

plot_correlation(heloc_9)

heloc_9$RiskPerformance %>% table()

plot_bar(heloc_dataset_v1)
heloc_no9 %>% filter(RiskPerformance== "Good") %>% plot_histogram()

heloc_no9 %>% filter(RiskPerformance== "Bad") %>% plot_histogram()

heloc_no9 %>% filter(RiskPerformance== "Good") %>% select(-RiskPerformance) -> good
heloc_no9 %>% filter(RiskPerformance== "Bad") %>% select(-RiskPerformance)-> bad

create_report(good)
colnames(bad)
