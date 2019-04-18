library(tidyverse)
library(dplyr)
library(FNN)
library(gbm)
library(mlr)
library(randomForest)
library(e1071)
library(ranger)
library(MASS)
library(DALEX)
library(xgboost)
library(DataExplorer)
heloc_dataset_v1<-read.csv(file = "heloc_dataset_v1.csv")

# Czyszczenie
#-9
heloc_no9 <- heloc_dataset_v1[heloc_dataset_v1$ExternalRiskEstimate != -9, ]
#-7
heloc_no9$MSinceMostRecentDelq[heloc_no9$MSinceMostRecentDelq == -7]<-max(heloc_no9$MSinceMostRecentDelq)
heloc_no9$MSinceMostRecentInqexcl7days[heloc_no9$MSinceMostRecentInqexcl7days== -7]<-max(heloc_no9$MSinceMostRecentInqexcl7days)




give_me_AUC<-function(train_set)
{
  
  n<-ncol(train_set)-1;
  

  cv <- makeResampleDesc("CV", iters = 5)
  
  #Random Froest
  
  task <- makeClassifTask(data = train_set, target = "RiskPerformance")
  
  model_all_rf <- makeLearner("classif.randomForest", 
                                    predict.type = "prob",
                                    ntree = 100)
  
  auc_all_rf<-r <- resample(model_all_rf, task, cv,measures=list(acc))
  
  AUC<-auc_all_rf$aggr
  name<-"rf"
  number_of_cols<-n
  AUC
  # SVM
  
  model_all_svm <- makeLearner("classif.svm", 
                                     predict.type = "prob")
  
  auc_all_svm<-resample(model_all_svm, task, cv,measures=list(acc))
  AUC<-append(AUC, auc_all_svm$aggr)
  name<-append(name, "svm")
  number_of_cols<-append(n, n)
  #rpart
  
  model_all_rpart <- makeLearner("classif.rpart", 
                                       predict.type = "prob")
  
  auc_all_rpart<-resample(model_all_rpart, task, cv,measures=list(acc))
  AUC<-append(AUC, auc_all_rpart$aggr)
  name<-append(name, "rpart")
  number_of_cols<-append(n, n)
  
  
  #qda
  
  model_all_qda <- makeLearner("classif.qda", 
                                     predict.type = "prob")
  
  auc_all_qda<-resample(model_all_qda, task, cv,measures=list(acc))
  
  AUC<-append(AUC, auc_all_qda$aggr)
  name<-append(name, "qda")
  number_of_cols<-append(n, n)
  
  #lda
  model_all_lda <- makeLearner("classif.lda", 
                                     predict.type = "prob")
  
  auc_all_lda<-resample(model_all_lda, task, cv,measures=list(acc))
  
  AUC<-append(AUC, auc_all_lda$aggr)
  name<-append(name, "lda")
  number_of_cols<-append(n, n)
  
  #naive Bayes
  model_all_nb <- makeLearner("classif.naiveBayes", 
                                    predict.type = "prob")
  
  auc_all_nb<-resample(model_all_nb, task, cv,measures=list(acc))
                          
  
  AUC<-append(AUC, auc_all_nb$aggr)
  name<-append(name, "nb")
  number_of_cols<-append(n, n)
  
  
  
  A<-data.frame(AUC=AUC, Classifier=name, number_of_cols=number_of_cols)
  
  
  
}

score1<-give_me_AUC(heloc_no9)


eloc_notall<-subset(heloc_no9, select=c( "RiskPerformance", 
                                         "MSinceOldestTradeOpen",  
                                         "PercentTradesNeverDelq",  
                                         "MSinceMostRecentDelq" ,
                                         "MaxDelq2PublicRecLast12M", 
                                         "MaxDelqEver" ,
                                         "NetFractionRevolvingBurden" , 
                                         "NetFractionInstallBurden" ,
                                         "NumRevolvingTradesWBalance",
                                         "NumBank2NatlTradesWHighUtilization",
                                         "PercentTradesWBalance"  ))

score2<-give_me_AUC(eloc_notall)

heloc_notall2<-subset(heloc_no9, select=c( "RiskPerformance", 
                                           "MSinceOldestTradeOpen",  
                                           "PercentTradesNeverDelq",  
                                           "MSinceMostRecentDelq" ,
                                           "NetFractionRevolvingBurden" , 
                                           "NetFractionInstallBurden" ,
                                           "NumRevolvingTradesWBalance",
                                           "NumBank2NatlTradesWHighUtilization",
                                           "PercentTradesWBalance"  ))
score3<-give_me_AUC(heloc_notall2)



heloc_notall3<-heloc_no9[,-12]
heloc_notall3<-heloc_notall3[,-11]


score4<-give_me_AUC(heloc_notall3)

heloc_notall4<-subset(heloc_no9, select=c( "RiskPerformance", 
                                           "ExternalRiskEstimate" , 
                                           "PercentTradesNeverDelq",  
                                           "MSinceMostRecentDelq" ,
                                           "AverageMInFile"  ,
                                           "NumSatisfactoryTrades",
                                           "NetFractionRevolvingBurden" , 
                                           "NumBank2NatlTradesWHighUtilization",
                                           "PercentTradesWBalance"  ))
score5<-give_me_AUC(heloc_notall4)

heloc_notall5<-subset(heloc_no9, select=c( "RiskPerformance", 
                                           "ExternalRiskEstimate" , 
                                           "PercentTradesNeverDelq",  
                                           "MSinceMostRecentDelq"))

score6<-give_me_AUC(heloc_notall5)

AUC_all<-rbind(score1, score2, score3, score4, score5, score6)
max(AUC_all$AUC)



ggplot(AUC_all, aes(x=AUC_all$number_of_cols, y=AUC_all$AUC, color=AUC_all$Classifier)) +
  geom_point(size=3)+
  scale_color_discrete(name="Classifier",
                       labels=c("rf", "svm", "rpart", "qda", "lda", "nb"))+
  xlab("number of features")+
  ylab("AUC")+
  ylim(min(AUC_all$AUC)-0.05, max(AUC_all$AUC)+0.05)+
  xlim(min(AUC_all$number_of_cols)-3, max(AUC_all$number_of_cols)+3)




