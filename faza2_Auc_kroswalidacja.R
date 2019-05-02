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
# heloc_dataset_v1<-read.csv(file = "heloc_dataset_v1.csv")

# # Czyszczenie
# #-9
# heloc_no9 <- heloc_dataset_v1[heloc_dataset_v1$ExternalRiskEstimate != -9, ]
# #-7
# heloc_no9$MSinceMostRecentDelq[heloc_no9$MSinceMostRecentDelq == -7]<-max(heloc_no9$MSinceMostRecentDelq)
# heloc_no9$MSinceMostRecentInqexcl7days[heloc_no9$MSinceMostRecentInqexcl7days== -7]<-max(heloc_no9$MSinceMostRecentInqexcl7days)
# heloc_dataset_v1<-read.csv(file = "heloc_dataset_v1.csv")

# # Czyszczenie
# #-9
# heloc_no9 <- heloc_dataset_v1[heloc_dataset_v1$ExternalRiskEstimate != -9, ]
# #-7
# heloc_no9$MSinceMostRecentDelq[heloc_no9$MSinceMostRecentDelq == -7]<-max(heloc_no9$MSinceMostRecentDelq)
# heloc_no9$MSinceMostRecentInqexcl7days[heloc_no9$MSinceMostRecentInqexcl7days== -7]<-max(heloc_no9$MSinceMostRecentInqexcl7days)
# #doczyszczanie
# unclean<-apply(heloc_no9[,-1], 1, function(x){any(x %in% c(-9,-8,-7))})
# heloc_clean<-heloc_no9[!unclean,]
# heloc_unclean<-heloc_no9[unclean,]

# # Epic knn imputation

# heloc_clean_knn<-heloc_clean %>% dplyr::select(-MaxDelqEver,-MaxDelq2PublicRecLast12M,-RiskPerformance)
# heloc_unclean_knn<-heloc_unclean %>% dplyr::select(-MaxDelqEver,-MaxDelq2PublicRecLast12M,-RiskPerformance)

# for(i in 1:nrow(heloc_unclean)) {
#   #print(i)
#   badcols <- which(heloc_unclean_knn[i,] %in% c(-9,-8,-7))
#   k <- FNN::knn(heloc_clean_knn[-badcols], heloc_unclean_knn[i,-badcols], cl=heloc_clean[[1]] , k=5,algorithm="cover_tree")
#   indices <- attr(k, "nn.index")
#   heloc_clean_knn[indices[1,],] %>% summarise_at(names(heloc_clean_knn)[badcols],mean)->imput
#   heloc_unclean_knn[i,badcols]<-imput
# }

# #mozna optymalniej ale dziala
# heloc_unclean[!which(names(heloc_unclean) %in% c("MaxDelqEver","MaxDelq2PublicRecLast12M","RiskPerformance"))] <-heloc_unclean_knn

# heloc_ok<-sample_frac(rbind(heloc_clean,heloc_unclean))


# heloc_ok$MaxDelqEver<-factor(heloc_ok$MaxDelqEver)
# heloc_ok$MaxDelq2PublicRecLast12M<-factor(heloc_ok$MaxDelq2PublicRecLast12M)





heloc_ok<-read_csv("heloc_ok.csv")
heloc_ok<-heloc_ok[-1]
heloc_ok$MaxDelqEver<-factor(heloc_ok$MaxDelqEver)
heloc_ok$MaxDelq2PublicRecLast12M<-factor(heloc_ok$MaxDelq2PublicRecLast12M)
heloc_ok<-mlr::createDummyFeatures(heloc_ok)
heloc_ok$RiskPerformance<-factor(heloc_ok$RiskPerformance)

give_me_AUC<-function(train_set)
{
  
 
  n<-ncol(train_set)-2;
  

  cv <- makeResampleDesc("CV", iters = 5)
  
  #Random Froest
  
  task <- makeClassifTask(data = train_set, target = "RiskPerformance")
  
  model_all_rf <- makeLearner("classif.randomForest", 
                                    predict.type = "prob")
  
  auc_all_rf<-r <- resample(model_all_rf, task, cv,measures=list(auc))
  
  AUC<-auc_all_rf$aggr
  name<-"rf"
  AUC
  # SVM
  
  model_all_svm <- makeLearner("classif.svm", 
                                     predict.type = "prob")
  
  auc_all_svm<-resample(model_all_svm, task, cv,measures=list(auc))
  AUC<-append(AUC, auc_all_svm$aggr)
  name<-append(name, "svm")
  #rpart
  
  model_all_rpart <- makeLearner("classif.rpart", 
                                       predict.type = "prob", par.vals = list(minsplit = 10))
  
  auc_all_rpart<-resample(model_all_rpart, task, cv,measures=list(auc))
  AUC<-append(AUC, auc_all_rpart$aggr)
  name<-append(name, "rpart")

  AUC
  name
  #gbm
  
   model_all_qda <- makeLearner("classif.gbm", 
                                      predict.type = "prob")
  
   auc_all_qda<-resample(model_all_qda, task, cv,measures=list(auc))
  
   AUC<-append(AUC, auc_all_qda$aggr)
   name<-append(name, "gbm")
  # number_of_cols<-append(n, n)
  
  A<-data.frame(AUC=AUC, Classifier=name, number_of_cols=n)
  
  
A  
}

score1<-give_me_AUC(heloc_ok)

score1$number_of_cols<-23

heloc_notall<-subset(heloc_ok, select=c( "RiskPerformance",
                                         "MSinceOldestTradeOpen",
                                         "PercentTradesNeverDelq",
                                         "MSinceMostRecentDelq" ,
                                      
                                         "NetFractionRevolvingBurden" ,
                                         "NetFractionInstallBurden" ,
                                         "NumRevolvingTradesWBalance",
                                         "NumBank2NatlTradesWHighUtilization",
                                         "PercentTradesWBalance","MaxDelq2PublicRecLast12M.0"  ,       "MaxDelq2PublicRecLast12M.1"  ,      
                                          "MaxDelq2PublicRecLast12M.2"   ,      "MaxDelq2PublicRecLast12M.3"   ,      "MaxDelq2PublicRecLast12M.4"   ,     
                                         "MaxDelq2PublicRecLast12M.5"  ,       "MaxDelq2PublicRecLast12M.6"    ,     "MaxDelq2PublicRecLast12M.7"  ,      
                                          "MaxDelq2PublicRecLast12M.9"  ,       "MaxDelqEver.2"      ,                "MaxDelqEver.3"  ,                   
                                          "MaxDelqEver.4"      ,                "MaxDelqEver.5"    ,                  "MaxDelqEver.6"    ,                
                                          "MaxDelqEver.7"  ,                    "MaxDelqEver.8"      ))

score2<-give_me_AUC(heloc_notall)

score2$number_of_cols<-10

heloc_notall2<-subset(heloc_ok, select=c( "RiskPerformance", 
                                           "MSinceOldestTradeOpen",  
                                           "PercentTradesNeverDelq",  
                                           "MSinceMostRecentDelq" ,
                                           "NetFractionRevolvingBurden" , 
                                           "NetFractionInstallBurden" ,
                                           "NumRevolvingTradesWBalance",
                                           "NumBank2NatlTradesWHighUtilization",
                                           "PercentTradesWBalance"  ))
score3<-give_me_AUC(heloc_notall2)

score3$number_of_cols<-8

heloc_notall3<-heloc_ok[1:22]

score4<-give_me_AUC(heloc_notall3)

score4$number_of_cols<-21

heloc_notall4<-subset(heloc_ok, select=c( "RiskPerformance", 
                                           "ExternalRiskEstimate" , 
                                           "PercentTradesNeverDelq",  
                                           "MSinceMostRecentDelq" ,
                                           "AverageMInFile"  ,
                                           "NumSatisfactoryTrades",
                                           "NetFractionRevolvingBurden" , 
                                           "NumBank2NatlTradesWHighUtilization",
                                           "PercentTradesWBalance"  ))
score5<-give_me_AUC(heloc_notall4)

score5$number_of_cols<-9

heloc_notall5<-subset(heloc_ok, select=c( "RiskPerformance", 
                                           "ExternalRiskEstimate" , 
                                           "PercentTradesNeverDelq",  
                                           "MSinceMostRecentDelq"))

score6<-give_me_AUC(heloc_notall5)

score6$number_of_cols<-3

AUC_all<-rbind(score1,score2, score3, score4 ,score5, score6)
max(AUC_all$AUC)



ggplot(AUC_all, aes(x=AUC_all$number_of_cols, y=AUC_all$AUC, color=AUC_all$Classifier)) +
  geom_point(size=3)+
  scale_color_discrete(name="Classifier",
                       labels=c("rf", "svm", "rpart", "gbm"))+
  xlab("number of features")+
  ylab("AUC")+
  ylim(min(AUC_all$AUC)-0.05, max(AUC_all$AUC)+0.05)+
  xlim(min(AUC_all$number_of_cols)-3, max(AUC_all$number_of_cols)+3)




