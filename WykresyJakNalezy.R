
library(readr)
library(mlr)
library(ggplot2)
library(DALEX)
library(knitr)

heloc_ok<-read_csv("heloc_ok.csv")
heloc_ok<-heloc_ok[-1]
heloc_ok$MaxDelqEver<-factor(heloc_ok$MaxDelqEver)
heloc_ok$MaxDelq2PublicRecLast12M<-factor(heloc_ok$MaxDelq2PublicRecLast12M)
heloc_ok<-mlr::createDummyFeatures(heloc_ok)
heloc_ok$RiskPerformance<-factor(heloc_ok$RiskPerformance)
heloc_dataset_v1 <- read_csv("heloc_dataset_v1.csv")
heloc_dataset_v1$RiskPerformance<-factor(heloc_dataset_v1$RiskPerformance)

task_unclean <- makeClassifTask(id = "task", data = heloc_dataset_v1, target ="RiskPerformance")
task_clean <- makeClassifTask( data = heloc_ok, target = "RiskPerformance")

classif_lrn_rf <- makeLearner("classif.randomForest", predict.type = "prob")
classif_lrn_gbm <- makeLearner("classif.gbm", predict.type = "prob")
classif_lrn_svm <- makeLearner("classif.svm", predict.type = "prob")
#classif_lrn_qda <- makeLearner("classif.qda", predict.type = "prob")
classif_lrn_rpart <- makeLearner("classif.rpart", predict.type = "prob", par.vals = list(minsplit=10))


cv <- makeResampleDesc("CV", iters = 5)

test_rf <- resample(classif_lrn_rf, task_clean, cv, measures = auc, show.info = FALSE)
test_gbm <- resample(classif_lrn_gbm, task_clean, cv, measures = auc, show.info = FALSE)
test_svm <- resample(classif_lrn_svm, task_clean, cv, measures = auc, show.info = FALSE)
#test_qda <- resample(classif_lrn_qda, task_clean, cv, measures = auc, show.info = FALSE) Error in qda.default(x, grouping, ...) : rank deficiency in group Bad
test_rpart <- resample(classif_lrn_rpart, task_clean, cv, measures = auc, show.info = FALSE)

roc_r = generateThreshVsPerfData(list(rf=test_rf$pred,gbm=test_gbm$pred,svm=test_svm$pred,rpart=test_rpart$pred), list(fpr, tpr), aggregate = TRUE)
plotROCCurves(roc_r)
