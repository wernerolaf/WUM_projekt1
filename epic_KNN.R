library(tidyverse)
library(dplyr)
library(FNN)
library(gbm)
library(randomForest)
library(e1071)
library(ranger)
library(MASS)
library(DALEX)
library(mlr)
heloc_dataset_v1<-read.csv(file = "heloc_dataset_v1.csv")

# Czyszczenie
#-9
heloc_no9 <- heloc_dataset_v1[heloc_dataset_v1$ExternalRiskEstimate != -9, ]
#-7
heloc_no9$MSinceMostRecentDelq[heloc_no9$MSinceMostRecentDelq == -7]<-max(heloc_no9$MSinceMostRecentDelq)
heloc_no9$MSinceMostRecentInqexcl7days[heloc_no9$MSinceMostRecentInqexcl7days== -7]<-max(heloc_no9$MSinceMostRecentInqexcl7days)
#doczyszczanie
unclean<-apply(heloc_no9[,-1], 1, function(x){any(x %in% c(-9,-8,-7))})
heloc_clean<-heloc_no9[!unclean,]
heloc_unclean<-heloc_no9[unclean,]

# Epic knn imputation

heloc_clean_knn<-heloc_clean %>% dplyr::select(-MaxDelqEver,-MaxDelq2PublicRecLast12M,-RiskPerformance)
heloc_unclean_knn<-heloc_unclean %>% dplyr::select(-MaxDelqEver,-MaxDelq2PublicRecLast12M,-RiskPerformance)

for(i in 1:nrow(heloc_unclean)) {
  #print(i)
  badcols <- which(heloc_unclean_knn[i,] %in% c(-9,-8,-7))
  k <- FNN::knn(heloc_clean_knn[-badcols], heloc_unclean_knn[i,-badcols], cl=heloc_clean[[1]] , k=5,algorithm="cover_tree")
  indices <- attr(k, "nn.index")
  heloc_clean_knn[indices[1,],] %>% summarise_at(names(heloc_clean_knn)[badcols],mean)->imput
  heloc_unclean_knn[i,badcols]<-imput
}

#mozna optymalniej ale dziala
heloc_unclean[!which(names(heloc_unclean) %in% c("MaxDelqEver","MaxDelq2PublicRecLast12M","RiskPerformance"))] <-heloc_unclean_knn

heloc_ok<-sample_frac(rbind(heloc_clean,heloc_unclean))


heloc_ok$MaxDelqEver<-factor(heloc_ok$MaxDelqEver)
heloc_ok$MaxDelq2PublicRecLast12M<-factor(heloc_ok$MaxDelq2PublicRecLast12M)


heloc_ok<-read.csv("heloc_ok.csv")[-1]

#
outcome <- "RiskPerformance"

names <- setdiff(colnames(heloc_ok), outcome)


isTrain <- runif(nrow(heloc_ok)) <= 0.7; isTrain
dTrain <- heloc_ok[isTrain, , drop = FALSE]
dTest <- heloc_ok[!isTrain, , drop = FALSE]

treatment <- vtreat::mkCrossFrameCExperiment(dTrain, names, outcome, "Good")
treatment_to_prepare<-treatment$treatments
newNames <- treatment_to_prepare$scoreFrame$varName
crossFrame <- treatment$crossFrame

depending <- paste(outcome, 
                   paste(newNames,  collapse = ' + '), 
                   sep = ' ~ ')

model <- ranger(as.formula(depending),  probability = TRUE,  data = crossFrame)

treatedTest <- vtreat::prepare(treatment_to_prepare, dTest, 
                               pruneSig = NULL, 
                               varRestriction = newNames)
pred <- predict(model, 
                data=treatedTest, 
                type='response')


treatedTest$pred <- pred$predictions[,"Good"];treatedTest$pred


WVPlots::ROCPlot(treatedTest, 
                 'pred', outcome, "Good",
                 'test performance')


train_set <- sample_frac(heloc_ok, 0.6)
test_set <- setdiff(heloc_ok, train_set)

colnames(heloc_ok)
task <- makeClassifTask(data = train_set, target = "RiskPerformance")
a<-listLearners(task)
model_szybko <- train(makeLearner("classif.randomForest", 
                                  predict.type = "prob",
                                  ntree = 100), 
                      task)



model_szybko2 <- train(makeLearner("classif.svm", 
                                  predict.type = "prob"), 
                      task)


performance(predict(model_szybko2, newdata = test_set ),
            measures = auc)

train_index <- sample(1:nrow(heloc_ok), 0.6 * nrow(heloc_ok))
test_index <- setdiff(1:nrow(heloc_ok), train_index)

helocTest <- heloc_ok[test_index,]


classif_task <- makeClassifTask( data = heloc_ok, target = "RiskPerformance")
classif_lrn_rf <- makeLearner("classif.randomForest", predict.type = "prob")
classif_lrn_gbm <- makeLearner("classif.gbm", predict.type = "prob")
classif_lrn_svm <- makeLearner("classif.svm", predict.type = "prob")
classif_lrn_qda <- makeLearner("classif.qda", predict.type = "prob")
classif_lrn_rpart <- makeLearner("classif.rpart", predict.type = "prob")

classif_rf <- train(classif_lrn_rf, classif_task, subset=train_index)
classif_gbm <- train(classif_lrn_gbm, classif_task, subset=train_index)
classif_svm <- train(classif_lrn_svm, classif_task, subset=train_index)
classif_qda <- train(classif_lrn_qda, classif_task, subset=train_index)
classif_rpart <- train(classif_lrn_rpart, classif_task, subset=train_index)


y_test <-as.numeric((helocTest$RiskPerformance))


# TEST

train_index <- sample(1:nrow(heloc_ok), 0.8 * nrow(heloc_ok))

train <- heloc_ok[train_index,]
test <- heloc_ok[-train_index,]

# custom predict

custom_predict <- function(object, newdata) {
  pred <- predict(object, newdata=newdata)
  response <- pred$data$response
  return(response)
}

custom_predict_classif <- function(object, newdata) {pred <- predict(object, newdata=newdata)
response <- pred$data[,3]
return(response)}

explainer_classif_rf <- DALEX::explain(classif_rf, data=helocTest, y=y_test, label= "rf", predict_function = custom_predict_classif)
explainer_classif_gbm <- DALEX::explain(classif_gbm, data=helocTest, y=y_test, label="glm", predict_function = custom_predict_classif)
explainer_classif_svm <- DALEX::explain(classif_svm, data=helocTest, y=y_test, label ="svm", predict_function = custom_predict_classif)
explainer_classif_qda <- DALEX::explain(classif_qda, data=helocTest, y=y_test, label ="qda", predict_function = custom_predict_classif)
explainer_classif_rpart <- DALEX::explain(classif_rpart, data=helocTest, y=y_test, label ="rpart", predict_function = custom_predict_classif)



mp_classif_rf<-model_performance(explainer_classif_rf)
mp_classif_glm <- model_performance(explainer_classif_gbm)
mp_classif_svm <- model_performance(explainer_classif_svm)
mp_classif_qda <- model_performance(explainer_classif_qda)
mp_classif_rpart <- model_performance(explainer_classif_rpart)
plot(mp_classif_rf)

getParamSet(classif_lrn_rf)

rf_pars <- tuneParams(
  
  makeLearner("classif.randomForest", predict.type = "prob"),
  classif_task,
  resampling = cv5,
  measures = mlr::auc,
  par.set = makeParamSet(
    makeDiscreteParam("ntree", values = 100:1000),
    makeDiscreteParam("mtry", values = 10:50),
    makeDiscreteParam("nodesize", values = seq(1, 50, by = 5))
  ),
  control = makeTuneControlRandom(maxit = 20)
  # makeTuneControlGrid
  # makeTuneControlMBO
  # itd
)



## one hot encoding 

# sprawdzenie typów kolumn
library(caret)

ncol(heloc_ok)

for(i in 1:ncol(heloc_ok)) {
  print(class(heloc_ok[,i]))
}

heloc_ok[,"MaxDelqEver"] <- as.factor(heloc_ok[,"MaxDelqEver"])
heloc_ok[,"MaxDelq2PublicRecLast12M"] <- as.factor(heloc_ok[,"MaxDelq2PublicRecLast12M"])

library(data.table)
library(mltools)
heloc_even_better <- one_hot(as.data.table(heloc_ok), cols = c("MaxDelqEver","MaxDelq2PublicRecLast12M"))


rf_pars_better <- tuneParams(
  
  makeLearner("classif.randomForest", predict.type = "prob"),
  subsetTask(makeClassifTask(data = data.frame(heloc_even_better), target = "RiskPerformance")),
  resampling = cv5,
  measures = mlr::auc,
  par.set = makeParamSet(
    makeDiscreteParam("ntree", values = 100:1000),
    makeDiscreteParam("mtry", values = 10:50),
    makeDiscreteParam("nodesize", values = seq(1, 50, by = 5))
  ),
  control = makeTuneControlRandom(maxit = 100)
  # makeTuneControlGrid
  # makeTuneControlMBO
  # itd
)

rf_pars_better



