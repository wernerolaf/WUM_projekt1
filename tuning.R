
library(mlr)
heloc_ok<-read.csv("heloc_ok.csv")
heloc_ok<-heloc_ok[-1]
classif_task <- makeClassifTask( data = heloc_ok, target = "RiskPerformance")
rf_pars <- tuneParams(
  
  makeLearner("classif.randomForest", predict.type = "prob"),
  subsetTask(classif_task),
  resampling = cv5,
  measures = mlr::auc,
  par.set = makeParamSet(
    makeDiscreteParam("ntree", values = 100:1000),
    makeDiscreteParam("mtry", values = 10:50),
    makeDiscreteParam("nodesize", values = seq(1, 50, by = 5))
  ),
  control = makeTuneControlRandom(maxit = 10)
  
)
