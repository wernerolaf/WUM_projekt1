library(tidyverse)
library(dplyr)
library(FNN)

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

heloc_clean_knn<-heloc_clean %>% select(-MaxDelqEver,-MaxDelq2PublicRecLast12M,-RiskPerformance)
heloc_unclean_knn<-heloc_unclean %>% select(-MaxDelqEver,-MaxDelq2PublicRecLast12M,-RiskPerformance)

for(i in 1:nrow(heloc_unclean)) {
  print(i)
  badcols <- which(heloc_unclean_knn[i,] %in% c(-9,-8,-7))
  k <- FNN::knn(heloc_clean_knn[-badcols], heloc_unclean_knn[i,-badcols], cl=heloc_clean[[1]] , k=5,algorithm="cover_tree")
  indices <- attr(k, "nn.index")
  heloc_clean_knn[indices[1,],badcols] %>% summarise_at(names(heloc_clean_knn)[badcols],mean)->imput
  heloc_unclean_knn[i,badcols]<-imput
}












