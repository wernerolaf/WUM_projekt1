library(tidyverse)
library(class)
library(caret)
library(dplyr)

heloc_dataset_v1<-read.csv(file = "heloc_dataset_v1.csv")

# -9 
# całe wiersze + 10 wierszy z -9 w ExternalRiskEstimate w całości

heloc_no9 <- heloc_dataset_v1[heloc_dataset_v1$ExternalRiskEstimate != -9, ]

heloc_no9_2<-heloc_no9[,c(- 12, -13) ]

# -7

heloc_no9$MSinceMostRecentDelq[heloc_no9$MSinceMostRecentDelq == -7]<-max(heloc_no9$MSinceMostRecentDelq)

heloc_no9$MSinceMostRecentInqexcl7days[heloc_no9$MSinceMostRecentInqexcl7days== -7]<-max(heloc_no9$MSinceMostRecentInqexcl7days)


#-8
heloc_MSinceOldestTradeOpen<-heloc_no9_2[heloc_no9$MSinceOldestTradeOpen != -8,]
idxs <- sample(1:nrow(heloc_MSinceOldestTradeOpen),as.integer(0.5*nrow(heloc_MSinceOldestTradeOpen)))
train <- heloc_MSinceOldestTradeOpen[idxs,]
test <- heloc_MSinceOldestTradeOpen[-idxs,]

model<-class::knn(train,test, cl=train$MSinceMostRecentTradeOpen, k=5)

# Czyszczenie

unclean<-apply(heloc_dataset_v1[,-1], 1, function(x){any(x %in% c(-9,-8,-7))})
heloc_clean<-heloc_dataset_v1[!unclean,]
heloc_unclean<-heloc_dataset_v1[unclean,]

# Epic knn imputation

heloc_clean_knn<-heloc_clean %>% select(-MaxDelqEver,-MaxDelq2PublicRecLast12M,-RiskPerformance)
heloc_unclean_knn<-heloc_unclean %>% select(-MaxDelqEver,-MaxDelq2PublicRecLast12M,-RiskPerformance)

for(i in 1:nrow(heloc_unclean)) {
  badcols <- which(heloc_unclean_knn[[i,]] %in% c(-9,-8,-7))
  k <- class::knn(heloc_clean_knn[-badcols], heloc_unclean_knn[[i,-badcols]], cl=heloc_clean[1] , k=5)
  
}





















#unclean<-apply(heloc_dataset_v1, 1, function(x){any(x %in% c(-9,-8,-7))})
#heloc_clean<-heloc_dataset_v1[!unclean,]
#(heloc_dataset_v1[1,] %in% c(-9,-8,-7))
#heloc_clean<-heloc_dataset_v1[clean,]
#heloc_clean %>% select(NetFractionRevolvingBurden) %>% filter()
#DataExplorer::plot_histogram(d)

