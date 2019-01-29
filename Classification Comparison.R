#' @title c_accuracy
#' @description calculates accuracy metrics, including recall, precision, accuracy, tpr, fpr, fmeasure, TP, TN, FP, and FN
#' @param actuals a vector of actual values (the true classification) 
#' @param classifications a vector of predicted values
#' @return list of scores in order c("recall","precision","accuracy","tpr","fpr","fmeasure","TP","TN","FP","FN")
c_accuracy=function(actuals, classifications){
  
  df=data.frame(actuals, classifications)
  
  TP=nrow(df[df$classifications==1 & df$actuals==1,])  # true positive        
  FP=nrow(df[df$classifications==1 & df$actuals==0,])  # false positive
  FN=nrow(df[df$classifications==0 & df$actuals==1,])  # false negative
  TN=nrow(df[df$classifications==0 & df$actuals==0,])  # true negative
  
  
  recall=round(TP/(TP+FN),3)
  precision=round(TP/(TP+FP),3)
  accuracy=round((TP+TN)/(TP+FN+FP+TN),3)
  tpr=recall
  fpr=round(FP/(FP+TN),3)
  fmeasure=round(2*precision*recall/(precision+recall),3)
  scores=c(recall,precision,accuracy,tpr,fpr,fmeasure,TP,FP,FP,FN)
  names(scores)=c("recall","precision","accuracy","tpr","fpr","fmeasure","TP","TN","FP","FN")
  
  scoresB=c(TP,FP,FP,FN)
  names(scoresB)=c("TP","TN","FP","FN")
  
  # print(scoresB)
  return(list(scores));
}

#' @title optimize_fmeasure
#' @description finds the probability threshold that optimizes the fmeasure accuracy metric for binary prediction
#' @param actuals a vector of actual values (the true classification) 
#' @param classifications a vector of predicted values/probabilities
#' @param rangelow (optional) integer indicating lowest value for prediction range (default = 0)
#' @param rangehigh (optional) integer indicating highest value for prediction range (default = 1)
#' @return summary containing optimal threshold and optimal fmeasure for classification
optimize_fmeasure <- function(actuals, predictions, rangelow, rangehigh) {
  
  if (missing(rangelow)) rangelow <- 0
  if (missing(rangehigh)) rangehigh <- 1
  
  thresholds <- seq(from = rangelow, to = rangehigh, by = (rangehigh-rangelow)/1000)
  optimalThreshold <- 0
  optimalFmeasure <- 0
  
  for (i in thresholds) {
    classifications <- if_else(predictions > i, 1, 0)
    
    df <- data.frame(actuals, classifications)
    
    TP <- nrow(df[df$classifications==1 & df$actuals==1,])  # true positive        
    FP <- nrow(df[df$classifications==1 & df$actuals==0,])  # false positive
    FN <- nrow(df[df$classifications==0 & df$actuals==1,])  # false negative
    TN <- nrow(df[df$classifications==0 & df$actuals==0,])  # true negative
    
    fmeasure <- round(2*(TP/(TP+FP))*(TP/(TP+FN)) / 
                        ((TP/(TP+FP))+(TP/(TP+FN))),3)
    
    if (is.nan(fmeasure)) fmeasure <- 0
    
    if (fmeasure > optimalFmeasure) {
      optimalFmeasure <- fmeasure
      optimalThreshold <- i
    } # end if
    
  } # end for
  
  summary <- c(optimalThreshold, optimalFmeasure)
  names(summary) <- c("Optml Thresh", "Optml Fmeasure")
  return(summary)
}

#' @title roc_df
#' @description Plot on ROC curve to compare models and estimate optimal accuracy thresholds; gets TPR and FPR at every 1/1000th of provided range
#' @param actuals a vector of actual values (the true classification) 
#' @param classifications a vector of predicted values/probabilities
#' @param rangelow (optional) integer indicating lowest value for prediction range (default = 0)
#' @param rangehigh (optional) integer indicating highest value for prediction range (default = 1)
#' @return df containing true positive rate and false positive rate for plotting 
roc_df <- function(actuals, predictions, rangelow, rangehigh) {
  
  if (missing(rangelow)) rangelow <- 0
  if (missing(rangehigh)) rangehigh <- 1
  
  thresholds <- seq(from = rangelow, to = rangehigh, by = (rangehigh-rangelow)/1000)
  tpr <- vector()
  fpr <- vector()
  for (i in thresholds) {
    classifications <- if_else(predictions > i, 1, 0)
    
    df <- data.frame(actuals, classifications)
    
    TP=nrow(df[df$classifications==1 & df$actuals==1,]);  # true positive        
    FP=nrow(df[df$classifications==1 & df$actuals==0,]);  # false positive
    FN=nrow(df[df$classifications==0 & df$actuals==1,]);  # false negative
    TN=nrow(df[df$classifications==0 & df$actuals==0,]);  # true negative
    
    tprate=round(TP/(TP+FN),3)
    fprate=round(FP/(FP+TN),3)
    
    # if (is.nan(tpr)) tprate <- 0
    # if (is.nan(fpr)) tprate <- 0  
    
    tpr[i*1000] <- tprate
    fpr[i*1000] <- fprate
  } # end for
  
  rates <- data.frame(tpr,fpr)
  return(rates)
}

### --- Find optimal Threshold & Fmeasure ---

optimize_fmeasure(test$CKD, pred)     # 0.213     # 0.464
optimize_fmeasure(test2$CKD, pred2)   # 0.224     # 0.467
optimize_fmeasure(test$CKD, predA)    # 0.229     # 0.472
optimize_fmeasure(test2$CKD, pred2A)  # 0.215     # 0.467
optimize_fmeasure(test$CKD, predB)    # 0.234     # 0.465
optimize_fmeasure(test2$CKD, pred2B)  # 0.237     # 0.467

### --- Plot ROC curves for comparison ---
# set up data
df1 <- data.frame(roc_df(test$CKD, pred), "Original","Full")
df2 <- data.frame(roc_df(test2$CKD, pred2), "Factor","Full")
df3 <- data.frame(roc_df(test$CKD, predA),"Original","Significant")
df4 <- data.frame(roc_df(test2$CKD, pred2A),"Factor","Significant")
df5 <- data.frame(roc_df(test$CKD, predB), "Original","Selection")
df6 <- data.frame(roc_df(test2$CKD, pred2B), "Factor","Selection")

colnames(df1) <- c("tpr","fpr","data","model")
colnames(df2) <- c("tpr","fpr","data","model")
colnames(df3) <- c("tpr","fpr","data","model")
colnames(df4) <- c("tpr","fpr","data","model")
colnames(df5) <- c("tpr","fpr","data","model")
colnames(df6) <- c("tpr","fpr","data","model")

ggdf <- bind_rows(df1,df2,df3,df4,df5,df6)
ggdf$data <- factor(ggdf$data)
ggdf$model <- factor(ggdf$model)
rm(df1, df2, df3, df4, df5, df6)

# compare curves
ggplot(ggdf, aes(x=fpr,y=tpr)) +
  geom_line(aes(color = model, linetype = data), size = 1) + 
  scale_linetype_manual(values=c("solid", "dotdash"))+
  scale_color_manual(values=c("#E69F00", "#56B4E9", "#009E73")) +
  labs(title= "ROC curve", 
       x = "False Positive Rate (1-Specificity)", 
       y = "True Positive Rate (Sensitivity)") +
  theme_bw()
# models are extremely similar

### --- Calculate AUC for comparison ---
install.packages('pROC')
library(pROC)
cat("AUC for original data model w/ all variables: ", 
    auc(test$CKD, pred, percent=TRUE, plot=F), "%","\n", sep="")    # 90.72%
cat("AUC for factor model w/ all variables: ", 
    auc(test2$CKD, pred2, percent=TRUE, plot=F), "%","\n", sep="")  # 90.61%
cat("AUC for original data model w/ significant variables: ", 
    auc(test$CKD, predA, percent=TRUE, plot=F), "%","\n", sep="")   # 90.06%
cat("AUC for factor data model w/ significant variables: ", 
    auc(test2$CKD, pred2A, percent=TRUE, plot=F), "%","\n", sep="") # 90.18%
cat("AUC for original data model w/ backwards selected variables: ", 
    auc(test$CKD, predB, percent=TRUE, plot=F), "%","\n", sep="")   # 90.53%
cat("AUC for factor data model w/ backwards selected variables: ", 
    auc(test2$CKD, pred2B, percent=TRUE, plot=F), "%","\n", sep="") # 90.34%
# models are extremely similar!