library(tidyverse)

location <- '~/Documents/_SCHOOL/_DREXEL/Archives/STAT\ 630\ -\ Multivariate\ Analysis/Data/'
data <- read.csv(paste(location,'LogisticRegressionData.csv',sep='/'),stringsAsFactors = F)

# --- Basic exploration ---
  summary(data)
  
  # review % missing in all variables
  pctMiss <- as.data.frame(sapply(data, is.na)) %>%
    sapply(sum)
  pctMiss <- round(pctMiss/length(data$ID), 4)
  pctMiss

# --- clean factor (categorical) variables by creating a binary variable per category ---
  new <- model.matrix(~ -1 + Racegrp + CareSource, data=data)
  CKDdata <- data %>%
    select(-ID, -Racegrp, -CareSource)  %>%   # remove original factor variables
    cbind(new)   # add new binary variables to the data set 
  rm(new)


# --- scaling or normalizing ---
  # scale continuous data
  scaled <- as.data.frame(scale(data))
  # that doesn't look right - do we really want categorical data to be scaled in such a way?
  
  # many of our variables are dummy variables, so maybe we just 
  # normalize everything into 0,1 range?
  normalize <- function(df) {
    df <- as.matrix(df)
    min <- apply(df,2, min)
    max <- apply(df,2, max)
    range <- max-min
    norm <- function(x) { (x-min) / range } # normalize to 0,1 range
    return(as.data.frame(t(apply(df,1,norm))))
  }
  
  normed <- normalize(data)
  
# --- train/test or train/validate/test split ---
  
  # --- Remove NAs ---
  # find NAs in CKD column (identifies row numbers of missing CKD data)
  ckdNa <- which(is.na(CKDdata$CKD==1))
  
  # remove NAs from CKD column
  missing <- CKDdata[ckdNa,]
  clean <- CKDdata[-ckdNa,]
  rm(ckdNa)
  
  # use caret
  install.packages('caret')
  library(caret)
  inTrain <- createDataPartition(y = CKDdata$CKD, ## the outcome data are needed
                                 p = .75,         ## The percentage of data in the training set
                                 list = FALSE
                                 )
  
  train <- clean[ inTrain,]
  test <- clean[-inTrain,]
  
  # ensure distributions are roughly equivalent between groups
  summary(train)
  summary(test)
  # summary(val)

# --- impute missing values ---
install.packages('VIM')
library(VIM)

# The kNN function determines the k (default: 5) nearest neighbors of a record with missing
# values. For numerical variables the median of the k nearest neighbors is used as imputation
# value; for categorical variables the category that most often occurs in the k nearest neighbors
# is used:
idx <- 31 # column index of identifier
fixedMissing <- VIM::kNN(data=missing[,-idx], 
                         k=5)

# remove the additional columns built by the kNN function
names(fixedMissing)
fixedMissing <- fixedMissing %>%
  select(1:40)  

# review our work!
str(fixedMissing)  
summary(fixedMissing)   # no NA's!!!

# ---------------------------------------------------
# Visualizations
# ---------------------------------------------------
  
# --- corrplot ---
  install.packages('corrplot')
  library(corrplot)
  
  # categorical
  grouping <- CKDdata %>% 
    group_by(CKD) %>% 
    summarise(Female = sum(Female, na.rm = TRUE),
              Diabetes = sum(Diabetes, na.rm = TRUE), 
              Hypertension = sum(Hypertension, na.rm = TRUE), 
              CVD = sum(CVD, na.rm = TRUE),  
              RaceBlack = sum(Racegrpblack, na.rm = TRUE),
              RaceHispanic = sum(Racegrphispa, na.rm = TRUE),
              RaceOther = sum(Racegrpother, na.rm = TRUE), 
              RaceWhite = sum(Racegrpwhite, na.rm = TRUE))
  
  row.names(grouping) <- c("No","Yes")
  
  # Run chi-square test for categorical variables
  chisq <- chisq.test(grouping)
  
  # plot
  corrplot(chisq$residuals, is.cor = FALSE,
           tl.cex = .66, tl.col = "black")
  
  # continuous
  dataCors <- CKDdata[,-c(1,4,8)]    # remove the ID original factor variables
  temp <- sapply(dataCors, as.numeric)
  dataCors <- as_data_frame(temp)
  
  cors <- dataCors %>%
    cor(use="pairwise.complete.obs")
  
  corrplot(cors, order="FPC", tl.cex=.66,tl.col = "black",
           col=colorRampPalette(c("red","gray","blue"))(12))
  
  corrplot.mixed(cors, lower = "number", upper = "circle", order="FPC", tl.cex=.66,
                 lower.col="black",	number.cex=.66,
                 upper.col=colorRampPalette(c("red","gray","blue"))(8))
  
  # clean environment
  rm(grouping, chisq, dataCors, temp, cors)

