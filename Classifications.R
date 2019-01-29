# --- glmnet - nonlinear regressions with regularizaion ---
install.packages(c('parallel','doParallel','glmnet'))
library(glmnet)
library(parallel)
library(doParallel)

# build parallel environment
ncores <- parallel::detectCores()-2
if (ncores > 6) {
  ncores <- 6
}

cl <- parallel::makeCluster(ncores)
doParallel::registerDoParallel(cl)

# glmnet needs a matrix
trainMX <- as.matrix(train)
  # might need to impute or na.omit(train)

# fit the model
idx <- 31 # column index of identifier
cvfit <- glmnet::cv.glmnet(x=trainMX[,-idx], y=trainMX[,idx],
                           family='binomial',                # model type: binomial vs multinomial 
                           type.measure='mse',               # loss function: deviance, class, auc, mse
                           alpha = 1,                        # elasticnet mix
                           type.logistic='modified.Newton',  # speeds logistic
                           #type.multinomial = 'grouped',    # use grouped lasso for multinomial
                           maxit=100000, 
                           nfolds=10, 
                           parallel=T)

# save model
saveRDS(cvfit, paste0(location, '/cvfit.RDS'), compress=T)

# verify model
predictions <- predict(object=cvfit, 
                       newx=trainMX[,-idx],  
                       type="response", # response = %; class = class ~ max probability
                       s="lambda.min")

# How good is it?  A result of 0 = meaningless, 1 = perfect.
install.packages('pROC')
library(pROC)
roc <- suppressWarnings(pROC::roc(train[,idx], predictions))
auc <- roc$auc
print(paste("The model has an AUC of", round(auc,4), sep=" "))
plot(roc)
rm(roc, auc) # remove to free up memory

# predict for new data
testMX <- as.matrix(test)
predictions <- predict(object=cvfit, 
                       newx=testMX,  # might need [,-idx] if class variable exists in test data
                       type="response", # response = %; class = class ~ max probability
                       s="lambda.min")
