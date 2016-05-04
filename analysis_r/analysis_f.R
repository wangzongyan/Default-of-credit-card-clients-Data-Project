###############################
# analysis.R
################################
# anova(data, variable)
#' @param  variable: the variable interested for a One-way ANOVA and pairwise T
#' @source package testthat
#' @return a dataframe of p-value of the one-way ANOVA and pairwise T
#' 
# anova(data)
#' @return a data frame contains the p-value for one-way ANOVA and pairwise T
#' 
#' 
##This file provide the functions for building models:
#
## model(glm.fun, level = "Charged_Off")
#' @param glm.fun a logistic function of loan_status
#' @return an expression of model
#' 
#
## cv_k(data, k)
#' @param data is the data want to use glm and apply cross-validation
#' @param number of fold of cross-validation
#' @return cross validation error (random value)
#' 
#' 
#' 
#' 
##############################
#require(plyr)
#require(dplyr)
#require(ggplot2)
#require("XML")
#require("reshape2")
###############################
# a dataframe of p-value of the one-way ANOVA and pairwise T
anova_one <- function(data, variable){
  test_that(paste0("Cannot find ", variable, " in your data, please verify your data's name or variable's name"), {
    expect_equal(length(data[,variable]) > 0, T)
  })
  test_that("Your data should include variable loan_status", {
    expect_equal(length(data$loan_status) > 0, T)
  })
  anova.data <- data[, c("loan_status",variable)]
  # overall ANOVA
  data_all <- anova.data %>% 
    mutate(group=factor(anova.data$loan_status,labels=1:length(unique(anova.data$loan_status))))
  model_all <- aov(data_all[,variable] ~ loan_status, data = data_all)
  p.value_all <- summary(model_all)[[1]][["Pr(>F)"]][1]
  return(p.value_all)
}


# return a data frame contains the p-value for one-way ANOVA and pairwise T
anova <- function(data){
  p.list <- as.character(colnames(data)[colnames(data) != "loan_status"]) %>% sapply(., function(x) anova_one(data, x))
  return(p.list)
}



#Logistic Regression
###############################
logistic <- function(data){
  if(length(data$weight) >0){
    l<-glm(y~., data, weights = weight, family=binomial(link = "logit"))
  }else{
    l<-glm(y~., data, family=binomial(link = "logit"))
  }
  
  return(l)
}

# model
model <- function(glm.fun, level){
  glm.fun$coefficients[is.na(glm.fun$coefficients)] <- 0
  paste0("odds(p(", level, ")) = ", "exp(", 
         paste0("(", glm.fun$coefficients, ")", "*", 
                names(glm.fun$coefficients), collapse = " + "), ")")
}

###################
## Aggregate logistic prediction model
######################

## Cross-Validation resample
cv_k <- function(data, k = 10, threshold = 0.5){
  flds <- createFolds(1:length(data[,1]), k = k, list = TRUE, returnTrain = FALSE)
  error <- c()
  cv.error <- c()
  cv.weight <- 1
  cv_error <- 1
    for(i in 1:10){
      test <- data[flds[[i]],]
      train <- data[-flds[[i]],]
      log.f <- logistic(train)
      pred <- predict(log.f, test, type = "response")
      pred1 <- as.numeric(pred >= threshold)
      error[i] <- 1- sum(pred1 == test$y, na.rm = T)/length(test$y)
    }
  cv.error <- mean(error)
  return(cv.error)
}

####################
# SVM
######################
svm <- function(lc_svm, kernel = "rbfdot"){
  # an example of R code, goodLoan = Fully_paid or current loan status
  # change multi-level response to binary response
  lc_svm <- lc_svm %>% na.omit()
    # SVM
  SVM_X <- as.matrix(lc_svm %>% dplyr::select(-y))
  SVM_Y <- as.matrix(lc_svm %>% dplyr::select(y))
  
  SVM_GaussianKernel <- ksvm(SVM_X, SVM_Y, kernel = kernel, type = "C-svc")
 
  return(SVM_GaussianKernel)
}


cv_ksvm <- function(data, threshold, C, kernel = "rbfdot", k = 10){
  flds <- createFolds(1:length(data[,1]), k = k, list = TRUE, returnTrain = FALSE)
  error <- c()
  cv.error <- c()
  cv.weight <- 1
  cv_error <- 1
  C.value = C
  n = length(C.value)
  for(i in 1:k){
    for(j in 1:n){
      test <- data[flds[[i]],]
      train <- data[-flds[[i]],]
      train.Y <- as.matrix(train %>% dplyr::select(default))
      train.X <- as.matrix(train %>% dplyr::select(-default))
      log.f <- ksvm(train.X, train.Y, kernel = kernel, type = "C-svc", C = C.value[j])
      newdata <- as.matrix(test%>% dplyr::select(-default))
      pred <- predict(log.f, newdata)
      pred = as.numeric(pred > threshold)
      error[j] <- 1- sum(pred == test$default, na.rm = T)/length(test$default)
    }
    cv.error[i] <- mean(error)
  }
  cv.error <- data.frame(C = C.value, error = error)
  return(cv.error)
}



# Calculate the roc(FP: pred=1,true=0; FN:pred=0, true=1)
calculate_roc <- function(df, cost_of_fp, cost_of_fn, n=100) {
  tpr <- function(df, threshold) {
    sum(df$pred >= threshold & df$true == 1, na.rm = T) / sum(df$true == 1, na.rm = T)
  }
  
  fpr <- function(df, threshold) {
    sum(df$pred >= threshold & df$true == 0, na.rm = T) / sum(df$true == 0, na.rm = T)
  }
  
  cost <- function(df, threshold, cost_of_fp, cost_of_fn) {
    sum(df$pred >= threshold & df$true == 0, na.rm = T) * cost_of_fp + 
      sum(df$pred < threshold & df$true == 1, na.rm = T) * cost_of_fn
  }
  
  roc <- data.frame(threshold = seq(0,1,length.out=n), tpr=NA, fpr=NA)
  roc$tpr <- sapply(roc$threshold, function(th) tpr(df, th))
  roc$fpr <- sapply(roc$threshold, function(th) fpr(df, th))
  roc$cost <- sapply(roc$threshold, function(th) cost(df, th, cost_of_fp, cost_of_fn))
  
  return(roc)
}

###############################################
### EM algorithm 
#################################################

#######################
# regression EM
###########################
M_step <- function(data){
  lc_l <- data %>% na.omit() %>% mutate(y = y1 + y2)%>%
    dplyr::select(-y1,-y2,-id)
  # SVM
  k <- lm(y ~., data = lc_l)
  return(k)
}
E_step <- function(k, data){
  newdata <- data%>% dplyr::select(-y1,-y2, -id)
  pred <- predict(k, newdata)
  data$y2[data$y2 != 0] <- pred[data$y2 != 0] - data$y1[data$y2 != 0]
  data$y2[data$y2 < 0] <- 1
  return(data)
}





#############################################
# Kernel regression EM
################################
M_step_kern <- function(data, kernel = "rbfdot"){
  lc_svm <- data %>% na.omit() %>% mutate(y = y1 + y2)%>%
    dplyr::select(-y1,-y2,-id)
  # SVM
  SVM_X <- as.matrix(lc_svm %>% dplyr::select(-y))
  SVM_Y <- as.matrix(lc_svm %>% dplyr::select(y))
  
  k <- ksvm(SVM_X, SVM_Y, kernel = kernel)
  return(k)
}
E_step_kern <- function(k, data){
  newdata <- as.matrix(data%>% dplyr::select(-y1,-y2, -id))
  pred <- predict(k, newdata)
  data$y2[data$y2 != 0] <- pred[data$y2 != 0] - data$y1[data$y2 != 0]
  data$y2[data$y2 < 0] <- 1
  return(data)
}