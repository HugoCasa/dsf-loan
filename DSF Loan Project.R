
graphics.off()
rm(list=ls())

detach("package:MASS", unload=TRUE)
library('tidyverse')

data_0 = read_csv('train_loan.csv')
data_0_test = read_csv('test_loan.csv')

####### Data Cleaning

data = data_0 %>%
  select(-Loan_ID) %>%
  na.omit() %>%
  mutate(Gender = sapply(Gender, switch, 
                            "Male" = 1, 
                            "Female" = 0) ) %>%
  mutate(Married = sapply(Married, switch, 
                         "Yes" = 1, 
                         "No" = 0) ) %>%
  mutate(Dependents = sapply(Dependents, switch,
                             '0' = 0,
                             '1' = 1,
                             '2' = 2,
                             '3+' = 3 )) %>%
  mutate(Self_Employed = sapply(Self_Employed, switch, 
                          "Yes" = 1, 
                          "No" = 0) ) %>%
  mutate(Education = sapply(Education, switch, 
                              "Graduate" = 1, 
                              "Not Graduate" = 0) ) %>%
  mutate(Property_Area = sapply(Property_Area, switch, 
                            "Urban" = 1, 
                            "Semiurban" = 0,
                            "Rural" = -1) ) %>%
  mutate(Loan_Status = sapply(Loan_Status, switch, 
                     Y = 1, 
                     N = 0) )


# Yes = 1; No = 0

X = as.matrix(select(data, Credit_History))
#X = as.matrix(select(data, -Loan_Status))
Y = data$Loan_Status

n = dim(X)[1]

data_df = data.frame(Y,X)

####### Logistic Regression

model_logit = glm(Y ~., data_df, family=binomial(link="logit"))
beta_logistic  = model_logit$coefficients

eta_logistic = rep(0,n)
T1_Err_Log = 0 # False Positive
T2_Err_Log = 0 # False Negative
for (i in 1:n) {
  eta_logistic[i] = exp( c(1,X[i,]) %*% beta_logistic)/(1 + exp(c(1, X[i,]) %*% beta_logistic))
  if (eta_logistic[i] >= 0.5 & Y[i] == 0) {
    T1_Err_Log = T1_Err_Log + 1
  }
  if (eta_logistic[i] < 0.5 & Y[i] == 1) {
    T2_Err_Log = T2_Err_Log + 1
  }
}

Emp_Err_Log = (T1_Err_Log + T2_Err_Log) / n

####### KNN

library("class")


# Training errors
labels = knn(X, X, Y, k = 1)

T1_Err_KNN = 0
T2_Err_KNN = 0
for (i in 1:n) {
  if (labels[i] != Y[i]) {
    if (labels[i] == 1) {
      T1_Err_KNN = T1_Err_KNN + 1
    } else {
      T2_Err_KNN = T2_Err_KNN + 1 
    }
  }
}

Emp_Err_KNN = (T1_Err_KNN + T2_Err_KNN) / n

### perceptron

Empirical_error_perceptron_function = function(beta) {
  X_per = cbind(rep(1,n), X) %*% beta
  X_per[which(X_per >= 0.5)] = 1
  X_per[which(X_per < 0.5)] = 0
  length(which( X_per != Y))
}

optim_result = optim(par=rep(0,2), fn=Empirical_error_perceptron_function, method="SANN") # we can not print itertions with optim()
beta_perceptron = optim_result$par

X_per = cbind(rep(1,n), X) %*% beta_perceptron
X_per[which(X_per >= 0.5)] = 1
X_per[which(X_per < 0.5)] = 0
T1_Err_Per = length(which(X_per != Y & X_per == 1))
T2_Err_Per = length(which(X_per != Y & X_per == 0))
Emp_Err_Per = length(which(X_per != Y))/n


####### LDA

library(MASS)
fit <- lda(Y ~ Credit_History, data=data_df, 
           na.action="na.omit", CV=TRUE)
LDA_Err <- table(data_df$Y, fit$class)
T1_Err_LDA = LDA_Err[1,2]
T2_Err_LDA = LDA_Err[2,1]
Emp_Err_LDA = (T1_Err_LDA + T2_Err_LDA) / n

