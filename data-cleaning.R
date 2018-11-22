

graphics.off()
rm(list=ls())

library('tidyverse')

data_0 = read_csv('train_loan.csv')

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

X = as.matrix(select(data, -Loan_Status))
Y = data$Loan_Status

n = dim(X)[1]

data_df = data.frame(Y,X)
