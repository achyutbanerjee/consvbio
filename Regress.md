Logistic regression
================
Banerjee AK et al.
March 15, 2022

**Objectives of the study:** To identify variables’ influence on sale
availability of alien plant species of three invasion categories

``` r
#install the required packages
library(sjPlot)
library(sjmisc)
library(rlang)

#Set the working directory
setwd("C:/Users/Achyut/OneDrive/R_codes/2023_consbio")

data<-read.csv("regress_data1.csv")
sale_availability<-as.factor(data$Sale.Availability)
#Three invasion categories together
fit1 <- glm(sale_availability ~ GrowthForm, data = data,family = binomial("logit"),maxit=100)
summary(fit1)
sjPlot::sjp.poly(fit1)
fit2 <- glm(sale_availability ~ MRT, data = data,family = binomial("logit"),maxit=100)
summary(fit2)
fit3 <- glm(sale_availability ~ Uses, data = data,family = binomial("logit"),maxit=100)
summary(fit3)
fit4 <- glm(sale_availability ~ Nat.range, data = data,family = binomial("logit"),maxit=100)
summary(fit4)
fit5 <- glm(sale_availability ~ Natu.range, data = data,family = binomial("logit"),maxit=100)
summary(fit5)
fit6 <- glm(sale_availability ~ Nat.con, data = data,family = binomial("logit"),maxit=100)
summary(fit6)
fit7 <- glm(sale_availability ~ invasion_status_final, data = data,family = binomial("logit"),maxit=100)
summary(fit7)

#Invasive alien species
data_invasive<-data[data$invasion_status_final == "0", ]
sale_availability<-as.factor(data_invasive$Sale.Availability)
fit1 <- glm(sale_availability ~ GrowthForm, data = data_invasive,family = binomial("logit"),maxit=100)
fit2 <- glm(sale_availability ~ MRT, data = data_invasive,family = binomial("logit"),maxit=100)
fit3 <- glm(sale_availability ~ Uses, data = data_invasive,family = binomial("logit"),maxit=100)
fit4 <- glm(sale_availability ~ Nat.range, data = data_invasive,family = binomial("logit"),maxit=100)
fit5 <- glm(sale_availability ~ Natu.range, data = data_invasive,family = binomial("logit"),maxit=100)
fit6 <- glm(sale_availability ~ Nat.con, data = data_invasive,family = binomial("logit"),maxit=100)
summary(fit1)
summary(fit2)
summary(fit3)
summary(fit4)
summary(fit5)
summary(fit6)

#Naturalized alien species
data_naturalized<-data[data$invasion_status_final == "1", ]
sale_availability<-as.factor(data_naturalized$Sale.Availability)
fit1 <- glm(sale_availability ~ GrowthForm, data = data_naturalized,family = binomial("logit"),maxit=100)
fit2 <- glm(sale_availability ~ MRT, data = data_naturalized,family = binomial("logit"),maxit=100)
fit3 <- glm(sale_availability ~ Uses, data = data_naturalized,family = binomial("logit"),maxit=100)
fit4 <- glm(sale_availability ~ Nat.range, data = data_naturalized,family = binomial("logit"),maxit=100)
fit5 <- glm(sale_availability ~ Natu.range, data = data_naturalized,family = binomial("logit"),maxit=100)
fit6 <- glm(sale_availability ~ Nat.con, data = data_naturalized,family = binomial("logit"),maxit=100)
summary(fit1)
summary(fit2)
summary(fit3)
summary(fit4)
summary(fit5)
summary(fit6)

#Introduced alien species
data_introduced<-data[data$invasion_status_final == "2", ]
sale_availability<-as.factor(data_introduced$Sale.Availability)
fit1 <- glm(sale_availability ~ GrowthForm, data = data_introduced,family = binomial("logit"),maxit=100)
fit2 <- glm(sale_availability ~ MRT, data = data_introduced,family = binomial("logit"),maxit=100)
fit3 <- glm(sale_availability ~ Uses, data = data_introduced,family = binomial("logit"),maxit=100)
fit4 <- glm(sale_availability ~ Nat.range, data = data_introduced,family = binomial("logit"),maxit=100)
fit5 <- glm(sale_availability ~ Natu.range, data = data_introduced,family = binomial("logit"),maxit=100)
fit6 <- glm(sale_availability ~ Nat.con, data = data_introduced,family = binomial("logit"),maxit=100)
summary(fit1)
summary(fit2)
summary(fit3)
summary(fit4)
summary(fit5)
summary(fit6)
```

**END**

**References**

- \[Paper - Conservation Biology\]
  (<https://doi.org/10.1111/cobi.14055>)
