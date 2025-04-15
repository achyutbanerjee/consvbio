Structural Equation Modelling
================
Banerjee AK et al.
March 15, 2022

**Objectives of the study:** To identify variables’ direct and indirect
influence on sale availability of alien plant species of three invasion
categories

``` r
#install the required packages
library(lavaan)
library(semPlot)

#Set the working directory
setwd("C:/Users/Achyut/OneDrive/R_codes/2023_consbio")

#All 3 invasion categories together
data<-read.csv("regress_data.csv")
sale_availability<-as.factor(data$Sale_Availability)
HS.model1.1 <- '
Sale_Availability~GrowthForm+MRT_scaled+Use+Seed_scaled+Height_scaled+SLA_scaled+Nat.range+Natu.range_scaled+Nat.con
MRT_scaled~Nat.range+Natu.range_scaled+Use+Nat.con+GrowthForm+Seed_scaled+Height_scaled+SLA_scaled
Nat.range~GrowthForm+Seed_scaled+Height_scaled+SLA_scaled
Natu.range_scaled~GrowthForm+Seed_scaled+Height_scaled+SLA_scaled+Use+Nat.range
Use~Nat.range+Natu.range_scaled+Nat.con+GrowthForm+Seed_scaled+Height_scaled+SLA_scaled
Nat.con~GrowthForm+Seed_scaled+Height_scaled+SLA_scaled
'
fit1.1 <- sem(HS.model1.1, data=data,ordered="Sale_Availability")
fitMeasures(fit1.1, c("cfi","wrmr","rmsea"))
summary(fit1.1)
varTable(fit1.1)

#Invasive alien species
data_invasive<-data[data$invasion_status == "Invasive", ]
HS.model2.1 <- '
Sale_Availability~GrowthForm+MRT_scaled+Use+Seed_scaled+Height_scaled+SLA_scaled+Nat.range+Natu.range_scaled+Nat.con
MRT_scaled~Nat.range+Natu.range_scaled+Use+Nat.con+GrowthForm+Seed_scaled+Height_scaled+SLA_scaled
Nat.range~GrowthForm+Seed_scaled+Height_scaled+SLA_scaled
Natu.range_scaled~GrowthForm+Seed_scaled+Height_scaled+SLA_scaled+Use+Nat.range
Use~Nat.range+Natu.range_scaled+Nat.con+GrowthForm+Seed_scaled+Height_scaled+SLA_scaled
Nat.con~GrowthForm+Seed_scaled+Height_scaled+SLA_scaled
'
fit2.1 <- sem(HS.model2.1, data=data_invasive,ordered="Sale_Availability")
fitMeasures(fit2.1, c("cfi","wrmr","rmsea"))
summary(fit2.1)
varTable(fit2.1)

#Naturalized alien species
data_naturalized<-data[data$invasion_status == "Naturalized", ]
HS.model3.1 <- '
Sale_Availability~GrowthForm+MRT_scaled+Use+Seed_scaled+Height_scaled+SLA_scaled+Nat.range+Natu.range_scaled+Nat.con
MRT_scaled~Nat.range+Natu.range_scaled+Use+Nat.con+GrowthForm+Seed_scaled+Height_scaled+SLA_scaled
Nat.range~GrowthForm+Seed_scaled+Height_scaled+SLA_scaled
Natu.range_scaled~GrowthForm+Seed_scaled+Height_scaled+SLA_scaled+Use+Nat.range
Use~Nat.range+Natu.range_scaled+Nat.con+GrowthForm+Seed_scaled+Height_scaled+SLA_scaled
Nat.con~GrowthForm+Seed_scaled+Height_scaled+SLA_scaled
'
fit3.1 <- sem(HS.model3.1, data=data_naturalized,ordered="Sale_Availability")
fitMeasures(fit3.1, c("cfi","wrmr","rmsea"))
summary(fit3.1)
varTable(fit3.1)

#Introduced alien species
data_introduced<-data[data$invasion_status == "Introduced", ]
HS.model4.1 <- '
Sale_Availability~GrowthForm+MRT_scaled+Use+Seed_scaled+Height_scaled+SLA_scaled+Nat.range+Natu.range_scaled+Nat.con
MRT_scaled~Nat.range+Natu.range_scaled+Use+Nat.con+GrowthForm+Seed_scaled+Height_scaled+SLA_scaled
Nat.range~GrowthForm+Seed_scaled+Height_scaled+SLA_scaled
Natu.range_scaled~GrowthForm+Seed_scaled+Height_scaled+SLA_scaled+Use+Nat.range
Use~Nat.range+Natu.range_scaled+Nat.con+GrowthForm+Seed_scaled+Height_scaled+SLA_scaled
Nat.con~GrowthForm+Seed_scaled+Height_scaled+SLA_scaled
'
fit4.1 <- sem(HS.model4.1, data=data_introduced,ordered="Sale_Availability")
fitMeasures(fit4.1, c("cfi","wrmr","rmsea"))
summary(fit4.1)
varTable(fit4.1)

#Plot the results
semPaths(fit1.1, title = FALSE, curvePivot = TRUE) #Change the fitted models
semPaths(fit1.1, "std", edge.label.cex = 0.5, curvePivot = TRUE) #Change the fitted models
```

**END**

**References**

- \[Paper - Conservation Biology\]
  (<https://doi.org/10.1111/cobi.14055>)
