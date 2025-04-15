Comparison of variables between species offered for sale and not offered
for sale
================
Banerjee AK et al.
March 15, 2022

**Objectives of the study:** To compare variables between the alien
species offered for sale and not offered for sale in Chinese online
marketplaces

``` r
#install the required packages
library(tidyverse)
library(ggpubr)
library(rstatix)
library(datarium)
library(rstatix)
library(Rmisc)
library(car)
library(FSA)

#Set the working directory
setwd("C:/Users/Achyut/OneDrive/R_codes/2023_consbio")

data<-read.csv("mrt.csv") #Change the variable names to: "height.csv", "natCon.csv", "natRange.csv", "natuRange.csv", "seed.csv", "sla.csv","use.csv"
data$group<-as.factor(data$Sale.Availability)
data$group1<-as.factor(data$invasion_status)
levels(data$group)
levels(data$group1)
#Check descriptive
tgc <- summarySE(data, measurevar="MRT", groupvars=c("group","group1"))
tgc
tgc1 <- summarySE(data, measurevar="MRT", groupvars=c("group"))
tgc1
#visualize data
bxp <- ggboxplot(
  data, x = "group", y = "MRT", 
  ylab = "MRT", xlab = "Community", add = "jitter"
)
bxp
bxp <- ggboxplot(
  data, x = "group1", y = "MRT", 
  ylab = "MRT", xlab = "Community", add = "jitter"
)
bxp

##Compare selling and non-selling species##
#Compute Shapiro wilk test by groups (normality check)
data %>%
  group_by(group) %>%
  shapiro_test(MRT)
#Compute levene test by groups (homogeneity of variance check)
leveneTest(MRT ~ group, data = data)
#Compare and effect size estimation
m1<-wilcox.test(MRT ~ group, data=data, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
m1
m<-t.test(MRT ~ group, data=data, var.equal=FALSE, na.rm=TRUE)
print(m)
d<-wilcox_effsize(MRT ~ group, data=data,paired=FALSE)
d
##Subset data to compare selling and non-selling species for each group separately##
#For 'INVASIVE' category
data_invasive<-data[data$invasion_status == "Invasive", ]
tgc <- summarySE(data_invasive, measurevar="MRT", groupvars=c("group"))
tgc
data_invasive %>%
  group_by(group) %>%
  shapiro_test(MRT)
leveneTest(MRT ~ group, data = data_invasive)
m1<-wilcox.test(MRT ~ group, data=data_invasive, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
m1
d<-wilcox_effsize(MRT ~ group, data=data_invasive,paired=FALSE)
d
m<-t.test(MRT ~ group, data=data_invasive, var.equal=FALSE, na.rm=TRUE)
print(m)
#For 'NATURALIZED' category
data_naturalized<-data[data$invasion_status == "Naturalized", ]
tgc <- summarySE(data_naturalized, measurevar="MRT", groupvars=c("group"))
tgc
data_naturalized %>%
  group_by(group) %>%
  shapiro_test(MRT)
leveneTest(MRT ~ group, data = data_naturalized)
m1<-wilcox.test(MRT ~ group, data=data_naturalized, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
m1
d<-wilcox_effsize(MRT ~ group, data=data_naturalized,paired=FALSE)
d
m<-t.test(MRT ~ group, data=data_naturalized, var.equal=FALSE, na.rm=TRUE)
print(m)
#For 'INTRODUCED' category
data_introduced<-data[data$invasion_status == "Introduced", ]
tgc <- summarySE(data_introduced, measurevar="MRT", groupvars=c("group"))
tgc
data_introduced %>%
  group_by(group) %>%
  shapiro_test(MRT)
leveneTest(MRT ~ group, data = data_introduced)
m1<-wilcox.test(MRT ~ group, data=data_introduced, na.rm=TRUE, paired=FALSE, exact=FALSE, conf.int=TRUE)
m1
d<-wilcox_effsize(MRT ~ group, data=data_introduced,paired=FALSE)
d
m<-t.test(MRT ~ group, data=data_introduced, var.equal=FALSE, na.rm=TRUE)
print(m)

##Subset data (sold species) to compare between groups##
data_1<-data[data$Sale.Availability == "Selling", ]
tgc <- summarySE(data_1, measurevar="MRT", groupvars=c("group1"))
tgc
data_1 %>%
  group_by(group1) %>%
  shapiro_test(MRT)
leveneTest(MRT ~ group1, data = data_1)
kruskal.test(MRT ~ group1, data = data_1)
kruskal_effsize(MRT ~ group1,data = data_1)
dunnTest(MRT~group1,data=data_1)
```

**END**

**References**

- \[Paper - Conservation Biology\]
  (<https://doi.org/10.1111/cobi.14055>)
