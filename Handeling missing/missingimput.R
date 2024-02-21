### Missing data imputation ####
### 04/08/22 ###
### Yuchen Zhang ####

library(dplyr)
library(tidyr)
library(tidyverse)
library(data.table)
library(Hmisc)
library(haven)
library(survival)		#Survival analysis
library(survminer) 		#Survival analysis
library(rms)
library(survMisc)
library(mice)

data_f <- fread("/Users/yuchen/Desktop/UKBiobank/Data/update/data_f.csv", header=TRUE, sep=",")

data_f$Alcohol_status = as.factor(data_f$Alcohol_status)
data_f$Smoking_status =as.factor(data_f$Smoking_status)
data_f$IPAQ_activity = as.factor(data_f$IPAQ_activity)
data_f$sex = as.factor(data_f$sex)
data_f$center = as.factor(data_f$center)
data_f$comorb = as.factor(data_f$comorb)

#data_f$omega3_pctg = as.factor(data_f$omega3_pctg)
#data_f$omega6_pctg = as.factor(data_f$omega6_pctg)
data_f$omega_ratiog = as.factor(data_f$omega_ratiog)

# select the dataset we will impute
impute = data_f %>% select(age, sex, TDI, center, eth_group_new, omega_ratio, time_to_death_months, death, death_cancer, death_cvd, BMI, Alcohol_status, Smoking_status,
                               IPAQ_activity, comorb, omega_ratiog, omegar_m)
# pattern of missing data exploration
p_missing <- unlist(lapply(impute, function(x) sum(is.na(x)))) / nrow(impute)
sort(p_missing[p_missing > 0], decreasing = TRUE)

# Change the character variables to factors
impute = impute %>% mutate(eth_group_new = ifelse(eth_group_new == "White", 1, 
                                                  ifelse(eth_group_new == "Black", 2, 
                                                         ifelse(eth_group_new == "Asian", 3,
                                                                ifelse(eth_group_new == "Others", 4, NA)))))
impute$eth_group_new = as.factor(impute$eth_group_new)


# We run the mice code with 0 iterations 

imp <- mice(impute, maxit=0)

# Extract predictorMatrix and methods of imputation 

predM <- imp$predictorMatrix
meth <- imp$method

# Setting values of variables I'd like to leave out to 0 in the predictor matrix
predM[, c("time_to_death_months")] <- 0
predM[, c("death")] <- 0
predM[, c("death_cancer")] <- 0
predM[, c("death_cvd")] <- 0
predM[, c("sex")] <- 0
predM[, c("age")] <- 0
predM[, c("comorb")] <- 0
predM[, c("omega_ratiog")] <- 0
predM[, c("omegar_m")] <- 0

# Specify a separate imputation model for variables of interest 

# Ordered categorical variables 
poly <- c("IPAQ_activity")

# Dichotomous variable
#log <- c("Fish_oil_baseline", "sex")

# Unordered categorical variable 
poly2 <- c("eth_group_new", "Alcohol_status", "Smoking_status")

# Turn their methods matrix into the specified imputation models
meth[poly] <- "polr"
#meth[log] <- "logreg"
meth[poly2] <- "polyreg"


# With this command, we tell mice to impute the impute data, create 5
# datasets, use predM as the predictor matrix and don't print the imputation
# process. If you would like to see the process, set print as TRUE

imp2 <- mice(impute, maxit = 5, 
             predictorMatrix = predM, 
             method = meth, print =  FALSE)

# Look at head and tail of imputed values for china_econ variable 
head(imp2$imp$IPAQ_activity)

# First, turn the datasets into long format
impute_long <- complete(imp2, action="long", include = TRUE)

# Convert back to mids type - mice can work with this type
impute_long_mids<-as.mids(impute_long)

#### Model

## all-cause

#### Minimum adjusted

# Continuous
fitimp_min_con <- with(impute_long_mids,
                       coxph(formula = Surv(time_to_death_months, death)~ omega_ratio + 
                                 age + TDI + eth_group_new + sex + center))

min_con = summary(pool(fitimp_min_con))
min_con$exp = round(exp(min_con$estimate),2)
min_con$lw = round(exp(min_con$estimate-1.96*min_con$std.error),2)
min_con$hi = round(exp(min_con$estimate+1.96*min_con$std.error),2)
min_con[1,]

# Category
fitimp_min_cat <- with(impute_long_mids,
                       coxph(formula = Surv(time_to_death_months, death)~ omega_ratiog + 
                                 age + TDI + eth_group_new + sex + center))

min_cat = summary(pool(fitimp_min_cat))
min_cat$exp = round(exp(min_cat$estimate),2)
min_cat$lw = round(exp(min_cat$estimate-1.96*min_cat$std.error),2)
min_cat$hi = round(exp(min_cat$estimate+1.96*min_cat$std.error),2)
min_cat[1:4, ]
# Test for trend
fitimp_min_trd <- with(impute_long_mids,
                       coxph(formula = Surv(time_to_death_months, death)~ omegar_m + 
                                 age + TDI + eth_group_new + sex + center))

summary(pool(fitimp_min_trd))



#### Fully adjusted

# Continuous
fitimp_ful_con <- with(impute_long_mids,
                       coxph(formula = Surv(time_to_death_months, death)~ omega_ratio + 
                                 age + TDI + eth_group_new + sex + center  + BMI + Smoking_status + Alcohol_status + IPAQ_activity + comorb))

ful_con = summary(pool(fitimp_ful_con))
ful_con$exp = round(exp(ful_con$estimate),2)
ful_con$lw = round(exp(ful_con$estimate-1.96*ful_con$std.error),2)
ful_con$hi = round(exp(ful_con$estimate+1.96*ful_con$std.error),2)
ful_con[1,]
# Category
fitimp_ful_cat <- with(impute_long_mids,
                       coxph(formula = Surv(time_to_death_months, death)~ omega_ratiog + 
                                 age + TDI + eth_group_new + sex + center  + BMI + Smoking_status + Alcohol_status + IPAQ_activity + comorb))

ful_cat = summary(pool(fitimp_ful_cat))
ful_cat$exp = round(exp(ful_cat$estimate),2)
ful_cat$lw = round(exp(ful_cat$estimate-1.96*ful_cat$std.error),2)
ful_cat$hi = round(exp(ful_cat$estimate+1.96*ful_cat$std.error),2)
ful_cat[1:4,]

# Test for trend
fitimp_ful_trd <- with(impute_long_mids,
                       coxph(formula = Surv(time_to_death_months, death)~ omegar_m + 
                                 age + TDI + eth_group_new + sex + center +  BMI + Smoking_status + Alcohol_status + IPAQ_activity + comorb))

summary(pool(fitimp_ful_trd))



## cancer

#### Minimum adjusted

# Continuous
fitimp_min_con <- with(impute_long_mids,
                       coxph(formula = Surv(time_to_death_months, death_cancer)~ omega_ratio + 
                                 age + TDI + eth_group_new + sex + center))

min_con = summary(pool(fitimp_min_con))
min_con$exp = round(exp(min_con$estimate),2)
min_con$lw = round(exp(min_con$estimate-1.96*min_con$std.error),2)
min_con$hi = round(exp(min_con$estimate+1.96*min_con$std.error),2)
min_con[1,]
# Category
fitimp_min_cat <- with(impute_long_mids,
                       coxph(formula = Surv(time_to_death_months, death_cancer)~ omega_ratiog + 
                                 age + TDI + eth_group_new + sex + center))

min_cat = summary(pool(fitimp_min_cat))
min_cat$exp = round(exp(min_cat$estimate),2)
min_cat$lw = round(exp(min_cat$estimate-1.96*min_cat$std.error),2)
min_cat$hi = round(exp(min_cat$estimate+1.96*min_cat$std.error),2)
min_cat[1:4,]

# Test for trend
fitimp_min_trd <- with(impute_long_mids,
                       coxph(formula = Surv(time_to_death_months, death_cancer)~ omegar_m + 
                                 age + TDI + eth_group_new + sex + center))

summary(pool(fitimp_min_trd))



#### Fully adjusted

# Continuous
fitimp_ful_con <- with(impute_long_mids,
                       coxph(formula = Surv(time_to_death_months, death_cancer)~ omega_ratio + 
                                 age + TDI + eth_group_new + sex + center  + BMI + Smoking_status + Alcohol_status + IPAQ_activity + comorb))

ful_con = summary(pool(fitimp_ful_con))
ful_con$exp = round(exp(ful_con$estimate),2)
ful_con$lw = round(exp(ful_con$estimate-1.96*ful_con$std.error),2)
ful_con$hi = round(exp(ful_con$estimate+1.96*ful_con$std.error),2)
ful_con[1,]
# Category
fitimp_ful_cat <- with(impute_long_mids,
                       coxph(formula = Surv(time_to_death_months, death_cancer)~ omega_ratiog + 
                                 age + TDI + eth_group_new + sex + center  + BMI + Smoking_status + Alcohol_status + IPAQ_activity + comorb))

ful_cat = summary(pool(fitimp_ful_cat))
ful_cat$exp = round(exp(ful_cat$estimate),2)
ful_cat$lw = round(exp(ful_cat$estimate-1.96*ful_cat$std.error),2)
ful_cat$hi = round(exp(ful_cat$estimate+1.96*ful_cat$std.error),2)
ful_cat[1:4,]

# Test for trend
fitimp_ful_trd <- with(impute_long_mids,
                       coxph(formula = Surv(time_to_death_months, death_cancer)~ omegar_m + 
                                 age + TDI + eth_group_new + sex + center +  BMI + Smoking_status + Alcohol_status + IPAQ_activity + comorb))

summary(pool(fitimp_ful_trd))



## cvd

#### Minimum adjusted

# Continuous
fitimp_min_con <- with(impute_long_mids,
                       coxph(formula = Surv(time_to_death_months, death_cvd)~ omega_ratio + 
                                 age + TDI + eth_group_new + sex + center))

min_con = summary(pool(fitimp_min_con))
min_con$exp = round(exp(min_con$estimate),2)
min_con$lw = round(exp(min_con$estimate-1.96*min_con$std.error),2)
min_con$hi = round(exp(min_con$estimate+1.96*min_con$std.error),2)
min_con[1,]
# Category
fitimp_min_cat <- with(impute_long_mids,
                       coxph(formula = Surv(time_to_death_months, death_cvd)~ omega_ratiog + 
                                 age + TDI + eth_group_new + sex + center))

min_cat = summary(pool(fitimp_min_cat))
min_cat$exp = round(exp(min_cat$estimate),2)
min_cat$lw = round(exp(min_cat$estimate-1.96*min_cat$std.error),2)
min_cat$hi = round(exp(min_cat$estimate+1.96*min_cat$std.error),2)
min_cat[1:4,]
# Test for trend
fitimp_min_trd <- with(impute_long_mids,
                       coxph(formula = Surv(time_to_death_months, death_cvd)~ omegar_m + 
                                 age + TDI + eth_group_new + sex + center))

summary(pool(fitimp_min_trd))



#### Fully adjusted

# Continuous
fitimp_ful_con <- with(impute_long_mids,
                       coxph(formula = Surv(time_to_death_months, death_cvd)~ omega_ratio + 
                                 age + TDI + eth_group_new + sex + center  + BMI + Smoking_status + Alcohol_status + IPAQ_activity + comorb))

ful_con = summary(pool(fitimp_ful_con))
ful_con$exp = round(exp(ful_con$estimate),2)
ful_con$lw = round(exp(ful_con$estimate-1.96*ful_con$std.error),2)
ful_con$hi = round(exp(ful_con$estimate+1.96*ful_con$std.error),2)
ful_con[1,]

# Category
fitimp_ful_cat <- with(impute_long_mids,
                       coxph(formula = Surv(time_to_death_months, death_cvd)~ omega_ratiog + 
                                 age + TDI + eth_group_new + sex + center  + BMI + Smoking_status + Alcohol_status + IPAQ_activity + comorb))

ful_cat = summary(pool(fitimp_ful_cat))
ful_cat$exp = round(exp(ful_cat$estimate),2)
ful_cat$lw = round(exp(ful_cat$estimate-1.96*ful_cat$std.error),2)
ful_cat$hi = round(exp(ful_cat$estimate+1.96*ful_cat$std.error),2)
ful_cat[1:4,]

# Test for trend
fitimp_ful_trd <- with(impute_long_mids,
                       coxph(formula = Surv(time_to_death_months, death_cvd)~ omegar_m + 
                                 age + TDI + eth_group_new + sex + center +  BMI + Smoking_status + Alcohol_status + IPAQ_activity + comorb))

summary(pool(fitimp_ful_trd))



