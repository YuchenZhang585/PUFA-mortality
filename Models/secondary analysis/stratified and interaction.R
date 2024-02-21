### All covariates and data combined ###
### 03-09-2022 ###

library(dplyr)
library(tidyr)
library(tidyverse)
library(data.table)
library(survival)
library(Hmisc)
library(haven)
library(survival)		#Survival analysis
library(survminer) 		#Survival analysis
library(rms)
library(survMisc)
library(splines)
library(Greg)

## All data of omega-ratio (data_1208)
data_1208 <- fread("/Users/yuchen/Desktop/UK Biobank/Data/data_1208.csv", header=TRUE, sep=",")

#######################################
## All data of omega-3/6 (data_omega_0210)
data_omega_0210 <- fread("/Users/yuchen/Desktop/UK Biobank/Data/data_omega_0210.csv", header=TRUE, sep=",")
data_omega_0210 = data_omega_0210 %>% select(-omega_ratio)

#######################################
## All data of fish oil (data_fish_0210)
data_fish_0210 <- fread("/Users/yuchen/Desktop/UK Biobank/Data/data_fish_0210.csv", header=TRUE, sep=",")

# Merge 3 datasets
merge1 <- merge(data_fish_0210, data_1208, all=TRUE)
total_data = merge(merge1, data_omega_0210, all=TRUE)
write.csv(total_data,file="/Users/yuchen/Desktop/UK Biobank/Data/total_data.csv",row.names = F)
# total data that has omega_ratio (exposure)
new_mydata = total_data %>% filter(!is.na(omega_ratio))



# total data death
total_death = total_data %>% filter(!is.na(date_of_death))
total_death$time_to_death = as.numeric(difftime(total_death$date_of_death, total_death$base_date)) # in days
total_death$time_to_death_months = total_death$time_to_death/30.417
write.csv(total_death,file="/Users/yuchen/Desktop/UK Biobank/Data/total_death.csv",row.names = F)

## total data death with cause

# filter the patients with primary death cause information
total_death_cause = total_death %>% filter(!is.na(cause_of_death_primary))

# Get the group of primary death cause information
icd_initial = substr(total_death_cause$cause_of_death_primary,1,1)
icd_num2 = substr(total_death_cause$cause_of_death_primary,2,3)
total_death_cause = total_death_cause %>% mutate(icd_initial) %>% mutate(icd_num2)
total_death_cause$icd_num2 = as.numeric(total_death_cause$icd_num2)

total_death_cause = total_death_cause %>% 
    mutate(death_cause_group = ifelse(icd_initial %in% c("A","B"),"01_infectious_parasitic",
                                      ifelse(icd_initial == "C","02_Neoplasms",
                                             # later I will modify D10-48 to 02_Neoplasms group
                                             ifelse(icd_initial == "D","03_blood",
                                                    ifelse(icd_initial == "E","04_Endocrine_nutrition",
                                                           ifelse(icd_initial == "F","05_Mental",
                                                                  ifelse(icd_initial == "G","06_nervous",
                                                                         ifelse(icd_initial == "H","08_ear",
                                                                                ifelse(icd_initial == "I","09_circulatory",
                                                                                       ifelse(icd_initial == "J","10_respiratory",
                                                                                              ifelse(icd_initial == "K","11_digestive",
                                                                                                     ifelse(icd_initial == "L","12_skin",
                                                                                                            ifelse(icd_initial == "M","13_musculoskeletal",
                                                                                                                   ifelse(icd_initial == "N","14_genitourinary",
                                                                                                                          ifelse(icd_initial == "O","15_Pregnancy",
                                                                                                                                 ifelse(icd_initial == "Q","17_Congenital",
                                                                                                                                        ifelse(icd_initial == "R","18_abnormal clinical",
                                                                                                                                               ifelse(icd_initial %in% c("V","W","X","Y"),"20_External causes",
                                                                                                                                                      ifelse(icd_initial == "U","22_special_purposes",NA
                                                                                                                                                      )))))))))))))))))))
# Change D10-48 to 02 group
total_death_cause = total_death_cause %>% mutate(death_cause_group = ifelse(icd_initial == "D" & icd_num2 > 9 & icd_num2 < 49,"02_Neoplasms",death_cause_group))
#write.csv(total_death_cause,file="/Users/yuchen/Desktop/UK Biobank/total_death_cause.csv",row.names = F)
write.csv(total_death_cause,file="/Users/yuchen/Desktop/UK Biobank/Data/total_death_cause.csv",row.names = F)

##### Interaction analysis for omega_ratio*fish_oil baseline

# Firstly filter the patients with exposure (omega_ratio)
total_omega = total_omega %>% mutate(date_of_death = ifelse(is.na(date_of_death) == TRUE, as.Date("2018-02-08"), date_of_death))
total_omega$date_of_death = as.Date(total_omega$date_of_death, origin = "1970-01-01")
total_omega = total_omega %>% mutate(time_to_death = as.numeric(difftime(total_omega$date_of_death, total_omega$base_date))) 
total_omega = total_omega %>% mutate(time_to_death_months = time_to_death/30.417)  
total_omega = total_omega %>% mutate(icd_initial = substr(cause_of_death_primary,1,1))
total_omega = total_omega %>% mutate(icd_num2 = substr(cause_of_death_primary,2,3))
total_omega$icd_num2 = as.numeric(total_omega$icd_num2)
total_omega = total_omega %>% 
    mutate(death_cause_group = ifelse(icd_initial %in% c("A","B"),"01_infectious_parasitic",
                                      ifelse(icd_initial == "C","02_Neoplasms",
                                             # later I will modify D10-48 to 02_Neoplasms group
                                             ifelse(icd_initial == "D","03_blood",
                                                    ifelse(icd_initial == "E","04_Endocrine_nutrition",
                                                           ifelse(icd_initial == "F","05_Mental",
                                                                  ifelse(icd_initial == "G","06_nervous",
                                                                         ifelse(icd_initial == "H","08_ear",
                                                                                ifelse(icd_initial == "I","09_circulatory",
                                                                                       ifelse(icd_initial == "J","10_respiratory",
                                                                                              ifelse(icd_initial == "K","11_digestive",
                                                                                                     ifelse(icd_initial == "L","12_skin",
                                                                                                            ifelse(icd_initial == "M","13_musculoskeletal",
                                                                                                                   ifelse(icd_initial == "N","14_genitourinary",
                                                                                                                          ifelse(icd_initial == "O","15_Pregnancy",
                                                                                                                                 ifelse(icd_initial == "Q","17_Congenital",
                                                                                                                                        ifelse(icd_initial == "R","18_abnormal clinical",
                                                                                                                                               ifelse(icd_initial %in% c("V","W","X","Y"),"20_External causes",
                                                                                                                                                      ifelse(icd_initial == "U","22_special_purposes",NA
                                                                                                                                                      )))))))))))))))))))
# Change D10-48 to 02 group
total_omega = total_omega %>% mutate(death_cause_group = ifelse(icd_initial == "D" & icd_num2 > 9 & icd_num2 < 49,"02_Neoplasms",death_cause_group))

total_omega = total_omega %>% mutate(death = ifelse(is.na(cause_of_death_primary) == TRUE, 0, 1)) %>% mutate(death_neop = ifelse(death_cause_group == "02_Neoplasms", 1, 0)) %>% mutate(death_circ = ifelse(death_cause_group == "09_circulatory", 1, 0)) 
total_omega$death[is.na(total_omega$death)==TRUE] = 0
total_omega$death_neop[is.na(total_omega$death_neop)==TRUE] = 0
total_omega$death_circ[is.na(total_omega$death_circ)==TRUE] = 0
write.csv(total_omega,file="/Users/yuchen/Desktop/UK Biobank/Data/total_omega.csv",row.names = F)

## Stratified and interaction analysis ##
### --------------------------------------------------------------------------###
total_omega = total_omega %>% mutate(age_group = ifelse(age < 58, 1, 2)) 
total_omega = total_omega %>% mutate(TDI_group = ifelse(TDI < -2, 1, 2)) 
total_omega = total_omega %>% mutate(ratio_group = ifelse(omega_ratio<7.139, 1, 
                         ifelse(omega_ratio<8.990, 2,
                                ifelse(omega_ratio<11.464, 3, 4))))
total_omega$ratio_factor = as.factor(total_omega$ratio_group)
sum = total_omega %>% group_by(ratio_group) %>% summarise(Median=median(omega_ratio), Mean=mean(omega_ratio))
total_omega = total_omega %>% mutate(ratio_trend = ifelse(ratio_group == 1, 6.05, 
                                                          ifelse(ratio_group == 2, 8.07,
                                                                 ifelse(ratio_group ==3 , 10.0 , 13.8))))
total_omega$center = as.factor(total_omega$center)
total_omega$ratio_factor = as.factor(total_omega$ratio_factor)
total_omega$sex = as.factor(total_omega$sex)
total_omega$age_group = as.factor(total_omega$age_group)
total_omega$TDI_group = as.factor(total_omega$TDI_group)
total_omega$Fish_oil_baseline = as.factor(total_omega$Fish_oil_baseline)


############################ Codes 0406 ############################
new_mydata <- fread("/Users/yuchen/Desktop/UK Biobank/Data/new_mydata.csv", header=TRUE, sep=",")

new_mydata = new_mydata %>% mutate(BMI_group = ifelse(BMI < 25, 1, 2)) 
new_mydata = new_mydata %>% mutate(smoke_group = ifelse(Smoking_status  == -3 | Smoking_status  == 0, 0, 1)) 
new_mydata = new_mydata %>% mutate(alc_group = ifelse(Alcohol_status  == -3 | Alcohol_status  == 0, 0, 1)) 
new_mydata = new_mydata %>% mutate(physical_group = ifelse(IPAQ_activity  == "High", 1, ifelse(IPAQ_activity  == "Low" | IPAQ_activity  == "Moderate", 0, NA))) 

new_mydata$center = as.factor(new_mydata$center)
new_mydata$ratio_group_5f = as.factor(new_mydata$ratio_group_5f)
new_mydata$sex = as.factor(new_mydata$sex)
new_mydata$age_group = as.factor(new_mydata$age_group)
new_mydata$TDI_group = as.factor(new_mydata$TDI_group)
new_mydata$Fish_oil_baseline = as.factor(new_mydata$Fish_oil_baseline)
new_mydata$Alcohol_status = as.factor(new_mydata$Alcohol_status)
new_mydata$Smoking_status = as.factor(new_mydata$Smoking_status)
new_mydata$Alcohol_status = relevel(new_mydata$Alcohol_status, ref="2")
new_mydata$Smoking_status = relevel(new_mydata$Smoking_status, ref="2")
new_mydata = new_mydata %>% mutate(ratio_trend_5 = ifelse(ratio_group_5 == 1, 5.78, 
                                                          ifelse(ratio_group_5 == 2, 7.51,
                                                                 ifelse(ratio_group_5 ==3 ,8.99,
                                                                        ifelse(ratio_group_5 ==4 , 10.8 , 14.6)))))



## Age
age1 = new_mydata %>% filter(age_group == 1)
age2 = new_mydata %>% filter(age_group == 2)

### Age 1
# -- stratified (as quartiles)
summary(coxph(formula = Surv(time_to_death_months, death_circ)~ ratio_group_5f + TDI + eth_group_new + 
                  sex + center  + BMI + Smoking_status + Alcohol_status + IPAQ_activity, data = age1))
# P for trend
summary(coxph(formula = Surv(time_to_death_months, death_circ)~ ratio_trend_5 +  TDI + eth_group_new + 
                  sex + center  + BMI + Smoking_status + Alcohol_status + IPAQ_activity, data = age1))

### Age 2
# -- stratified (as quartiles)
summary(coxph(formula = Surv(time_to_death_months, death_circ)~ ratio_group_5f + TDI + eth_group_new + 
                  sex + center  + BMI + Smoking_status + Alcohol_status + IPAQ_activity, data = age2))
# P for trend
summary(coxph(formula = Surv(time_to_death_months, death_circ)~ ratio_trend_5 + TDI + eth_group_new + 
                  sex + center  + BMI + Smoking_status + Alcohol_status + IPAQ_activity, data = age2))

# P for interaction
fit1 = coxph(formula = Surv(time_to_death_months, death_circ)~ ratio_group_5f*age_group + TDI + eth_group_new + 
                 sex + center  + BMI + Smoking_status + Alcohol_status + IPAQ_activity , data = new_mydata)

fit2 = coxph(formula = Surv(time_to_death_months, death_circ)~  ratio_group_5f  + age_group + TDI + eth_group_new + 
                 sex + center  + BMI + Smoking_status + Alcohol_status + IPAQ_activity , data = new_mydata)
anova(fit2, fit1)

# P for continuous (interaction)
fit3 = coxph(formula = Surv(time_to_death_months, death_circ)~ omega_ratio*age + TDI + eth_group_new + 
                 sex + center  + BMI + Smoking_status + Alcohol_status + IPAQ_activity , data = new_mydata)

fit4 = coxph(formula = Surv(time_to_death_months, death_circ)~ omega_ratio + age +  TDI + eth_group_new + 
                 sex + center  + BMI + Smoking_status + Alcohol_status + IPAQ_activity , data = new_mydata)
anova(fit3, fit4)

## Gender
male = new_mydata %>% filter(sex == 1)
female = new_mydata %>% filter(sex == 0)

## Male
# -- stratified (as quartiles)
summary(coxph(formula = Surv(time_to_death_months, death_circ)~ ratio_group_5f + age + TDI + eth_group_new + 
                   center  + BMI + Smoking_status + Alcohol_status + IPAQ_activity, data = male))
# P for trend
summary(coxph(formula = Surv(time_to_death_months, death_circ)~ ratio_trend_5 + age + TDI + eth_group_new + 
                  center  + BMI + Smoking_status + Alcohol_status + IPAQ_activity, data = male))

## Female
# -- stratified (as quartiles)
summary(coxph(formula = Surv(time_to_death_months, death_circ)~ ratio_group_5f + age + TDI + eth_group_new + 
                  center  + BMI + Smoking_status + Alcohol_status + IPAQ_activity, data = female))
# P for trend
summary(coxph(formula = Surv(time_to_death_months, death_circ)~ ratio_trend_5 + age + TDI + eth_group_new + 
                  center  + BMI + Smoking_status + Alcohol_status + IPAQ_activity, data = female))

# P for interaction
fit1 = coxph(formula = Surv(time_to_death_months, death_circ)~ ratio_group_5f*sex + age + TDI + eth_group_new + center  +
                 BMI + Smoking_status + Alcohol_status + IPAQ_activity,  data = new_mydata)
fit2 = coxph(formula = Surv(time_to_death_months, death_circ)~ ratio_group_5f + sex + age + TDI + eth_group_new + center +
                 BMI + Smoking_status + Alcohol_status + IPAQ_activity, data = new_mydata)
anova(fit2, fit1)



## TDI
tdi1 = new_mydata %>% filter(TDI_group == 1)
tdi2 = new_mydata %>% filter(TDI_group == 2)

### TDI 1
# -- stratified (as quartiles)
summary(coxph(formula = Surv(time_to_death_months, death_circ)~ ratio_group_5f + age + eth_group_new + 
                  sex + center  + BMI + Smoking_status + Alcohol_status + IPAQ_activity, data = tdi1))
# P for trend
summary(coxph(formula = Surv(time_to_death_months, death_circ)~ ratio_trend_5 +  age + eth_group_new + 
                  sex + center  + BMI + Smoking_status + Alcohol_status + IPAQ_activity, data = tdi1))

### TDI 2
# -- stratified (as quartiles)
summary(coxph(formula = Surv(time_to_death_months, death_circ)~ ratio_group_5f + age + eth_group_new + 
                  sex + center  + BMI + Smoking_status + Alcohol_status + IPAQ_activity, data = tdi2))
# P for trend
summary(coxph(formula = Surv(time_to_death_months, death_circ)~ ratio_trend_5 + age + eth_group_new + 
                  sex + center  + BMI + Smoking_status + Alcohol_status + IPAQ_activity, data = tdi2))

# P for interaction
fit1 = coxph(formula = Surv(time_to_death_months, death_circ)~ ratio_group_5f*TDI_group + age + eth_group_new + 
                 sex + center  + BMI + Smoking_status + Alcohol_status + IPAQ_activity , data = new_mydata)

fit2 = coxph(formula = Surv(time_to_death_months, death_circ)~  ratio_group_5f  + TDI_group + age + eth_group_new + 
                 sex + center  + BMI + Smoking_status + Alcohol_status + IPAQ_activity , data = new_mydata)
anova(fit2, fit1)

# P for continuous (interaction)
fit3 = coxph(formula = Surv(time_to_death_months, death_circ)~ omega_ratio*TDI + age + eth_group_new + 
                 sex + center  + BMI + Smoking_status + Alcohol_status + IPAQ_activity , data = new_mydata)

fit4 = coxph(formula = Surv(time_to_death_months, death_circ)~ omega_ratio + TDI +  age + eth_group_new + 
                 sex + center  + BMI + Smoking_status + Alcohol_status + IPAQ_activity , data = new_mydata)
anova(fit3, fit4)



## BMI
bmi1 = new_mydata %>% filter(BMI_group == 1)
bmi2 = new_mydata %>% filter(BMI_group == 2)

### BMI 1
# -- stratified (as quartiles)
summary(coxph(formula = Surv(time_to_death_months, death_circ)~ ratio_group_5f + TDI + eth_group_new + 
                  sex + center  + age + Smoking_status + Alcohol_status + IPAQ_activity, data = bmi1))
# P for trend
summary(coxph(formula = Surv(time_to_death_months, death_circ)~ ratio_trend_5 +  TDI + eth_group_new + 
                  sex + center  + age + Smoking_status + Alcohol_status + IPAQ_activity, data = bmi1))

### BMI 2
# -- stratified (as quartiles)
summary(coxph(formula = Surv(time_to_death_months, death_circ)~ ratio_group_5f + TDI + eth_group_new + 
                  sex + center + age + Smoking_status + Alcohol_status + IPAQ_activity, data = bmi2))
# P for trend
summary(coxph(formula = Surv(time_to_death_months, death_circ)~ ratio_trend_5 + TDI + eth_group_new + 
                  sex + center  + age + Smoking_status + Alcohol_status + IPAQ_activity, data = bmi2))

# P for interaction
fit1 = coxph(formula = Surv(time_to_death_months, death_circ)~ ratio_group_5f*BMI_group + TDI + eth_group_new + 
                 sex + center  + age + Smoking_status + Alcohol_status + IPAQ_activity , data = new_mydata)

fit2 = coxph(formula = Surv(time_to_death_months, death_circ)~  ratio_group_5f  + BMI_group + TDI + eth_group_new + 
                 sex + center  + age + Smoking_status + Alcohol_status + IPAQ_activity , data = new_mydata)
anova(fit2, fit1)

# P for continuous (interaction)
fit3 = coxph(formula = Surv(time_to_death_months, death_circ)~ omega_ratio*BMI + TDI + eth_group_new + 
                 sex + center  + age + Smoking_status + Alcohol_status + IPAQ_activity , data = new_mydata)

fit4 = coxph(formula = Surv(time_to_death_months, death_circ)~ omega_ratio + BMI +  TDI + eth_group_new + 
                 sex + center  + age + Smoking_status + Alcohol_status + IPAQ_activity , data = new_mydata)
anova(fit3, fit4)


## Smoke
smoke_yes = new_mydata %>% filter(smoke_group == 1)
smoke_no = new_mydata %>% filter(smoke_group == 0)

### yes
# -- stratified (as quartiles)
summary(coxph(formula = Surv(time_to_death_months, death_circ)~ ratio_group_5f + age + eth_group_new + 
                  sex + center + TDI + BMI + Alcohol_status + IPAQ_activity, data = smoke_yes))
# P for trend
summary(coxph(formula = Surv(time_to_death_months, death_circ)~ ratio_trend_5 +  age + eth_group_new + 
                  sex + center + TDI + BMI +  Alcohol_status + IPAQ_activity, data = smoke_yes))

### no
# -- stratified (as quartiles)
summary(coxph(formula = Surv(time_to_death_months, death_circ)~ ratio_group_5f + age + eth_group_new + 
                  sex + center + TDI + BMI +  Alcohol_status + IPAQ_activity, data = smoke_no))
# P for trend
summary(coxph(formula = Surv(time_to_death_months, death_circ)~ ratio_trend_5 + age + eth_group_new + 
                  sex + center + TDI + BMI +  Alcohol_status + IPAQ_activity, data = smoke_no))

# P for interaction
fit1 = coxph(formula = Surv(time_to_death_months, death_circ)~ ratio_group_5f*smoke_group + age + eth_group_new + 
                 sex + center + TDI + BMI + Alcohol_status + IPAQ_activity , data = new_mydata)

fit2 = coxph(formula = Surv(time_to_death_months, death_circ)~  ratio_group_5f  + smoke_group + age + eth_group_new + 
                 sex + center + TDI + BMI +  Alcohol_status + IPAQ_activity , data = new_mydata)
anova(fit2, fit1)



## physical activity

phy_low = new_mydata %>% filter(physical_group == 0)
phy_high = new_mydata %>% filter(physical_group == 1)

### low
# -- stratified (as quartiles)
summary(coxph(formula = Surv(time_to_death_months, death_circ)~ ratio_group_5f + age + eth_group_new + 
                  sex + center + TDI + BMI + Smoking_status + Alcohol_status, data = phy_low))
# P for trend
summary(coxph(formula = Surv(time_to_death_months, death_circ)~ ratio_trend_5 +  age + eth_group_new + 
                  sex + center + TDI + BMI +  Smoking_status + Alcohol_status, data = phy_low))

### high
# -- stratified (as quartiles)
summary(coxph(formula = Surv(time_to_death_months, death_circ)~ ratio_group_5f + age + eth_group_new + 
                  sex + center + TDI + BMI +  Smoking_status + Alcohol_status, data = phy_high))
# P for trend
summary(coxph(formula = Surv(time_to_death_months, death_circ)~ ratio_trend_5 + age + eth_group_new + 
                  sex + center + TDI + BMI +  Smoking_status + Alcohol_status , data = phy_high))

# P for interaction
fit1 = coxph(formula = Surv(time_to_death_months, death_circ)~ ratio_group_5f*physical_group + age + eth_group_new + 
                 sex + center + TDI + BMI + Smoking_status + Alcohol_status , data = new_mydata)

fit2 = coxph(formula = Surv(time_to_death_months, death_circ)~  ratio_group_5f  + physical_group + age + eth_group_new + 
                 sex + center + TDI + BMI +  Smoking_status + Alcohol_status  , data = new_mydata)
anova(fit2, fit1)


## Alcohol
alc_yes = new_mydata %>% filter(alc_group == 1)
alc_no = new_mydata %>% filter(alc_group == 0)

### yes
# -- stratified (as quartiles)
summary(coxph(formula = Surv(time_to_death_months, death_circ)~ ratio_group_5f + age + eth_group_new + 
                  sex + center + TDI + BMI + Smoking_status + IPAQ_activity, data = alc_yes))
# P for trend
summary(coxph(formula = Surv(time_to_death_months, death_circ)~ ratio_trend_5 +  age + eth_group_new + 
                  sex + center + TDI + BMI +  Smoking_status + IPAQ_activity, data = alc_yes))

### no
# -- stratified (as quartiles)
summary(coxph(formula = Surv(time_to_death_months, death_circ)~ ratio_group_5f + age + eth_group_new + 
                  sex + center + TDI + BMI +  Smoking_status + IPAQ_activity, data = alc_no))
# P for trend
summary(coxph(formula = Surv(time_to_death_months, death_circ)~ ratio_trend_5 + age + eth_group_new + 
                  sex + center + TDI + BMI +  Smoking_status + IPAQ_activity, data = alc_no))

# P for interaction
fit1 = coxph(formula = Surv(time_to_death_months, death_circ)~ ratio_group_5f*alc_group + age + eth_group_new + 
                 sex + center + TDI + BMI + Smoking_status + IPAQ_activity , data = new_mydata)

fit2 = coxph(formula = Surv(time_to_death_months, death_circ)~  ratio_group_5f  + alc_group + age + eth_group_new + 
                 sex + center + TDI + BMI +  Smoking_status + IPAQ_activity , data = new_mydata)
anova(fit2, fit1)

