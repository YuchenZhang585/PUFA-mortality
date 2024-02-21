# Library the required packages
library(dplyr)
library(tidyr)
library(tidyverse)
library(data.table)
# Read in the data
raw <- fread("/scratch/yz92460/ukb/omega_data/ukb671174.tab", header=TRUE, sep="\t")


## ----------------##
#### death data ####
## ----------------##
death_0321 = raw %>% select(f.eid, f.40000.0.0, f.40001.0.0) %>% rename(date_of_death = f.40000.0.0, cause_of_death_primary = f.40001.0.0)   
death_0321 = as_tibble(death_0321)  
# Save the data
write.csv(death_0321,file="/scratch/yz92460/ukb/omega_data/death_0321.csv",row.names = F)


## ----------------##
#### plasma PUFAs ####
## ----------------##

#PUFA_plas_0321 = raw %>% select(f.eid,f.23451.0.0,f.23452.0.0,f.23459.0.0, f.23456.0.0, f.23457.0.0) %>%
#    rename(ID = f.eid,
#           omega3_pct = f.23451.0.0,
#           omega6_pct = f.23452.0.0,
#           omega_ratio = f.23459.0.0,
#           LA_pct = f.23456.0.0,
#           DHA_pct = f.23457.0.0)
#PUFA_plas_0321 = as_tibble(PUFA_plas_0321) 
# Save the data
#write.csv(PUFA_plas_0321,file="/scratch/yz92460/ukb/omega_data/PUFA_plas_0321.csv",row.names = F)

## ----------------##
#### Dietary PUFAs ####
## ----------------##
diet_n3 = raw %>% select(f.eid, starts_with('f.26015')) %>% rename(ID = f.eid)
diet_n3 = as_tibble(diet_n3)

diet_n6 = raw %>% select(f.eid, starts_with('f.26016')) %>% rename(ID = f.eid)
diet_n6 = as_tibble(diet_n6)
# Save the data
write.csv(diet_n3,file="/scratch/yz92460/ukb/omega_data/diet_n3.csv",row.names = F)
write.csv(diet_n6,file="/scratch/yz92460/ukb/omega_data/diet_n6.csv",row.names = F)


## ----------------##
#### covariates ####
## ----------------##
covar = raw %>% select(f.eid, f.53.0.0, f.21003.0.0, f.31.0.0, f.21000.0.0, f.189.0.0, f.54.0.0) %>%
    rename(ID = f.eid,
           base_date = f.53.0.0,
           age = f.21003.0.0,
           sex = f.31.0.0,
           # 1 male; 0 female
           ethnicity = f.21000.0.0,
           TDI = f.189.0.0,
           center = f.54.0.0)   
covar = as_tibble(covar)    

# Read in the data
bmi <- fread("/scratch/yz92460/ukb/omega_data/ukb37330.tab", header=TRUE, sep="\t")
alcohol <- fread("/scratch/yz92460/ukb/omega_data/ukb34137.tab", header=TRUE, sep="\t")
act <- fread("/scratch/yz92460/ukb/omega_data/ukb43770.tab", header=TRUE, sep="\t")
smoke <- fread("/scratch/yz92460/ukb/omega_data/ukb40720.tab", header=TRUE, sep="\t")

# BMI 37330
new = bmi %>% select(f.eid, f.21001.0.0) %>% rename(ID = f.eid, BMI = f.21001.0.0)
# merge
covar = covar %>% left_join(new, by = "ID")
covar = as_tibble(covar)

# Alcohol_status 34137
new1 = alcohol %>% select(f.eid, f.20117.0.0) %>% rename(ID = f.eid, Alcohol_status = f.20117.0.0)   
new1 = as_tibble(new1) 
# merge
covar = covar %>% left_join(new1, by = "ID")
covar = as_tibble(covar)

# Smoking 40720
new2 =  smoke %>% select(f.eid,f.20116.0.0) %>% rename(ID = f.eid, Smoking_status = f.20116.0.0)
new2 = as_tibble(new2) 
# merge
covar = covar %>% left_join(new2, by = "ID")
covar = as_tibble(covar)

### activity
new3 = act %>% select(f.eid,f.22032.0.0) %>% rename(ID = f.eid, IPAQ_activity = f.22032.0.0)
new3 = as_tibble(new3) 
# merge
covar = covar %>% left_join(new3, by = "ID")
covar = as_tibble(covar)

### fish oil
fishoil_total_0210 <- read.delim("/scratch/yz92460/ukb/omega_data/UKB_pheno_cov_unQC_020522.txt", header=TRUE)
fishoil = fishoil_total_0210 %>% select(FID, Fish_oil_baseline) %>% rename(ID = FID)
fishoil = as_tibble(fishoil) 
# merge
covar = covar %>% left_join(fishoil, by = "ID")
covar = as_tibble(covar)

# create the ethnic group
covar = covar %>% 
    mutate(eth_group = ifelse(ethnicity %in% c(1,1001,2001,3001,4001),"White",
                              ifelse(ethnicity %in% c(2,1002,2002,3002,4002),"Mixed",
                                     ifelse(ethnicity %in% c(3,1003,2003,3003,4003),"Asian or Asian British",
                                            ifelse(ethnicity %in% c(4,2004,3004),"Black or Black British",
                                                   ifelse(ethnicity %in% c(5),"Chinese",
                                                          ifelse(ethnicity %in% c(6),"Other ethnic group",
                                                                 "DK/NR")))))))
# Combine the ethnicity
covar = covar %>% 
    mutate(eth_group_new = ifelse(eth_group == "White","White",
                                  ifelse(eth_group == "Black or Black British","Black",
                                         ifelse(eth_group %in% c("Asian or Asian British", "Chinese"),"Asian",
                                                ifelse(eth_group %in% c("Mixed", "Other ethnic group"),"Others",
                                                       NA)))))
loss = fread("/scratch/yz92460/ukb/omega_data/loss.csv", header=TRUE, sep=",")
# merge
covar = covar %>% left_join(loss, by = "ID")

covar$Alcohol_status[covar$Alcohol_status==-3] = NA
covar$Smoking_status[covar$Smoking_status==-3] = NA
# Save the data
write.csv(covar,file="/scratch/yz92460/ukb/omega_data/covar_0321.csv",row.names = F)


## ----------------##
#### history of disease ####
## ----------------##

## ----------------##
# History of CVD (using first occurrence data)
## ----------------##
first_date_CVD = raw %>% select(f.eid,  starts_with(c("f.131270", "f.131272", "f.131274", "f.131276", "f.131278", 
                                                      "f.131280", "f.131282", "f.131284", "f.131286", "f.131288", 
                                                      "f.131290", "f.131292", "f.131294", "f.131296", "f.131298", 
                                                      "f.131300", "f.131302", "f.131304", "f.131306", "f.131308",
                                                      "f.131310", "f.131312", "f.131314", "f.131316", "f.131318", 
                                                      "f.131320", "f.131322", "f.131324", "f.131326", "f.131328", 
                                                      "f.131330", "f.131332", "f.131334", "f.131336", "f.131338", 
                                                      "f.131340", "f.131342", "f.131344", "f.131346", "f.131348", 
                                                      "f.131350", "f.131352", "f.131354", "f.131356", "f.131358", 
                                                      "f.131360", "f.131362", "f.131364", "f.131366", "f.131368", 
                                                      "f.131370", "f.131372", "f.131374", "f.131376", "f.131378", 
                                                      "f.131380", "f.131382", "f.131384", "f.131386", "f.131388", 
                                                      "f.131390", "f.131392", "f.131394", "f.131396", "f.131398", 
                                                      "f.131400", "f.131402", "f.131404", "f.131406", "f.131408",
                                                      "f.131410", "f.131412", "f.131414", "f.131416", "f.131418",
                                                      "f.131420", "f.131422" ))) %>% rename(ID = f.eid)

#dates = first_date_CVD %>% select(-ID)
#date_CVD = first_date_CVD %>% drop_na(all_of(dates))
#date_CVD$date_min = apply(date_CVD[,-1], 1, min)
#date_CVD = date_CVD %>% select(ID, date_min)
# Save the data
write.csv(first_date_CVD,file="/scratch/yz92460/ukb/omega_data/first_date_CVD.csv",row.names = F)

# History of cancer
# use date_cancer.csv
# Combine with base_date to select

## ----------------##
# comorbidities
## ----------------##
self_hyper = raw %>% select(f.eid, f.20002.0.0) %>% rename(ID = f.eid, hyperten = f.20002.0.0) %>% filter(hyperten == 1065)
self_diabete = raw %>% select(f.eid, f.2443.0.0) %>% rename(ID = f.eid, diabet = f.2443.0.0)
self_ill = raw %>% select(f.eid, f.2188.0.0) %>% rename(ID = f.eid, ill = f.2188.0.0)
# Save the data
write.csv(self_hyper,file="/scratch/yz92460/ukb/omega_data/self_hyper.csv",row.names = F)
write.csv(self_diabete,file="/scratch/yz92460/ukb/omega_data/self_diabete.csv",row.names = F)
write.csv(self_ill,file="/scratch/yz92460/ukb/omega_data/self_ill.csv",row.names = F)

## ----------------##
#### Biomarkers ####
## ----------------##

biomark = raw %>% select(f.eid, f.30690.0.0, f.30780.0.0, f.30760.0.0, f.30870.0.0, f.30630.0.0, f.30640.0.0, 
                         f.30710.0.0, f.30790.0.0, f.30830.0.0, f.30850.0.0, f.30800.0.0, f.30770.0.0) %>%
    rename(ID = f.eid,
           chole = f.30690.0.0,
           DLD = f.30780.0.0,
           HDLC = f.30760.0.0,
           Trigly = f.30870.0.0,
           ApoA = f.30630.0.0,
           ApoB = f.30640.0.0,
           Cpro = f.30710.0.0,
           LipA = f.30790.0.0,
           SHBG = f.30830.0.0,
           Testo = f.30850.0.0,
           Oest = f.30800.0.0,
           IGF = f.30770.0.0)   
biomark = as_tibble(biomark)    
# Save the data
write.csv(biomark,file="/scratch/yz92460/ukb/omega_data/biomark.csv",row.names = F)
