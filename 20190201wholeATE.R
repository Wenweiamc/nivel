
###########################
# %% install libraries
###########################
# install.packages('glmnet')
# install.packages('csvread')

# install.packages('MASS')
# install.packages('leaps')
# install.packages('grid')
# install.packages('gridExtra')
# install.packages('dplyr')
# install.packages('MatchIt')
# install.packages('gmodels')
# install.packages('tableone')
# install.packages('ggplot2')
# install.packages('survey')
# install.packages('twang')
# install.packages('summarytools')
#  install.packages('Matching')
# install.packages('reshape2')
# install.packages('stddiff')
# install.packages('randomForestSRC')
# install.packages('subgroup.discovery')

###########################
# %% load libraries
###########################
library("installr")
library(readr)
library('glmnet')
library('csvread')
library('MASS')
library('leaps')
library('grid')
library('gridExtra')
library('dplyr')
library('MatchIt')
library('gmodels')
library('tableone')
library('ggplot2')
library('survey')
library('twang')
library('summarytools')
library('Matching')
library('reshape2')
library('stddiff')
library('randomForestSRC')
library('subgroup.discovery')
library(tableone)
library(sandwich)
library(survey)
library("Hmisc")
library("rms")
library(subgroup.discovery)
library(mice)
library(miceadds)
library(lubridate)
library(testthat)
#updateR()
###########################
# %% setwd
###########################
#setwd("C:/Users/mcschut/Documents/wip/chianti/data/processed/180523/")
#setwd("F:/åå£«/pumc/è¯¾é¢ç»/AMC/Utrecht/NIVEL/Thamar/20180823/")
setwd("H:/qww/AMC/Utrecht/NIVEL/Thamar/20180823/")

setwd("P:/NZR/NZR-001/Innovatieve Onderzoeksmethoden/2. Machine learning antibiotica bij ontstekingen/03. Dataverwerking/4. input R")
setwd("P:/NZR/NZR-001/Innovatieve Onderzoeksmethoden/2. Machine learning antibiotica bij ontstekingen/03. Dataverwerking/6. output R/20190125wqi/")
###########################
# %% read data and convert variables
###########################

# read in data as factor per default
#data_first_treatment <-read.csv(file = "dummyfile_firsttreatment.csv", na.strings=c("NA","NaN", " ",""), colClasses="factor")
#data_first_treatment<-read.csv(file = "F:/åå£«/pumc/è¯¾é¢ç»/AMC/Utrecht/NIVEL/Thamar/20180523/dummyfile_firsttreatment.csv", na.strings=c("NA","NaN", " ",""), colClasses="factor")


data_first_treatment <-read.csv(file = "researchfile_firsttreatment.csv", na.strings=c("NA","NaN", " ",""), colClasses="factor")

str(data_first_treatment)

# convert selected variables to numeric
cols.num <- c("age","nr_chron3", 'practice_size', 'nr_medication','nr_contacts_infection','nr_prescriptions_AB' ,'nr_contacts_resp',"days_prev_cont","CRP_values")

data_first_treatment[cols.num] <- sapply(data_first_treatment[cols.num],as.numeric)
data_first_treatment$date <- dmy(data_first_treatment$date)
str(data_first_treatment)
###date

hist(data_first_treatment$ days_prev_cont)
hist(data_first_treatment$age)
mean((data_first_treatment$age)<18)
mean((data_first_treatment$age)<10)

nrow(subset(data_first_treatment,outcome_2==1 & outcome_4==0))/nrow(subset(data_first_treatment,outcome_4==0))

mean(month(data_first_treatment$date)==12)
mean(month(data_first_treatment$date)<4)

data_first_treatment_12ow <- mutate(data_first_treatment, 
                                    agecat71018 = cut(data_first_treatment$age, c(0,7,10,18,81)))

data_first_treatment_12ow <- mutate(data_first_treatment, 
                                    agecat1=cut(data_first_treatment$age, seq(0,80,10),labels=c(1:8)))

summary(data_first_treatment_12ow$agecat1)
table(data_first_treatment_12ow$agecat1, data_first_treatment_12ow$AB_nose_infection)
summary(data_first_treatment_12ow$agecat71018)
table(data_first_treatment_12ow$agecat71018, data_first_treatment_12ow$AB_nose_infection)


levels(data_first_treatment$type) <- list(
  no = c(0),
  amoxicillin = c(1,2),
  macrolide  = c(3),
  doxycycline = c(4),
  others= c(5)
)
table(data_first_treatment$type)
table(data_first_treatment$type_AB_nose)


data_first_treatment_12ow <- mutate(data_first_treatment, 
                                    outcome_weight=ifelse(month(date)==12 & is.na(outcome_4),(31-day(data_first_treatment$date))/28, 1))

data_first_treatment_12ow <- mutate(data_first_treatment, 
                                    ID=seq.int(nrow(data_first_treatment_12ow)), 
                                    day=as.numeric(date - dmy(01012014)),
                                    month = month(date),
                                    flu_epi=as.factor(ifelse(week(date) %in% c(5:8, 10,11,49:53),1,0)),
                                    agecat1=cut(data_first_treatment$age, seq(0,80,10),labels=c(1:8)),
                                    agecat71018 = cut(data_first_treatment$age, c(0,7,10,18,81), right=FALSE),
                                    outcome_weight=ifelse(month(date)==12 & is.na(outcome_4),(31-day(data_first_treatment$date))/28, 1),
                                    outcome4_weighted=ifelse(is.na(outcome_4),outcome_weight, as.numeric(as.character(outcome_4))))

mean(data_first_treatment_12ow$outcome4_weighted)
#0.7696771

data_first_treatment_12ow$season[month(data_first_treatment_12ow$date) %in% c(3:5)] <- "spring"
data_first_treatment_12ow$season[month(data_first_treatment_12ow$date) %in% c(6:8)] <- "summer"
data_first_treatment_12ow$season[month(data_first_treatment_12ow$date) %in% c(9:11)] <- "autumn"
data_first_treatment_12ow$season[month(data_first_treatment_12ow$date) %in% c(1,2,12)] <- "winter"
data_first_treatment_12ow$season <- factor(data_first_treatment_12ow$season, ordered = TRUE, levels = c("spring", "summer", "autumn", "winter"))

str(data_first_treatment_12ow$season)
# AB type 2 distribution in a year especially in the first 3 months

table(data_first_treatment_12ow$type_AB_nose, data_first_treatment_12ow$month)
chisq.test(data_first_treatment_12ow$type_AB_nose, data_first_treatment_12ow$month, correct=FALSE)

chisq.test(data_first_treatment_12ow$type_AB_nose==2, data_first_treatment_12ow$month, correct=FALSE)

chisq.test(data_first_treatment_12ow$type_AB_nose==1, data_first_treatment_12ow$month, correct=FALSE)


#plot(AB_nose_infection ~ date, data_first_treatment_12ow)
#plot(type_AB_nose == 2 ~ date, data_first_treatment_12ow)


str(data_first_treatment)
typepractice <- table(data_first_treatment$type_AB_nose, data_first_treatment$nr_practice)
write.table(typepractice, file="type in practice.csv", sep = ",")

chisq.test(data_first_treatment_12ow$type_AB_nose, data_first_treatment$nr_practice, correct=FALSE)

chisq.test(data_first_treatment_12ow$type_AB_nose==4, data_first_treatment$nr_practice, correct=FALSE)




#####################
setwd("P:/NZR/NZR-001/Innovatieve Onderzoeksmethoden/2. Machine learning antibiotica bij ontstekingen/03. Dataverwerking/6. output R/20190201wqi/")
### Provide descriptives of all variables
z<-dfSummary(data_first_treatment_12ow)
write.table(z, file="descriptives_raw_data_first_treatment_12ow.csv", sep = ",")


###########################
# %% select variables
###########################

setwd("P:/NZR/NZR-001/Innovatieve Onderzoeksmethoden/2. Machine learning antibiotica bij ontstekingen/03. Dataverwerking/4. input R")

vars_selected <-read.csv(file = "variable_selections.csv",sep=';')

vars_treatment   <- as.character(filter(vars_selected,treatment == 1)$variable)
vars_outcome     <- as.character(filter(vars_selected,outcome == 1)$variable)
vars_other       <- as.character(filter(vars_selected,other == 1)$variable)
vars_relevant    <- as.character(filter(vars_selected,relevant == 1)$variable)
vars_relevant_nto    <- as.character(filter(vars_selected,relevant_nto == 1)$variable)

###########################
# %% drop variables that were indicated as irrelevant
###########################
data_first_treatment_12ow_relevant <- select(data_first_treatment_12ow,vars_relevant)

###########################
# %% drop variables with missing values
###########################

# select only variables with percentage missings less than threshold
data_first_treatment_12ow_relevant_0.4 <- data_first_treatment_12ow_relevant[, colMeans(is.na(data_first_treatment_12ow_relevant)) <= 0.4]

data_first_treatment_12ow_0.4 <- data_first_treatment_12ow[, colMeans(is.na(data_first_treatment_12ow)) <= 0.4]

# FYI what were the dropped variables?
setdiff(vars_selected$variable,names(data_first_treatment_12ow_relevant_0.4))
setdiff(vars_selected$variable,names(data_first_treatment_12ow_0.4))
# only CRP due to missings, other dropped variables are not relevant

# remove dropped variables from variable selection
vars_relevant_0.4 <- intersect(vars_relevant,names(data_first_treatment_12ow_relevant_0.4))

###########################
# %% descriptive statistics and remove variables with variance = 0 @@@ still gives warning messages about deprecated
###########################
# TvE: tried two alternatives, both resulted in no variabels with variance ==0. Checked in stata --> same result

# which variables have variance = 0?
# vars_zero = lapply(data_first_treatment_12ow_relevant_0.4, var, na.rm = TRUE) != 0
# vars_zero_names = names(vars_zero[vars_zero == FALSE])
all(duplicated(data_first_treatment_12ow_relevant_0.4)[-1L])
which(apply(data_first_treatment_12ow_relevant_0.4, 2, var) == 0)

# drop variables with variance = 0
#data_first_treatment_12ow_relevant_0.4_v0<- dplyr::select(data_first_treatment_12ow_relevant_0.4,-vars_zero_names)

# remove dropped variables from variable selection
#vars_relevant_0.4_v0 <- intersect(vars_relevant,names(data_first_treatment_12ow_relevant_0.4_v0)) 


###########################
# %% remove rows with any missing values
###########################

data_first_treatment_12ow_relevant_0.4_na.omit <- na.omit(data_first_treatment_12ow_relevant_0.4)

vars_relevant_0.4_na.omit <- names(data_first_treatment_12ow_relevant_0.4_na.omit)
# TvE: what happens here? (above, nothing?)

# FYI how many rows are deleted (~10000)
dim(data_first_treatment_12ow_relevant_0.4)[1] - dim(na.omit(data_first_treatment_12ow_relevant_0.4))[1]

###########################
# %% descriptive statistics and remove variables with variance = 0 @@@ still gives warning messages about deprecated
# TvE: this is a duplication of lines 131 and further, should be removed?
###########################

# which variables have variance = 0?
#vars_zero_na.omit = lapply(data_first_treatment_12ow_relevant_0.4_v0_na.omit, var, na.rm = TRUE) != 0
#vars_zero_na.omit_names = names(vars_zero_na.omit[vars_zero_na.omit == FALSE])
#all(duplicated(x)[-1L])

# drop variables with variance = 0
#data_first_treatment_12ow_relevant_0.4_na.omit_v0<- dplyr::select(data_first_treatment_12ow_relevant_0.4_v0_na.omit,-vars_zero_na.omit_names)

# remove dropped variables from variable selection
#vars_relevant_0.4_na.omit_v0 <- intersect(vars_relevant,names(data_first_treatment_12ow_relevant_0.4_v0_na.omit)) 



###########################
# %% print table one
###########################
setwd("P:/NZR/NZR-001/Innovatieve Onderzoeksmethoden/2. Machine learning antibiotica bij ontstekingen/03. Dataverwerking/6. output R/20190125wqi/")

a<-print(CreateTableOne(vars=names(data_first_treatment_12ow_relevant_0.4),
                        strata="AB_nose_infection",
                        data=data_first_treatment_12ow_relevant_0.4[,names(data_first_treatment_12ow_relevant_0.4) %in% vars_relevant_0.4, drop = F],
                        test=TRUE))

b<-print(CreateTableOne(vars=names(data_first_treatment_12ow_relevant_0.4),
                        strata="type_AB_nose",
                        data=data_first_treatment_12ow_relevant_0.4[,names(data_first_treatment_12ow_relevant_0.4) %in% vars_relevant_0.4, drop = F],
                        test=TRUE))

# TvE added 'c' to test shorter code (result: c=b)                        
#c<-print(CreateTableOne(vars=names(data_first_treatment_12ow_relevant_0.4),
#strata="type_AB_nose",
#data=data_first_treatment_12ow_relevant_0.4,
#test=TRUE))

write.table(a, file="Unmatched_ab_0.4.csv", sep = ",")
write.table(b, file="Unmatched_type_0.4.csv", sep = ",")
#write.table(c, file="Unmatched_type_test.csv", sep = ",")

a1<-print(CreateTableOne(vars=names(data_first_treatment_12ow_relevant),
                         strata="AB_nose_infection",
                         data=data_first_treatment_12ow_relevant,
                         test=TRUE))

b1<-print(CreateTableOne(vars=names(data_first_treatment_12ow_relevant),
                         strata="type_AB_nose",
                         data=data_first_treatment_12ow_relevant,
                         test=TRUE))

write.table(a1, file="Unmatched_ab.csv", sep = ",")
write.table(b1, file="Unmatched_type.csv", sep = ",")
###########################
# %% STEP 1: select variables with OR > 1%
###########################
# TVE: added 'link=logit' to first calculation (did not cause any differences)
# conclusion: no variables change the OR by more than ~1% so no confounders selected in this step

# calculate OR1
model_OR <-glm(
  outcome_4 ~ AB_nose_infection,
  family = binomial(link=logit),
  data = data_first_treatment_12ow_relevant_0.4)
OR1<- exp(model_OR$coefficients[2])

# FYI model summary
summary(model_OR)

# calculate rates for relevant variables
# TVE: vars you don't want to include here
vars_relevant_0.4_nto <- vars_relevant_0.4[!vars_relevant_0.4%in% c("outcome_4","AB_nose_infection","type_AB_nose")]

rate <- sapply(vars_relevant_0.4_nto,
               function(varname){
                 md <-glm(
                   as.formula(paste("outcome_4 ~ AB_nose_infection + ",varname)),
                   family=binomial(link=logit),
                   data=data_first_treatment_12ow_relevant_0.4
                 )
                 rate <- abs(exp(summary(md)$coef[2])-OR1)/OR1
               }
)

# drop the variables that have OR =< 1%
vars_OR <- labels(rate)[unlist(rate)> 0.01]

# drop ".AB_nose_infection1" from labels
vars_OR <- gsub(".AB_nose_infection1", "", vars_OR)

###########################
# %% STEP 2: AIC to select co-variables: outcome in age<10
###########################
model_AICo <- glm(
  outcome_4 ~ .,
  weights = outcome_weight,
  data = dplyr::select(subset(data_first_treatment_12ow_relevant_0.4_na.omit, age<10), - vars_treatment),
  family=binomial)


nrow(subset(data_first_treatment_12ow_relevant_0.4_na.omit, age<10))
names(subset(data_first_treatment_12ow_relevant_0.4_na.omit, age<10))
# TAKE the box with max_coverage as a variable to glm

model_AICo10y <- glm(
  outcome_4 ~  sex +  age+ poor_immune_response+ nr_medication+ diab_morb+ HIV_morb+ kanker_morb+ gezichtst_morb+ gehoorst_morb+ aangaf_morb+ coronhartz_morb+ RA_morb
  + COPD_morb+ astma_morb+ hartritme_morb+ migraine_morb+ hartklep_morb+ neurast_morb+ nr_contacts_infection
+ Angst_morb+ Depressie_morb+ versthand_morb
+ schizofr_morb
+nr_prescriptions_AB+ nr_contacts_resp+  nr_chron3+ practice_size+ day+fever+ season ,
  weights = outcome_weight,
  data = subset(data_first_treatment_12ow_relevant_0.4_na.omit, age<10),
  family=binomial)

str(subset(data_first_treatment_12ow_relevant_0.4_na.omit, age<10)$flu_epi)
table(subset(data_first_treatment_12ow_relevant_0.4_na.omit, age<10)$postalcode)


#delete   dementie_morb+ postalcode  flu_epi

# Select most predictive variables
AICo10y <- stepAIC(model_AICo10y)
# FYI show formula of the final model
formula(AICo10y)
# show the selected variables
vars_AICo10y <- attr(terms(AICo10y),"term.labels")

###########################
# %% STEP 3: AIC to select co-variables: treatment
###########################

model_AICt <- glm(
  AB_nose_infection ~ .,
  data = select(data_first_treatment_12ow_relevant_0.4_na.omit[!names(data_first_treatment_12ow_relevant_0.4_na.omit) %in% vars_outcome],- type_AB_nose),
  family=binomial)


model_AICt10y <- glm(
  AB_nose_infection ~  sex +  age+ poor_immune_response+ nr_medication+ diab_morb+ HIV_morb+ kanker_morb+ gezichtst_morb+ gehoorst_morb+ aangaf_morb+ coronhartz_morb+ RA_morb
  + COPD_morb+ astma_morb+ hartritme_morb+ migraine_morb+ hartklep_morb+ neurast_morb+ nr_contacts_infection
  + Angst_morb+ Depressie_morb+ versthand_morb
  + schizofr_morb
  +nr_prescriptions_AB+ nr_contacts_resp+  nr_chron3+ practice_size+ day+fever+ season ,
  weights = outcome_weight,
  data = subset(data_first_treatment_12ow_relevant_0.4_na.omit, age<10),
  family=binomial)




summary(model_AICt10y)
# Select most predictive variables
AICt10y <- stepAIC(model_AICt10y)

# FYI show formula of the final model
formula(AICt10y)

# Show the selected variables
vars_AICt10y <- attr(terms(AICt10y),"term.labels")

###########################
# %% Step 4 univariate relation outcome ~ selected treatment variables
###########################

models10y <- lapply(vars_AICt10y,
                 function(x) {
                   glm(substitute(outcome_4 ~ i, list(i = as.name(x))),
                       weights = outcome_weight,
                       family = binomial,
                       data = select(subset(data_first_treatment_12ow_relevant_0.4_na.omit, age<10),-vars_treatment))
                 }
)

lapply(models10y, summary)

# try logistic uni& multivariate to select confounders

#### logistic univariate
#uni<-glm(outcome_4~AB_nose_infection, data= data_first_treatment_12ow_relevant_0.4, family = binomial)

#summary(uni)
data_first_treatment_12ow_relevant_do_4 <- select(data_first_treatment_12ow_relevant,-outcome_4)

uni <- lapply(names(data_first_treatment_12ow_relevant_do_4),
              function(var){
                uni<-glm(
                  as.formula(paste("outcome_4 ~", var)), 
                  data= data_first_treatment_12ow_relevant, family = binomial)
                summary(uni)
              })

capture.output(uni, file = "Uni.doc")

uni_w <- glm(outcome_4 ~ AB_nose_infection, 
             weights = outcome_weight,
             family = "binomial",
             data = data_first_treatment_12ow_relevant_0.4)
summary(uni_w)

#### logistic multivariate
vars_relevant_0.4_nto <- vars_relevant_0.4[!vars_relevant_0.4%in% c("outcome_4","AB_nose_infection","type_AB_nose")]

multicov_w <-glm(
  as.formula(paste("outcome_4 ~ AB_nose_infection + ", paste(vars_relevant_0.4_nto, collapse="+"),sep="")),
  weights = outcome_weight,
  family = "binomial",
  data = data_first_treatment_12ow_relevant_0.4
)

summary(multicov_w)


###########################
# %% Determine confounder variables @@@ todo ask ameen how we do this

# TVE: I think that the confounders that resulted form step 1 (OR) are missing? nr_contacts_infectino and nr_prescriptions_AB? (nr_contacts_resp and nr_medications are included through AICo)
# Added manually (last two lines) --> omitted after email from AmeeN: 1% change is not enough (standaard is 10%)
###########################

vars_confounders = c(
  "age",
  "sex",
  "nr_medication",
  "COPD_morb",
  "migraine_morb",
  "nr_contacts_resp",
  "osteop_morb",
  "poor_immune_response",
  "alcmisb_morb",
  "practice_size",
  "Depressie_morb",
  "aangaf_morb",
  "hartfalen_morb",
  "flu_epi",
  "season",
  "postalcode",
  "nr_contacts_infection",
  "nr_prescriptions_AB",
  "day"
)
vars_confounders_10y = c(
  "sex",
  "nr_medication",
  "COPD_morb",
  "nr_contacts_resp",
  "poor_immune_response",
  "season",
  "HIV_morb ", 
  "nr_chron3",
  "nr_contacts_infection",
  "nr_prescriptions_AB",
  "day"
)
vars_confounders_10ynm = c(
  "sex",
  "nr_medication",
  "COPD_morb",
  "nr_contacts_resp",
  "poor_immune_response",
  "season",
  "HIV_morb ", 
  "nr_chron3",
  "day"
)
vars_confounders_np = c(
  "age",
  "sex",
  "nr_medication",
  "COPD_morb",
  "migraine_morb",
  "nr_contacts_resp",
  "osteop_morb",
  "poor_immune_response",
  "alcmisb_morb",
  "practice_size",
  "Depressie_morb",
  "aangaf_morb",
  "hartfalen_morb",
  "flu_epi",
  "season",
  "nr_contacts_infection",
  "nr_prescriptions_AB",
  "day"
)

vars_confounders_nm = c(
  "age",
  "sex",
  "nr_medication",
  "COPD_morb",
  "migraine_morb",
  "osteop_morb",
  "poor_immune_response",
  "alcmisb_morb",
  "practice_size",
  "Depressie_morb",
  "aangaf_morb",
  "hartfalen_morb",
  "flu_epi",
  "season",
  "postalcode",
  "day"
)

vars_confounders_npm = c(
  "age",
  "sex",
  "nr_medication",
  "COPD_morb",
  "migraine_morb",
  "osteop_morb",
  "poor_immune_response",
  "alcmisb_morb",
  "practice_size",
  "Depressie_morb",
  "aangaf_morb",
  "hartfalen_morb",
  "flu_epi",
  "season",
  "day"
)

data_confounders_to <- data_first_treatment_12ow_relevant[c(
  "outcome_4","AB_nose_infection","type_AB_nose", "outcome_weight", "ID", 
  "age",
  "sex",
  "nr_medication",
  "COPD_morb",
  "migraine_morb",
  "nr_contacts_resp",
  "osteop_morb",
  "poor_immune_response",
  "alcmisb_morb",
  "practice_size",
  "Depressie_morb",
  "aangaf_morb",
  "hartfalen_morb",
  "flu_epi",
  "season",
  "postalcode",
  "nr_contacts_infection",
  "nr_prescriptions_AB",
  "day"
)]        
data_confounders_to_10y <- subset(data_first_treatment_12ow_relevant, age< 10)[c(
  "outcome_4","AB_nose_infection","type_AB_nose", "outcome_weight", "ID", 
  "sex",
  "nr_medication",
  "COPD_morb",
  "nr_contacts_resp",
  "poor_immune_response",
  "season",
  "HIV_morb", 
  "nr_chron3",
  "nr_contacts_infection",
  "nr_prescriptions_AB",
  "day"
)]  

data_confounders_to_c <- data_first_treatment_12ow_relevant[c(
  "outcome_4","AB_nose_infection","type", "outcome_weight", "ID", 
  "age",
  "sex",
  "nr_medication",
  "COPD_morb",
  "migraine_morb",
  "nr_contacts_resp",
  "osteop_morb",
  "poor_immune_response",
  "alcmisb_morb",
  "practice_size",
  "Depressie_morb",
  "aangaf_morb",
  "hartfalen_morb",
  "flu_epi",
  "season",
  "postalcode",
  "nr_contacts_infection",
  "nr_prescriptions_AB",
  "day"
)]   


data_confounders <- data_first_treatment_12ow_relevant[c(
  "ID",
  "age",
  "sex",
  "nr_medication",
  "COPD_morb",
  "migraine_morb",
  "nr_contacts_resp",
  "osteop_morb",
  "poor_immune_response",
  "alcmisb_morb",
  "practice_size",
  "Depressie_morb",
  "aangaf_morb",
  "hartfalen_morb",
  "flu_epi",
  "season",
  "postalcode",
  "nr_contacts_infection",
  "nr_prescriptions_AB",
  "day"
)] 
data_confounders_10y <- subset(data_first_treatment_12ow_relevant, age< 10)[c(
  "ID",
  "sex",
  "nr_medication",
  "COPD_morb",
  "nr_contacts_resp",
  "poor_immune_response",
  "season",
  "HIV_morb", 
  "nr_chron3",
  "nr_contacts_infection",
  "nr_prescriptions_AB",
  "day"
)] 

s<-dfSummary(data_confounders_to)
write.table(s, file="descriptives_cto.csv", sep = ",")

s10y<-dfSummary(data_confounders_to_10y)
write.table(s10y, file="descriptives_cto_10y.csv", sep = ",")


###########################
# %% Propensity score matching with ps in children <10y

###########################


data_confounders_to_10y$AB<-0
data_confounders_to_10y$AB[as.numeric(data_confounders_to_10y$AB_nose_infection)==2]<-1


set.seed(1)
PS_Model_10y<-ps(
  as.formula(paste("AB ~ ",paste(vars_confounders_10y,collapse="+"),sep="")),
  data = data_confounders_to_10y,
  verbose = FALSE, 
  estimand = "ATE")

plot(PS_Model_10y, plots = 1)
plot(PS_Model_10y, plots = 2)
plot(PS_Model_10y, plots = 3)
plot(PS_Model_10y, plots = 4)
plot(PS_Model_10y, plots = 5)

summary(PS_Model_10y$gbm.obj,
        n.trees=PS_Model_10y$desc$ks.mean.ATE$n.trees,
        plot=TRUE)
summary(PS_Model_10y)
ps.balance_10y<-bal.table(PS_Model_10y)
ps.balance_10y
write.table(ps.balance_10y, file="P:/NZR/NZR-001/Innovatieve Onderzoeksmethoden/2. Machine learning antibiotica bij ontstekingen/03. Dataverwerking/6. output R/20190201wqi/ps.balance_ab_10y.csv", sep = ",")

#write.table(ps.balance, file="H:/qww/AMC/Utrecht/NIVEL/writing/tables/ps.balance_abflu.csv", sep = ",")

data_confounders_to_10y$w<- get.weights(PS_Model_10y,stop.method = "es.mean" )


design.ps_10y <- svydesign(ids=~1, weights = ~ w*outcome_weight, data = data_confounders_to_10y)


## survey
# model 1
glm_ab_10y <- svyglm(outcome_4 ~ AB_nose_infection, design = design.ps_10y, family=quasibinomial)

summary(glm_ab_10y)

cbind( exp(coef(glm_ab_10y)), exp(summary(glm_ab_10y)$coefficients[,1] - 1.96*summary(glm_ab_10y)$coefficients[,2]), exp(summary(glm_ab_10y)$coefficients[,1] + 1.96*summary(glm_ab_10y)$coefficients[,2]) )

# model 2
glm_ab_10y_part <- svyglm(outcome_4 ~ AB_nose_infection + nr_medication, design = design.ps_10y, family=quasibinomial)

summary(glm_ab_10y_part)

cbind( exp(coef(glm_ab_10y_part)), exp(summary(glm_ab_10y_part)$coefficients[,1] - 1.96*summary(glm_ab_10y_part)$coefficients[,2]), exp(summary(glm_ab_10y_part)$coefficients[,1] + 1.96*summary(glm_ab_10y_part)$coefficients[,2]) )

# model 3
glm_ab_10y_confounder <- svyglm(
  as.formula(paste("outcome_4 ~ AB_nose_infection +",paste(vars_confounders_10y,collapse="+"),sep="")), 
  design = design.ps_10y, family=quasibinomial)

summary(glm_ab_10y_confounder)

cbind( exp(coef(glm_ab_10y_confounder)), exp(summary(glm_ab_10y_confounder)$coefficients[,1] - 1.96*summary(glm_ab_10y_confounder)$coefficients[,2]), exp(summary(glm_ab_10y_confounder)$coefficients[,1] + 1.96*summary(glm_ab_10y_confounder)$coefficients[,2]) )

# model 4
glm_ab10y_confounder_nm <- svyglm(
  as.formula(paste("outcome_4 ~ AB_nose_infection +",paste(vars_confounders_10ynm,collapse="+"),sep="")), 
  design = design.ps_10y, family=quasibinomial)

summary(glm_ab10y_confounder_nm)

cbind( exp(coef(glm_ab10y_confounder_nm)), exp(summary(glm_ab10y_confounder_nm)$coefficients[,1] - 1.96*summary(glm_ab10y_confounder_nm)$coefficients[,2]), exp(summary(glm_ab10y_confounder_nm)$coefficients[,1] + 1.96*summary(glm_ab10y_confounder_nm)$coefficients[,2]) )

###########################
# %% Can we do Propensity score matching with ps seprated by 10 years old?  age->agecat71018
###########################

## survey
# model 1
subset=variable=="value"


glm_ab <- svyglm(outcome_4 ~ AB_nose_infection, subset=sex==1, design = design.ps, family=quasibinomial)
glm_ab1 <- svyglm(outcome_4 ~ AB_nose_infection +  agecat71018,  design = design.ps, family=quasibinomial)

summary(glm_ab1)


###########################
# %% Propensity score matching with ps in POPULATION Exclude under 10 

###########################


data_confounders_to$AB<-0
data_confounders_to$AB[as.numeric(data_confounders_to$AB_nose_infection)==2]<-1


set.seed(1)
PS_Model10<-ps(
  as.formula(paste("AB ~ ",paste(vars_confounders,collapse="+"),sep="")),
  data = subset(data_confounders_to, age>=10),
  verbose = FALSE, 
  estimand = "ATE")

plot(PS_Model10, plots = 1)
plot(PS_Model10, plots = 2)
plot(PS_Model10, plots = 3)
plot(PS_Model10, plots = 4)
plot(PS_Model10, plots = 5)

summary(PS_Model10$gbm.obj,
        n.trees=PS_Model$desc$ks.mean.ATE$n.trees,
        plot=TRUE)
summary(PS_Model10)
ps.balance<-bal.table(PS_Model10)
ps.balance
write.table(ps.balance, file="P:/NZR/NZR-001/Innovatieve Onderzoeksmethoden/2. Machine learning antibiotica bij ontstekingen/03. Dataverwerking/6. output R/20181221wqi/ps.balance_ab10.csv", sep = ",")

#write.table(ps.balance, file="H:/qww/AMC/Utrecht/NIVEL/writing/tables/ps.balance_abflu.csv", sep = ",")

data_confounders_to10 <- mutate(subset(data_confounders_to, age>=10), 
                                w10=get.weights(PS_Model10,stop.method = "es.mean" ))

design.ps10 <- svydesign(ids=~1, weights = ~ w10*outcome_weight, data = data_confounders_to10)


## survey
# model 1
glm_ab10 <- svyglm(outcome_4 ~ AB_nose_infection, design = design.ps10, family=quasibinomial)

summary(glm_ab10)

cbind( exp(coef(glm_ab10)), exp(summary(glm_ab10)$coefficients[,1] - 1.96*summary(glm_ab10)$coefficients[,2]), exp(summary(glm_ab10)$coefficients[,1] + 1.96*summary(glm_ab10)$coefficients[,2]) )

# model 2
glm_ab10_part <- svyglm(outcome_4 ~ AB_nose_infection + nr_medication, design = design.ps10, family=quasibinomial)

summary(glm_ab10_part)

cbind( exp(coef(glm_ab10_part)), exp(summary(glm_ab10_part)$coefficients[,1] - 1.96*summary(glm_ab10_part)$coefficients[,2]), exp(summary(glm_ab10_part)$coefficients[,1] + 1.96*summary(glm_ab10_part)$coefficients[,2]) )

# model 3
glm_ab10_confounder <- svyglm(
  as.formula(paste("outcome_4 ~ AB_nose_infection +",paste(vars_confounders,collapse="+"),sep="")), 
  design = design.ps10, family=quasibinomial)

summary(glm_ab10_confounder)

cbind( exp(coef(glm_ab10_confounder)), exp(summary(glm_ab10_confounder)$coefficients[,1] - 1.96*summary(glm_ab10_confounder)$coefficients[,2]), exp(summary(glm_ab10_confounder)$coefficients[,1] + 1.96*summary(glm_ab10_confounder)$coefficients[,2]) )

# model 4
glm_ab10_confounder_nm <- svyglm(
  as.formula(paste("outcome_4 ~ AB_nose_infection +",paste(vars_confounders_nm,collapse="+"),sep="")), 
  design = design.ps10, family=quasibinomial)

summary(glm_ab10_confounder_nm)

cbind( exp(coef(glm_ab10_confounder_nm)), exp(summary(glm_ab10_confounder_nm)$coefficients[,1] - 1.96*summary(glm_ab10_confounder_nm)$coefficients[,2]), exp(summary(glm_ab10_confounder_nm)$coefficients[,1] + 1.96*summary(glm_ab10_confounder_nm)$coefficients[,2]) )


###########################
# %% Propensity score matching with ps < 10 years old

###########################


data_confounders_to$AB<-0
data_confounders_to$AB[as.numeric(data_confounders_to$AB_nose_infection)==2]<-1


set.seed(1)
PS_Model10y<-ps(
  as.formula(paste("AB ~ ",paste(vars_confounders,collapse="+"),sep="")),
  data = subset(data_confounders_to, age<10),
  verbose = FALSE, 
  estimand = "ATE")

plot(PS_Model10y, plots = 1)
plot(PS_Model10y, plots = 2)
plot(PS_Model10y, plots = 3)
plot(PS_Model10y, plots = 4)
plot(PS_Model10y, plots = 5)

summary(PS_Model10y$gbm.obj,
        n.trees=PS_Model$desc$ks.mean.ATE$n.trees,
        plot=TRUE)
summary(PS_Model10y)
ps.balance<-bal.table(PS_Model10y)
ps.balance
write.table(ps.balance, file="P:/NZR/NZR-001/Innovatieve Onderzoeksmethoden/2. Machine learning antibiotica bij ontstekingen/03. Dataverwerking/6. output R/20181221wqi/ps.balance_ab10.csv", sep = ",")

write.table(ps.balance, file="H:/qww/AMC/Utrecht/NIVEL/writing/tables/ps.balance_ab10y.csv", sep = ",")

data_confounders_to10y <- mutate(subset(data_confounders_to, age<10), 
                                w10y=get.weights(PS_Model10y,stop.method = "es.mean" ))

design.ps10y <- svydesign(ids=~1, weights = ~ w10y*outcome_weight, data = data_confounders_to10y)


## survey
# model 1
glm_ab10y  <- svyglm(outcome_4 ~ AB_nose_infection, design = design.ps10y , family=quasibinomial)

summary(glm_ab10y )

cbind( exp(coef(glm_ab10y )), exp(summary(glm_ab10y )$coefficients[,1] - 1.96*summary(glm_ab10y )$coefficients[,2]), exp(summary(glm_ab10y )$coefficients[,1] + 1.96*summary(glm_ab10y )$coefficients[,2]) )

# model 2
glm_ab10y_part <- svyglm(outcome_4 ~ AB_nose_infection + nr_medication, design = design.ps10y , family=quasibinomial)

summary(glm_ab10y_part)

cbind( exp(coef(glm_ab10y_part)), exp(summary(glm_ab10y_part)$coefficients[,1] - 1.96*summary(glm_ab10y_part)$coefficients[,2]), exp(summary(glm_ab10y_part)$coefficients[,1] + 1.96*summary(glm_ab10y_part)$coefficients[,2]) )

# model 3
glm_ab10y_confounder <- svyglm(
  as.formula(paste("outcome_4 ~ AB_nose_infection +",paste(vars_confounders,collapse="+"),sep="")), 
  design = design.ps10y, family=quasibinomial)
#Error in `contrasts<-`(`*tmp*`, value = contr.funs[1 + isOF[nn]]) : 
#contrasts can be applied only to factors with 2 or more levels
summary(glm_ab10y_confounder)

cbind( exp(coef(glm_ab10y_confounder)), exp(summary(glm_ab10y_confounder)$coefficients[,1] - 1.96*summary(glm_ab10y_confounder)$coefficients[,2]), exp(summary(glm_ab10y_confounder)$coefficients[,1] + 1.96*summary(glm_ab10y_confounder)$coefficients[,2]) )

# model 4
glm_ab10y_confounder_nm <- svyglm(
  as.formula(paste("outcome_4 ~ AB_nose_infection +",paste(vars_confounders_nm,collapse="+"),sep="")), 
  design = design.ps10y, family=quasibinomial)

summary(glm_ab10y_confounder_nm)

cbind( exp(coef(glm_ab10y_confounder_nm)), exp(summary(glm_ab10y_confounder_nm)$coefficients[,1] - 1.96*summary(glm_ab10y_confounder_nm)$coefficients[,2]), exp(summary(glm_ab10y_confounder_nm)$coefficients[,1] + 1.96*summary(glm_ab10y_confounder_nm)$coefficients[,2]) )



###########################
# %% GLM before matching
###########################
#### logistic univariate


uni18<-glm(outcome_4~AB_nose_infection, weights = outcome_weight, data = subset(data_confounders_to, age>=18), family = binomial)

summary(uni18)

cbind( exp(coef(uni18)), exp(summary(uni18)$coefficients[,1] - 1.96*summary(uni18)$coefficients[,2]), exp(summary(uni18)$coefficients[,1] + 1.96*summary(uni18)$coefficients[,2]) )


uni10<-glm(outcome_4~AB_nose_infection, weights = outcome_weight, data = subset(data_confounders_to, age>=10), family = binomial)

summary(uni10)

cbind( exp(coef(uni10)), exp(summary(uni10)$coefficients[,1] - 1.96*summary(uni10)$coefficients[,2]), exp(summary(uni10)$coefficients[,1] + 1.96*summary(uni10)$coefficients[,2]) )

uni10y_sub<-glm(outcome_4~AB_nose_infection, weights = outcome_weight, data = subset(data_confounders_to, age<10), family = binomial)

summary(uni10y_sub)

cbind( exp(coef(uni10y_sub)), exp(summary(uni10y_sub)$coefficients[,1] - 1.96*summary(uni10y_sub)$coefficients[,2]), exp(summary(uni10y_sub)$coefficients[,1] + 1.96*summary(uni10y_sub)$coefficients[,2]) )

uni_10y<-glm(outcome_4~AB_nose_infection, weights = outcome_weight, data= data_confounders_to_10y, family = binomial)

summary(uni_10y)

cbind( exp(coef(uni_10y)), exp(summary(uni_10y)$coefficients[,1] - 1.96*summary(uni_10y)$coefficients[,2]), exp(summary(uni_10y)$coefficients[,1] + 1.96*summary(uni_10y)$coefficients[,2]) )

## combine type 1&2
uni_type_c<-glm(outcome_4 ~ factor(type),weights = outcome_weight, data= data_first_treatment_12ow_relevant_0.4, family = binomial)

summary(uni_type_c)

cbind( exp(coef(uni_type_c)), exp(summary(uni_type_c)$coefficients[,1] - 1.96*summary(uni_type_c)$coefficients[,2]), exp(summary(uni_type_c)$coefficients[,1] + 1.96*summary(uni_type_c)$coefficients[,2]) )




uni7<-glm(outcome_4~AB_nose_infection, weights = outcome_weight, data = subset(data_confounders_to, age>=7), family = binomial)

summary(uni7)

cbind( exp(coef(uni7)), exp(summary(uni7)$coefficients[,1] - 1.96*summary(uni7)$coefficients[,2]), exp(summary(uni7)$coefficients[,1] + 1.96*summary(uni7)$coefficients[,2]) )





#### logistic multivariate



uni18cov <-glm(
  as.formula(paste("outcome_4 ~ AB_nose_infection + ", paste(vars_confounders, collapse="+"),sep="")),
  weights = outcome_weight,
  family = "binomial",
  data = subset(data_confounders_to, age>=18)
)

summary(uni18cov)

cbind( exp(coef(uni18cov)), exp(summary(uni18cov)$coefficients[,1] - 1.96*summary(uni18cov)$coefficients[,2]), exp(summary(uni18cov)$coefficients[,1] + 1.96*summary(uni18cov)$coefficients[,2]) )


uni10cov <-glm(
  as.formula(paste("outcome_4 ~ AB_nose_infection + ", paste(vars_confounders, collapse="+"),sep="")),
  weights = outcome_weight,
  family = "binomial",
  data = subset(data_confounders_to, age>=10)
)

summary(uni10cov)

cbind( exp(coef(uni10cov)), exp(summary(uni10cov)$coefficients[,1] - 1.96*summary(uni10cov)$coefficients[,2]), exp(summary(uni10cov)$coefficients[,1] + 1.96*summary(uni10cov)$coefficients[,2]) )


uni10ycov_sub <-glm(
  as.formula(paste("outcome_4 ~ AB_nose_infection + ", paste(vars_confounders, collapse="+"),sep="")),
  weights = outcome_weight,
  family = "binomial",
  data = subset(data_confounders_to, age<10)
)
#Error in `contrasts<-`(`*tmp*`, value = contr.funs[1 + isOF[nn]]) : 
#?Ա?ֻ????????��????????��?????ε?????
summary(uni10ycov_sub)

cbind( exp(coef(uni10ycov_sub)), exp(summary(uni10ycov_sub)$coefficients[,1] - 1.96*summary(uni10ycov_sub)$coefficients[,2]), exp(summary(uni10ycov_sub)$coefficients[,1] + 1.96*summary(uni10ycov_sub)$coefficients[,2]) )


uni_10ycov <-glm(
  as.formula(paste("outcome_4 ~ AB_nose_infection + ", paste(vars_confounders_10y, collapse="+"),sep="")),
  weights = outcome_weight,
  family = "binomial",
  data = data_confounders_to_10y
)

summary(uni_10ycov)

cbind( exp(coef(uni_10ycov)), exp(summary(uni_10ycov)$coefficients[,1] - 1.96*summary(uni_10ycov)$coefficients[,2]), exp(summary(uni_10ycov)$coefficients[,1] + 1.96*summary(uni_10ycov)$coefficients[,2]) )




uni7cov <-glm(
  as.formula(paste("outcome_4 ~ AB_nose_infection + ", paste(vars_confounders, collapse="+"),sep="")),
  weights = outcome_weight,
  family = "binomial",
  data = subset(data_confounders_to, age>=7)
)

summary(uni7cov)

cbind( exp(coef(uni7cov)), exp(summary(uni7cov)$coefficients[,1] - 1.96*summary(uni7cov)$coefficients[,2]), exp(summary(uni7cov)$coefficients[,1] + 1.96*summary(uni7cov)$coefficients[,2]) )


###########################
## Type AB analysis 

plot(data_first_treatment_12ow_relevant_0.4$type_AB_nose)

table(data_first_treatment_12ow_relevant_0.4$type_AB_nose)


### Testing before mnps
#### logistic univariate

uni18_type<-glm(outcome_4 ~ factor(type_AB_nose),weights = outcome_weight, data = subset(data_confounders_to, age>=18), family = binomial)

summary(uni18_type)

cbind( exp(coef(uni18_type)), exp(summary(uni18_type)$coefficients[,1] - 1.96*summary(uni18_type)$coefficients[,2]), exp(summary(uni18_type)$coefficients[,1] + 1.96*summary(uni18_type)$coefficients[,2]) )


uni10_type<-glm(outcome_4 ~ factor(type_AB_nose),weights = outcome_weight, data = subset(data_confounders_to, age>=10), family = binomial)

summary(uni10_type)

cbind( exp(coef(uni10_type)), exp(summary(uni10_type)$coefficients[,1] - 1.96*summary(uni10_type)$coefficients[,2]), exp(summary(uni10_type)$coefficients[,1] + 1.96*summary(uni10_type)$coefficients[,2]) )



uni10y_type_sub<-glm(outcome_4 ~ factor(type_AB_nose),weights = outcome_weight, data = subset(data_confounders_to, age<10), family = binomial)

summary(uni10y_type_sub)

cbind( exp(coef(uni10y_type_sub)), exp(summary(uni10y_type_sub)$coefficients[,1] - 1.96*summary(uni10y_type_sub)$coefficients[,2]), exp(summary(uni10y_type_sub)$coefficients[,1] + 1.96*summary(uni10y_type_sub)$coefficients[,2]) )



uni_10y_type<-glm(outcome_4 ~ factor(type_AB_nose),weights = outcome_weight, data = data_confounders_to_10y,  family = binomial)

summary(uni_10y_type)

cbind( exp(coef(uni_10y_type)), exp(summary(uni_10y_type)$coefficients[,1] - 1.96*summary(uni_10y_type)$coefficients[,2]), exp(summary(uni_10y_type)$coefficients[,1] + 1.96*summary(uni_10y_type)$coefficients[,2]) )




uni7_type<-glm(outcome_4 ~ factor(type_AB_nose),weights = outcome_weight,  data = subset(data_confounders_to, age>=7), family = binomial)

summary(uni7_type)

cbind( exp(coef(uni7_type)), exp(summary(uni7_type)$coefficients[,1] - 1.96*summary(uni7_type)$coefficients[,2]), exp(summary(uni7_type)$coefficients[,1] + 1.96*summary(uni7_type)$coefficients[,2]) )



#### logistic multivariate
multi_type <-glm(
  as.formula(paste("outcome_4 ~ type + ", paste(vars_confounders, collapse="+"),sep="")),
  weights = outcome_weight,
  family = "binomial",  data = data_confounders_to_c) 

summary(multi_type)

cbind( exp(coef(multi_type)), exp(summary(multi_type)$coefficients[,1] - 1.96*summary(multi_type)$coefficients[,2]), exp(summary(multi_type)$coefficients[,1] + 1.96*summary(multi_type)$coefficients[,2]) )



multi_type18 <-glm(
  as.formula(paste("outcome_4 ~ type_AB_nose + ", paste(vars_confounders, collapse="+"),sep="")),
  weights = outcome_weight,
  family = "binomial",  data = subset(data_confounders_to, age>=18)) 

summary(multi_type18)

cbind( exp(coef(multi_type18)), exp(summary(multi_type18)$coefficients[,1] - 1.96*summary(multi_type18)$coefficients[,2]), exp(summary(multi_type18)$coefficients[,1] + 1.96*summary(multi_type18)$coefficients[,2]) )


multi_type10 <-glm(
  as.formula(paste("outcome_4 ~ type_AB_nose + ", paste(vars_confounders, collapse="+"),sep="")),
  weights = outcome_weight,
  family = "binomial",  data = subset(data_confounders_to, age>=10)) 

summary(multi_type10)

cbind( exp(coef(multi_type10)), exp(summary(multi_type10)$coefficients[,1] - 1.96*summary(multi_type10)$coefficients[,2]), exp(summary(multi_type10)$coefficients[,1] + 1.96*summary(multi_type10)$coefficients[,2]) )


multi_type_10y <-glm(
  as.formula(paste("outcome_4 ~ type_AB_nose + ", paste(vars_confounders_10y, collapse="+"),sep="")),
  weights = outcome_weight,
  family = "binomial",  data = data_confounders_to_10y) 

summary(multi_type_10y)

cbind( exp(coef(multi_type_10y)), exp(summary(multi_type_10y)$coefficients[,1] - 1.96*summary(multi_type_10y)$coefficients[,2]), exp(summary(multi_type_10y)$coefficients[,1] + 1.96*summary(multi_type_10y)$coefficients[,2]) )






multi_type7 <-glm(
  as.formula(paste("outcome_4 ~ type_AB_nose + ", paste(vars_confounders, collapse="+"),sep="")),
  weights = outcome_weight,
  family = "binomial",  data = subset(data_confounders_to, age>=7)) 

summary(multi_type7)

cbind( exp(coef(multi_type7)), exp(summary(multi_type7)$coefficients[,1] - 1.96*summary(multi_type7)$coefficients[,2]), exp(summary(multi_type7)$coefficients[,1] + 1.96*summary(multi_type7)$coefficients[,2]) )




###########################ctor
# %% running of multinomial propensity scores
###########################
###combine type1&2
MNPS_cto_type_c <- mnps(
  as.formula(paste("type ~ ",paste(vars_confounders,collapse="+"),sep="")),
  data = data_confounders_to_c,
  estimand = "ATE",
  verbose = FALSE,
  stop.method = c("es.mean", "ks.mean"),
  n.trees = 5000)
#Warning message:
#In ps(formula = currFormula, data = currDat, n.trees = n.trees[i],  :
#  Optimal number of iterations is close to the specified n.trees. n.trees is likely set too small and better balance might be obtainable by setting n.trees to be larger.

summary(MNPS_cto_type_c$gbm.obj,
        n.trees=MNPS_cto_type_c$desc$ks.mean.ATE$n.trees,
        plot=TRUE)

summary(MNPS_cto_type_c)
mnp.balance<-bal.table(MNPS_cto_type_c)
mnp.balance
write.table(mnp.balance, file="P:/NZR/NZR-001/Innovatieve Onderzoeksmethoden/2. Machine learning antibiotica bij ontstekingen/03. Dataverwerking/6. output R/20181221wqi/mnp.balance_type_c.csv", sep = ",")

write.table(mnp.balance, file="H:/qww/AMC/Utrecht/NIVEL/writing/tables/mnps.balance_type_c.csv", sep = ",")

plot(MNPS_cto_type_c, plots = 1) 
plot(MNPS_cto_type_c, plots = 2, subset = "es.mean")
plot(MNPS_cto_type_c, plots = 3) 
#Warning message:
#In plot.mnps(MNPS_cto_type_c, plots = 3) :
#Some effect sizes are larger than 3 and may not have been plotted.
plot(MNPS_cto_type_c, plots = 4) 
plot(MNPS_cto_type_c, plots = 5) 

data_confounders_to_c$wt <- get.weights(MNPS_cto_type_c, stop.method = "es.mean")

design.mnps_c <- svydesign(ids=~1, weights=~wt*outcome_weight, data=data_confounders_to_c) 

## survey
# model1
glm_type_c <- svyglm(outcome_4 ~ type,  design = design.mnps_c,family=quasibinomial)

summary(glm_type_c)

cbind( exp(coef(glm_type_c)), exp(summary(glm_type_c)$coefficients[,1] - 1.96*summary(glm_type_c)$coefficients[,2]), exp(summary(glm_type_c)$coefficients[,1] + 1.96*summary(glm_type_c)$coefficients[,2]) )


# model 2
glm_type_c_part <- svyglm(outcome_4 ~ type + season , design = design.mnps_c, family=quasibinomial)

summary(glm_type_c_part)

cbind( exp(coef(glm_type_c_part)), exp(summary(glm_type_c_part)$coefficients[,1] - 1.96*summary(glm_type_c_part)$coefficients[,2]), exp(summary(glm_type_c_part)$coefficients[,1] + 1.96*summary(glm_type_c_part)$coefficients[,2]) )

# model 3
glm_type_c_confounder <- svyglm(
  as.formula(paste("outcome_4 ~ type +",paste(vars_confounders,collapse="+"),sep="")), 
  design = design.mnps_c, family=quasibinomial)

summary(glm_type_c_confounder)

cbind( exp(coef(glm_type_c_confounder)), exp(summary(glm_type_c_confounder)$coefficients[,1] - 1.96*summary(glm_type_c_confounder)$coefficients[,2]), exp(summary(glm_type_c_confounder)$coefficients[,1] + 1.96*summary(glm_type_c_confounder)$coefficients[,2]) )

# model 4
glm_type_c_confounder_nm <- svyglm(
  as.formula(paste("outcome_4 ~ type +",paste(vars_confounders_nm,collapse="+"),sep="")), 
  design = design.mnps_c, family=quasibinomial)

summary(glm_type_c_confounder_nm)

cbind( exp(coef(glm_type_c_confounder_nm)), exp(summary(glm_type_c_confounder_nm)$coefficients[,1] - 1.96*summary(glm_type_c_confounder_nm)$coefficients[,2]), exp(summary(glm_type_c_confounder_nm)$coefficients[,1] + 1.96*summary(glm_type_c_confounder_nm)$coefficients[,2]) )
##########

MNPS_cto_type <- mnps(
  as.formula(paste("type_AB_nose ~ ",paste(vars_confounders,collapse="+"),sep="")),
  data = data_confounders_to,
  estimand = "ATE",
  verbose = FALSE,
  stop.method = c("es.mean", "ks.mean"),
  n.trees = 5000)
#Warning message:
#In ps(formula = currFormula, data = currDat, n.trees = n.trees[i],  :
#  Optimal number of iterations is close to the specified n.trees. n.trees is likely set too small and better balance might be obtainable by setting n.trees to be larger.

summary(MNPS_cto_type$gbm.obj,
        n.trees=MNPS_cto_type$desc$ks.mean.ATE$n.trees,
        plot=TRUE)

summary(MNPS_cto_type)
mnp.balance<-bal.table(MNPS_cto_type)
mnp.balance
write.table(mnp.balance, file="P:/NZR/NZR-001/Innovatieve Onderzoeksmethoden/2. Machine learning antibiotica bij ontstekingen/03. Dataverwerking/6. output R/20181221wqi/mnp.balance_type.csv", sep = ",")

#write.table(mnp.balance, file="H:/qww/AMC/Utrecht/NIVEL/writing/tables/mnps.balance_typeflu.csv", sep = ",")

plot(MNPS_cto_type, plots = 1) 
plot(MNPS_cto_type, plots = 2, subset = "es.mean")
plot(MNPS_cto_type, plots = 3) 
#Warning message:
#In plot.mnps(MNPS_cto_type, plots = 3) :
#Some effect sizes are larger than 3 and may not have been plotted.
plot(MNPS_cto_type, plots = 4) 
plot(MNPS_cto_type, plots = 5) 

data_confounders_to$wt <- get.weights(MNPS_cto_type, stop.method = "es.mean")

design.mnps <- svydesign(ids=~1, weights=~wt*outcome_weight, data=data_confounders_to) 

## survey
# model1
glm_type <- svyglm(outcome_4 ~ type_AB_nose, by=sex, design = design.mnps,family=quasibinomial)

summary(glm_type)

cbind( exp(coef(glm_type)), exp(summary(glm_type)$coefficients[,1] - 1.96*summary(glm_type)$coefficients[,2]), exp(summary(glm_type)$coefficients[,1] + 1.96*summary(glm_type)$coefficients[,2]) )


## survey
# model1
glm_type <- svyglm(outcome_4 ~ type_AB_nose, design = design.mnps,family=quasibinomial)

summary(glm_type)

cbind( exp(coef(glm_type)), exp(summary(glm_type)$coefficients[,1] - 1.96*summary(glm_type)$coefficients[,2]), exp(summary(glm_type)$coefficients[,1] + 1.96*summary(glm_type)$coefficients[,2]) )

# model 2
glm_type_part <- svyglm(outcome_4 ~ type_AB_nose + season + postalcode, design = design.mnps, family=quasibinomial)

summary(glm_type_part)

cbind( exp(coef(glm_type_part)), exp(summary(glm_type_part)$coefficients[,1] - 1.96*summary(glm_type_part)$coefficients[,2]), exp(summary(glm_type_part)$coefficients[,1] + 1.96*summary(glm_type_part)$coefficients[,2]) )
# model 2.1 np
glm_type_part_np <- svyglm(outcome_4 ~ type_AB_nose + season, design = design.mnps, family=quasibinomial)

summary(glm_type_part_np)

cbind( exp(coef(glm_type_part_np)), exp(summary(glm_type_part_np)$coefficients[,1] - 1.96*summary(glm_type_part_np)$coefficients[,2]), exp(summary(glm_type_part_np)$coefficients[,1] + 1.96*summary(glm_type_part_np)$coefficients[,2]) )

# model 3
glm_type_confounder <- svyglm(
  as.formula(paste("outcome_4 ~ type_AB_nose +",paste(vars_confounders,collapse="+"),sep="")), 
  design = design.mnps, family=quasibinomial)

summary(glm_type_confounder)

cbind( exp(coef(glm_type_confounder)), exp(summary(glm_type_confounder)$coefficients[,1] - 1.96*summary(glm_type_confounder)$coefficients[,2]), exp(summary(glm_type_confounder)$coefficients[,1] + 1.96*summary(glm_type_confounder)$coefficients[,2]) )

# model 4
glm_type_confounder_nm <- svyglm(
  as.formula(paste("outcome_4 ~ type_AB_nose +",paste(vars_confounders_nm,collapse="+"),sep="")), 
  design = design.mnps, family=quasibinomial)

summary(glm_type_confounder_nm)

cbind( exp(coef(glm_type_confounder_nm)), exp(summary(glm_type_confounder_nm)$coefficients[,1] - 1.96*summary(glm_type_confounder_nm)$coefficients[,2]), exp(summary(glm_type_confounder_nm)$coefficients[,1] + 1.96*summary(glm_type_confounder_nm)$coefficients[,2]) )

#agecat71018
###########################ctor
# %% running of multinomial propensity scores in <10y
###########################


MNPS_cto_type10y <- mnps(
  as.formula(paste("type_AB_nose ~ ",paste(vars_confounders_10y,collapse="+"),sep="")),
  data = data_confounders_to_10y,
  estimand = "ATE",
  verbose = FALSE,
  stop.method = c("es.mean", "ks.mean"),
  n.trees = 5000)
#Warning message:
#In ps(formula = currFormula, data = currDat, n.trees = n.trees[i],  :
#  Optimal number of iterations is close to the specified n.trees. n.trees is likely set too small and better balance might be obtainable by setting n.trees to be larger.
#2: In force(ordered) : restarting interrupted promise evaluation
#3: In factor(x) : restarting interrupted promise evaluation
summary(MNPS_cto_type10y$gbm.obj,
        n.trees=MNPS_cto_type10y$desc$ks.mean.ATE$n.trees,
        plot=TRUE)

summary(MNPS_cto_type10y)
mnp.balance10y<-bal.table(MNPS_cto_type10y)
mnp.balance10y
write.table(mnp.balance10y, file="P:/NZR/NZR-001/Innovatieve Onderzoeksmethoden/2. Machine learning antibiotica bij ontstekingen/03. Dataverwerking/6. output R/20190201wqi/mnp.balance_type10y.csv", sep = ",")

#write.table(mnp.balance, file="H:/qww/AMC/Utrecht/NIVEL/writing/tables/mnps.balance_typeflu.csv", sep = ",")

plot(MNPS_cto_type10y, plots = 1) 
plot(MNPS_cto_type10y, plots = 2, subset = "es.mean")
plot(MNPS_cto_type10y, plots = 3) 
#Warning message:
#In plot.mnps(MNPS_cto_type10y, plots = 3) :
#Some effect sizes are larger than 3 and may not have been plotted.
plot(MNPS_cto_type10y, plots = 4) 
plot(MNPS_cto_type10y, plots = 5) 

data_confounders_to10y <- mutate(data_confounders_to_10y,
                                wt10y=get.weights(MNPS_cto_type10y,stop.method = "es.mean" ))

design.mnps10y <- svydesign(ids=~1, weights = ~ wt10y*outcome_weight, data = data_confounders_to10y)

## survey
# model1
glm_type10y <- svyglm(outcome_4 ~ type_AB_nose, design = design.mnps10y,family=quasibinomial)

summary(glm_type10y)

cbind( exp(coef(glm_type10y)), exp(summary(glm_type10y)$coefficients[,1] - 1.96*summary(glm_type10y)$coefficients[,2]), exp(summary(glm_type10y)$coefficients[,1] + 1.96*summary(glm_type10y)$coefficients[,2]) )

# model 2
glm_type10y_part <- svyglm(outcome_4 ~ type_AB_nose + season, design = design.mnps10y, family=quasibinomial)

summary(glm_type10y_part)

cbind( exp(coef(glm_type10y_part)), exp(summary(glm_type10y_part)$coefficients[,1] - 1.96*summary(glm_type10y_part)$coefficients[,2]), exp(summary(glm_type10y_part)$coefficients[,1] + 1.96*summary(glm_type10y_part)$coefficients[,2]) )

# model 3
glm_type10y_confounder <- svyglm(
  as.formula(paste("outcome_4 ~ type_AB_nose +",paste(vars_confounders_10y,collapse="+"),sep="")), 
  design = design.mnps10y, family=quasibinomial)

summary(glm_type10y_confounder)

cbind( exp(coef(glm_type10y_confounder)), exp(summary(glm_type10y_confounder)$coefficients[,1] - 1.96*summary(glm_type10y_confounder)$coefficients[,2]), exp(summary(glm_type10y_confounder)$coefficients[,1] + 1.96*summary(glm_type10y_confounder)$coefficients[,2]) )

# model 4
glm_type10y_confounder_nm <- svyglm(
  as.formula(paste("outcome_4 ~ type_AB_nose +",paste(vars_confounders_10ynm,collapse="+"),sep="")), 
  design = design.mnps10y, family=quasibinomial)

summary(glm_type10y_confounder_nm)

cbind( exp(coef(glm_type10y_confounder_nm)), exp(summary(glm_type10y_confounder_nm)$coefficients[,1] - 1.96*summary(glm_type10y_confounder_nm)$coefficients[,2]), exp(summary(glm_type10y_confounder_nm)$coefficients[,1] + 1.96*summary(glm_type10y_confounder_nm)$coefficients[,2]) )



# model 2
glm_type10y_part <- svyglm(outcome_4 ~ type_AB_nose + season + postalcode + hartfalen_morb + nr_prescriptions_AB + nr_contacts_infection + poor_immune_response + nr_medication + COPD_morb + migraine_morb + sex, design = design.mnps10y, family=quasibinomial)

summary(glm_type10y_part)

cbind( exp(coef(glm_type10y_part)), exp(summary(glm_type10y_part)$coefficients[,1] - 1.96*summary(glm_type10y_part)$coefficients[,2]), exp(summary(glm_type10y_part)$coefficients[,1] + 1.96*summary(glm_type10y_part)$coefficients[,2]) )

# model 2.1 Multivariate Imputation

glm_type10y_part_mi <- svyglm(outcome_4 ~ type_AB_nose + season + postalcode + hartfalen_morb + nr_prescriptions_AB + nr_contacts_infection + poor_immune_response + nr_medication + COPD_morb + migraine_morb + sex, design = design.mnps10y_mi, family=quasibinomial)

summary(glm_type10y_part_mi)

cbind( exp(coef(glm_type10y_part_mi)), exp(summary(glm_type10y_part_mi)$coefficients[,1] - 1.96*summary(glm_type10y_part_mi)$coefficients[,2]), exp(summary(glm_type10y_part_mi)$coefficients[,1] + 1.96*summary(glm_type10y_part_mi)$coefficients[,2]) )


# model 2.2 no postalcode
glm_type10y_part_np <- svyglm(outcome_4 ~ type_AB_nose + season + hartfalen_morb + nr_prescriptions_AB + nr_contacts_infection + poor_immune_response + nr_medication + COPD_morb + migraine_morb + sex, design = design.mnps10y, family=quasibinomial)

summary(glm_type10y_part_np)

cbind( exp(coef(glm_type10y_part_np)), exp(summary(glm_type10y_part_np)$coefficients[,1] - 1.96*summary(glm_type10y_part_np)$coefficients[,2]), exp(summary(glm_type10y_part_np)$coefficients[,1] + 1.96*summary(glm_type10y_part_np)$coefficients[,2]) )

# model 2.3 Multivariate Imputation & no postalcode

glm_type10y_part_mi_np <- svyglm(outcome_4 ~ type_AB_nose + season + hartfalen_morb + nr_prescriptions_AB + nr_contacts_infection + poor_immune_response + nr_medication + COPD_morb + migraine_morb + sex, design = design.mnps10y_mi, family=quasibinomial)

summary(glm_type10y_part_mi_np)

cbind( exp(coef(glm_type10y_part_mi_np)), exp(summary(glm_type10y_part_mi_np)$coefficients[,1] - 1.96*summary(glm_type10y_part_mi_np)$coefficients[,2]), exp(summary(glm_type10y_part_mi_np)$coefficients[,1] + 1.96*summary(glm_type10y_part_mi_np)$coefficients[,2]) )

# model 3
glm_type10y_confounder <- svyglm(
  as.formula(paste("outcome_4 ~ type_AB_nose +",paste(vars_confounders,collapse="+"),sep="")), 
  design = design.mnps10y, family=quasibinomial)

summary(glm_type10y_confounder)

cbind( exp(coef(glm_type10y_confounder)), exp(summary(glm_type10y_confounder)$coefficients[,1] - 1.96*summary(glm_type10y_confounder)$coefficients[,2]), exp(summary(glm_type10y_confounder)$coefficients[,1] + 1.96*summary(glm_type10y_confounder)$coefficients[,2]) )


###########################ctor
# %% running of multinomial propensity scores in population exclude age<10
###########################


MNPS_cto_type10 <- mnps(
  as.formula(paste("type_AB_nose ~ ",paste(vars_confounders,collapse="+"),sep="")),
  data = subset(data_confounders_to, age>=10),
  estimand = "ATE",
  verbose = FALSE,
  stop.method = c("es.mean", "ks.mean"),
  n.trees = 5000)
#Warning message:
#In ps(formula = currFormula, data = currDat, n.trees = n.trees[i],  :
#  Optimal number of iterations is close to the specified n.trees. n.trees is likely set too small and better balance might be obtainable by setting n.trees to be larger.

summary(MNPS_cto_type10$gbm.obj,
        n.trees=MNPS_cto_type10$desc$ks.mean.ATE$n.trees,
        plot=TRUE)

summary(MNPS_cto_type10)
mnp.balance10<-bal.table(MNPS_cto_type10)
mnp.balance10
write.table(mnp.balance10, file="P:/NZR/NZR-001/Innovatieve Onderzoeksmethoden/2. Machine learning antibiotica bij ontstekingen/03. Dataverwerking/6. output R/20190102wqi/mnp.balance_type10.csv", sep = ",")

#write.table(mnp.balance, file="H:/qww/AMC/Utrecht/NIVEL/writing/tables/mnps.balance_typeflu.csv", sep = ",")

plot(MNPS_cto_type10, plots = 1) 
plot(MNPS_cto_type10, plots = 2, subset = "es.mean")
plot(MNPS_cto_type10, plots = 3) 
#Warning message:
#In plot.mnps(MNPS_cto_type10, plots = 3) :
#Some effect sizes are larger than 3 and may not have been plotted.
plot(MNPS_cto_type10, plots = 4) 
plot(MNPS_cto_type10, plots = 5) 

data_confounders_to10 <- mutate(subset(data_confounders_to, age>=10), 
                                wt10=get.weights(MNPS_cto_type10,stop.method = "es.mean" ))

design.mnps10 <- svydesign(ids=~1, weights = ~ wt10*outcome_weight, data = data_confounders_to10)

## survey
# model1
glm_type10 <- svyglm(outcome_4 ~ type_AB_nose, design = design.mnps10,family=quasibinomial)

summary(glm_type10)

cbind( exp(coef(glm_type10)), exp(summary(glm_type10)$coefficients[,1] - 1.96*summary(glm_type10)$coefficients[,2]), exp(summary(glm_type10)$coefficients[,1] + 1.96*summary(glm_type10)$coefficients[,2]) )

# model 2
glm_type10_part <- svyglm(outcome_4 ~ type_AB_nose + hartfalen_morb + postalcode, design = design.mnps10, family=quasibinomial)

summary(glm_type10_part)

cbind( exp(coef(glm_type10_part)), exp(summary(glm_type10_part)$coefficients[,1] - 1.96*summary(glm_type10_part)$coefficients[,2]), exp(summary(glm_type10_part)$coefficients[,1] + 1.96*summary(glm_type10_part)$coefficients[,2]) )
# model 2.1 no postalcode
glm_type10_part_np <- svyglm(outcome_4 ~ type_AB_nose + hartfalen_morb , design = design.mnps10, family=quasibinomial)

summary(glm_type10_part_np)

cbind( exp(coef(glm_type10_part_np)), exp(summary(glm_type10_part_np)$coefficients[,1] - 1.96*summary(glm_type10_part_np)$coefficients[,2]), exp(summary(glm_type10_part_np)$coefficients[,1] + 1.96*summary(glm_type10_part_np)$coefficients[,2]) )

# model 3
glm_type10_confounder <- svyglm(
  as.formula(paste("outcome_4 ~ type_AB_nose +",paste(vars_confounders,collapse="+"),sep="")), 
  design = design.mnps10, family=quasibinomial)

summary(glm_type10_confounder)

cbind( exp(coef(glm_type10_confounder)), exp(summary(glm_type10_confounder)$coefficients[,1] - 1.96*summary(glm_type10_confounder)$coefficients[,2]), exp(summary(glm_type10_confounder)$coefficients[,1] + 1.96*summary(glm_type10_confounder)$coefficients[,2]) )

# model 4
glm_type10_confounder_nm <- svyglm(
  as.formula(paste("outcome_4 ~ type_AB_nose +",paste(vars_confounders_nm,collapse="+"),sep="")), 
  design = design.mnps10, family=quasibinomial)

summary(glm_type10_confounder_nm)

cbind( exp(coef(glm_type10_confounder_nm)), exp(summary(glm_type10_confounder_nm)$coefficients[,1] - 1.96*summary(glm_type10_confounder_nm)$coefficients[,2]), exp(summary(glm_type10_confounder_nm)$coefficients[,1] + 1.96*summary(glm_type10_confounder_nm)$coefficients[,2]) )

###########################ctor
# %% running of multinomial propensity scores in population  age<10
###########################


MNPS_cto_type10y <- mnps(
  as.formula(paste("type_AB_nose ~ ",paste(vars_confounders,collapse="+"),sep="")),
  data = subset(data_confounders_to, age<10),
  estimand = "ATE",
  verbose = FALSE,
  stop.method = c("es.mean", "ks.mean"),
  n.trees = 5000)
#Warning message:
#In ps(formula = currFormula, data = currDat, n.trees = n.trees[i],  :
#  Optimal number of iterations is close to the specified n.trees. n.trees is likely set too small and better balance might be obtainable by setting n.trees to be larger.

summary(MNPS_cto_type10y$gbm.obj,
        n.trees=MNPS_cto_type10y$desc$ks.mean.ATE$n.trees,
        plot=TRUE)

summary(MNPS_cto_type10y)
mnp.balance10<-bal.table(MNPS_cto_type10y)
mnp.balance10
write.table(mnp.balance10, file="P:/NZR/NZR-001/Innovatieve Onderzoeksmethoden/2. Machine learning antibiotica bij ontstekingen/03. Dataverwerking/6. output R/20190125wqi/mnp.balance_type10y.csv", sep = ",")

#write.table(mnp.balance, file="H:/qww/AMC/Utrecht/NIVEL/writing/tables/mnps.balance_typeflu.csv", sep = ",")

plot(MNPS_cto_type10y, plots = 1) 
plot(MNPS_cto_type10y, plots = 2, subset = "es.mean")
plot(MNPS_cto_type10y, plots = 3) 
#Warning message:
#In plot.mnps(MNPS_cto_type10y, plots = 3) :
#Some effect sizes are larger than 3 and may not have been plotted.
plot(MNPS_cto_type10y, plots = 4) 
plot(MNPS_cto_type10y, plots = 5) 

data_confounders_to10y <- mutate(data_confounders_to_10y,  
                                 wt10y=get.weights(MNPS_cto_type10y,stop.method = "es.mean" ))

design.mnps10y <- svydesign(ids=~1, weights = ~ wt10y*outcome_weight, data = data_confounders_to10y)

## survey
# model1
glm_type10y <- svyglm(outcome_4 ~ type_AB_nose, design = design.mnps10y,family=quasibinomial)

summary(glm_type10y)

cbind( exp(coef(glm_type10y)), exp(summary(glm_type10y)$coefficients[,1] - 1.96*summary(glm_type10y)$coefficients[,2]), exp(summary(glm_type10y)$coefficients[,1] + 1.96*summary(glm_type10y)$coefficients[,2]) )

# model 2
glm_type10y_part <- svyglm(outcome_4 ~ type_AB_nose + season, design = design.mnps10y, family=quasibinomial)

summary(glm_type10y_part)

cbind( exp(coef(glm_type10y_part)), exp(summary(glm_type10y_part)$coefficients[,1] - 1.96*summary(glm_type10y_part)$coefficients[,2]), exp(summary(glm_type10y_part)$coefficients[,1] + 1.96*summary(glm_type10y_part)$coefficients[,2]) )

# model 3
glm_type10y_confounder <- svyglm(
  as.formula(paste("outcome_4 ~ type_AB_nose +",paste(vars_confounders_10y,collapse="+"),sep="")), 
  design = design.mnps10y, family=quasibinomial)

summary(glm_type10y_confounder)

cbind( exp(coef(glm_type10y_confounder)), exp(summary(glm_type10y_confounder)$coefficients[,1] - 1.96*summary(glm_type10y_confounder)$coefficients[,2]), exp(summary(glm_type10y_confounder)$coefficients[,1] + 1.96*summary(glm_type10y_confounder)$coefficients[,2]) )



###########################
# ITE
# %% synthetic random forest
###########################
setwd("P:/NZR/NZR-001/Innovatieve Onderzoeksmethoden/2. Machine learning antibiotica bij ontstekingen/03. Dataverwerking/6. output R/20190125wqi/")
### treatment using subset data with only no treatment AB 
AB_0 <- data_first_treatment_12ow_relevant_0.4%>% filter (type_AB_nose == 0) ## control

regF_0 <- rfsrcSyn(
  as.formula(paste("outcome_4 ~ ", paste(vars_confounders, collapse="+"),sep="")),
  data  = AB_0)

### using that syntetic forest to predict outcome of all participants (so wether they had treatment of not)

pred.Syn_type_0 <- rfsrcSyn(object = regF_0, 
                            na.action = "na.impute",
                            newdata = data_first_treatment_12ow_relevant_0.4)


### treatment using subset data with treatment AB 
AB_1 <- data_first_treatment_12ow_relevant_0.4%>% filter (AB_nose_infection == 1)

regF_1 <- rfsrcSyn(
  as.formula(paste("outcome_4 ~ ", paste(vars_confounders, collapse="+"),sep="")),
  data  = AB_1)

pred.Syn_AB_1 <- rfsrcSyn(object = regF_1, 
                          na.action = "na.impute",
                          newdata = data_first_treatment_12ow_relevant_0.4)

# calculate difference
data_first_treatment_12ow_relevant_0.4$delta <- (pred.Syn_AB_1$rfSynPred$predicted -  pred.Syn_type_0$rfSynPred$predicted)[,2]


fit <- glm(
  as.formula(paste("delta ~ ", paste(vars_confounders, collapse="+"),sep="")),
  data  = data_first_treatment_12ow_relevant_0.4, family = gaussian)

fit_delta<- summary(fit)
fit_delta

variable <- names(fit$coefficients)
coef <- coef(fit_delta)[,1]
p_value <- coef(fit_delta)[,4]

table_AB = data.frame(variable, coef, p_value)
write.csv(table_AB, file='fit_delta.csv')

fit_np <- glm(
  as.formula(paste("delta ~ ", paste(vars_confounders_np, collapse="+"),sep="")),
  data  = data_first_treatment_12ow_relevant_0.4, family = gaussian)

fit_np_delta<- summary(fit_np)
fit_np_delta

variable_np <- names(fit_np$coefficients)
coef_np <- coef(fit_np_delta)[,1]
p_value_np <- coef(fit_np_delta)[,4]

table_AB_np = data.frame(variable_np, coef_np, p_value_np)
write.csv(table_AB_np, file='fit_np_delta.csv')  

#-delta
fit_np_m <- glm(
  as.formula(paste("-delta ~ ", paste(vars_confounders_np, collapse="+"),sep="")),
  data  = data_first_treatment_12ow_relevant_0.4, family = gaussian)

fit_np_delta_m<- summary(fit_np_m)
fit_np_delta_m

variable_np_m <- names(fit_np_m$coefficients)
coef_np_m <- coef(fit_np_delta_m)[,1]
p_value_np_m <- coef(fit_np_delta_m)[,4]

table_AB_np_m = data.frame(variable_np_m, coef_np_m, p_value_np_m)
write.csv(table_AB_np_m, file='fit_np_delta_m.csv')  






fit_nm <- glm(
  as.formula(paste("delta ~ ", paste(vars_confounders_nm, collapse="+"),sep="")),
  data  = data_first_treatment_12ow_relevant_0.4, family = gaussian)

fit_nm_delta<- summary(fit_nm)
fit_nm_delta

variable_nm <- names(fit_nm$coefficients)
coef_nm <- coef(fit_nm_delta)[,1]
p_value_nm <- coef(fit_nm_delta)[,4]

table_AB_nm = data.frame(variable_nm, coef_nm, p_value_nm)
write.csv(table_AB_nm, file='fit_nm_delta.csv')  

fit_npm <- glm(
  as.formula(paste("delta ~ ", paste(vars_confounders_npm, collapse="+"),sep="")),
  data  = data_first_treatment_12ow_relevant_0.4, family = gaussian)

fit_npm_delta<- summary(fit_npm)
fit_npm_delta

variable_npm <- names(fit_npm$coefficients)
coef_npm <- coef(fit_npm_delta)[,1]
p_value_npm <- coef(fit_npm_delta)[,4]

table_AB_npm = data.frame(variable_npm, coef_npm, p_value_npm)
write.csv(table_AB_npm, file='fit_npm_delta.csv')    

#anova(fit,fit_np,fit_nm,fit_npm,test="Chisq")
#anova(fit,fit_np)
#anova(fit,fit_nm,test="Chisq")
#anova(fit_nm,fit_nmp)

###loop for type 1-5

for (j in 1:5) {
  
  AB_j <- data_first_treatment_12ow_relevant_0.4%>% filter (type_AB_nose == j)
  
  regF_j <- rfsrcSyn(
    as.formula(paste("outcome_4 ~ ", paste(vars_confounders, collapse="+"),sep="")),
    data  = AB_j)
  
  pred.Syn_type_j <- rfsrcSyn(object = regF_j, na.action = "na.impute", newdata = data_first_treatment_12ow_relevant_0.4)
  # calculate difference
  data_first_treatment_12ow_relevant_0.4$delta_j <- (pred.Syn_type_j$rfSynPred$predicted -  pred.Syn_type_0$rfSynPred$predicted)[,2]
  
  
  fit_j <- glm(
    as.formula(paste(" delta_j ~ ", paste(vars_confounders, collapse="+"),sep="")),
    data  = data_first_treatment_12ow_relevant_0.4, family = gaussian)
  fit_delta_j<- summary(fit_j)
  
  variable <- names(fit_j$coefficients)
  coef <- coef(fit_delta_j)[,1]
  p_value <- coef(fit_delta_j)[,4]
  
  table = data.frame(variable, coef, p_value)
  write.csv(table, file=sprintf('fit_delta_%s.csv',j))
  # write.csv(table, file=sprintf('H:/qww/AMC/Utrecht/NIVEL/Thamar/20180823/fit_delta_%s.csv',j))
  
  data_first_treatment_12ow_relevant_0.4[,ncol(data_first_treatment_12ow_relevant_0.4)+1]<-data_first_treatment_12ow_relevant_0.4$delta_j
  names(data_first_treatment_12ow_relevant_0.4)[ncol(data_first_treatment_12ow_relevant_0.4)]<-paste0("delta_",j)
}



###########################
# %% subgroup analysis
###########################



# PRIM with delta coming from binary AB
PRIM <- subset(data_first_treatment_12ow_relevant_0.4, select = c(age, sex, nr_medication,COPD_morb, migraine_morb, nr_contacts_resp, osteop_morb,poor_immune_response, alcmisb_morb,practice_size, Depressie_morb,aangaf_morb,hartfalen_morb,flu_epi,season,postalcode, nr_contacts_infection, nr_prescriptions_AB, delta,ID))

prim.table<-data.frame(matrix(ncol=(6)))
colnames(prim.table)<-c("alpha","beta","number_cov","rule_cov","number_div","rule_div")

set.seed(1234)

for (i in 5:10){
  alpha<-i/100
  for (j in 1:10){
    beta<-j/100
    p.cov<-subgroup.discovery::prim.cover(delta~., data = PRIM,
                                          peeling.quantile=alpha,
                                          min.support=beta, 
                                          plot= FALSE, 
                                          optimal.box= "2se")
    rule_cov<-p.cov$covers[[1]]$superrule
    #summary(p.cov)
    p.div <- prim.diversify(delta ~ .,
                            data = PRIM,
                            n = 4,
                            peeling.quantile = alpha,
                            min.support = beta,
                            plot = FALSE,
                            optimal.box = "2se")
    rule_div<-p.div$attempts[[1]]$superrule
    # summary(p.div)
    
    prim.table[10*i+j-10,]<-c(alpha,beta,length(rule_cov),toString(rule_cov),length(rule_div),toString(rule_div))
    
  }
}

#Error in combn(1:nr.of.attempts, 2) : n < m

write.csv(prim.table, file='prim.table.csv')  

p.cov<-subgroup.discovery::prim.cover(delta~., data = PRIM,
                                      peeling.quantile=0.05,
                                      min.support=0.1, 
                                      plot= TRUE, 
                                      optimal.box= "2se")
rule_cov<-p.cov$covers[[1]]$superrule
summary(p.cov)






# PRIM with delta coming from binary AB without postalcode
PRIM_np <- subset(data_first_treatment_12ow_relevant_0.4, select = c(age, sex, nr_medication,COPD_morb, migraine_morb, nr_contacts_resp, osteop_morb,poor_immune_response, alcmisb_morb,practice_size, Depressie_morb,aangaf_morb,hartfalen_morb,flu_epi,season, nr_contacts_infection, nr_prescriptions_AB, delta, ID))
PRIM_np_dummy <- subset(data_first_treatment_12ow_relevant_0.4, select = c(age, sex, nr_medication,COPD_morb, migraine_morb, nr_contacts_resp, osteop_morb,poor_immune_response, alcmisb_morb,practice_size, Depressie_morb,hartfalen_morb,flu_epi,season, nr_contacts_infection, nr_prescriptions_AB, delta, ID))

prim.table_np<-data.frame(matrix(ncol=(6)))
colnames(prim.table_np)<-c("alpha","beta","number_cov","rule_cov","number_div","rule_div")

set.seed(1234)

for (i in 5:10){
  alpha<-i/100
  for (j in 1:10){
    beta<-j/100
    p.cov<-subgroup.discovery::prim.cover(delta~., data = PRIM_np,
                                          peeling.quantile=alpha,
                                          min.support=beta, 
                                          plot= FALSE, 
                                          optimal.box= "2se")
    rule_cov<-p.cov$covers[[1]]$superrule
    p.div <- prim.diversify(delta ~ .,
                            data = PRIM_np,
                            n = 4,
                            peeling.quantile = alpha,
                            min.support = beta,
                            plot = FALSE,
                            optimal.box = "2se")
    rule_div<-p.div$attempts[[1]]$superrule
    
    prim.table_np[10*i+j-10,]<-c(alpha,beta,length(rule_cov),toString(rule_cov),length(rule_div),toString(rule_div))
    
  }
}
write.csv(prim.table_np, file='prim.table_np.csv')  

#Prevalence of the original population
m_average<-mean(PRIM$delta,na.rm=TRUE)
#0.0267812

####put different rules into subset automatically & put the string of rule into a vector

#prim.table.r_np<-read.csv(file = "H:/qww/AMC/Utrecht/NIVEL/Thamar/20190102/prim.table_np.csv")
#Prevalence and coverage !!!!!
for (i in 1:100){
  
  myString <- paste("subset(PRIM_np,", 
                    paste(gsub(',','&',prim.table_np$rule_cov[i]), 
                          ")"
                    ))# this will glue three strings together
  
  a<-eval(parse(text=myString))  # now we tell R to look at the string as a command and execute it
  
  box<- ifelse(PRIM_np$ID %in% a$ID,1,0) 
  
 # PRIM_np[,ncol(PRIM_np)+1]<-PRIM_np$box
 # names(PRIM_np)[ncol(PRIM_np)]<-paste0("box_",i)
  
  m_delta<-sum(subset(PRIM_np, box==1)$delta,na.rm=TRUE)/nrow(a)
 # m_delta<-sum(subset(PRIM_np, box==1)$delta,na.rm=TRUE)/nrow(PRIM_np)     confirm denominator!!!
  
  
  prim.table_np$m_delta[i]<-m_delta
  
  betat <-nrow(a)/nrow(PRIM_np)
  prim.table_np$betat[i]<- betat
  
 # prim.table_np$coverage1[i]<-(m_delta-m_average)*betat
  prim.table_np$coverage2[i]<-(m_average-m_delta)*betat
  
  write.csv( prim.table_np, file='prim.table_np_coverage.csv')
  
}

set.seed(1234)
p.cov<-subgroup.discovery::prim.cover(delta~. -ID, data = PRIM_np,
                                      peeling.quantile=0.05,
                                      min.support=0.07, 
                                      plot= TRUE, 
                                      optimal.box= "best")
summary(p.cov)

set.seed(1234)
p.cov_m<-subgroup.discovery::prim.cover(-delta~. -ID, data = PRIM_np,
                                      peeling.quantile=0.05,
                                      min.support=0.04, 
                                      plot= TRUE, 
                                      optimal.box= "best")
summary(p.cov_m)

#p.cov_m1<-subgroup.discovery::prim.cover(delta~. -ID, data = PRIM_np,
                                        peeling.quantile=0.05,
                                        min.support=0.04, 
                                        plot= TRUE, 
                                        minimize= TRUE,
                                        optimal.box= "best")
#summary(p.cov_m1)


rule1<-p.cov$covers[[1]]$superrule
rule2<-p.cov$covers[[2]]$superrule
rule3<-p.cov$covers[[3]]$superrule
rule4<-p.cov$covers[[4]]$superrule
rule5<-p.cov$covers[[5]]$superrule

rule6<-p.cov_m$covers[[1]]$superrule
rule7<-p.cov_m$covers[[2]]$superrule
rule8<-p.cov_m$covers[[3]]$superrule


#interst box

#PRIM_np_box<-data.frame(matrix(ncol=20))
#colnames(PRIM_np_box)<-colnames(PRIM_np)

myString1 <- paste("subset(PRIM_np,", 
                   paste(gsub(',','&', toString(rule1)), 
                         ")"
                   ))     # this will glue three strings together
myString2 <- paste("subset(PRIM_np,", 
                   paste(gsub(',','&', toString(rule2)), 
                         ")"
                   ))     # this will glue three strings together
myString3 <- paste("subset(PRIM_np,", 
                   paste(gsub(',','&', toString(rule3)), 
                         ")"
                   ))     # this will glue three strings together
myString4 <- paste("subset(PRIM_np,", 
                   paste(gsub(',','&', toString(rule4)), 
                         ")"
                   ))     # this will glue three strings together
myString5 <- paste("subset(PRIM_np,", 
                   paste(gsub(',','&', toString(rule5)), 
                         ")"
                   ))     # this will glue three strings together

myString6 <- paste("subset(PRIM_np,", 
                   paste(gsub(',','&', toString(rule6)), 
                         ")"
                   ))     # this will glue three strings together
myString7 <- paste("subset(PRIM_np,", 
                   paste(gsub(',','&', toString(rule7)), 
                         ")"
                   ))     # this will glue three strings together
myString8 <- paste("subset(PRIM_np,", 
                   paste(gsub(',','&', toString(rule8)), 
                         ")"
                   ))     # this will glue three strings together

# a is the subset that follow the rule

a1<-eval(parse(text=myString1))  # now we tell R to look at the string as a command and execute it
#risk_data1<-rbind(a,risk_data)
a2<-eval(parse(text=myString2))  # now we tell R to look at the string as a command and execute it
#risk_data1<-rbind(a,risk_data)
a3<-eval(parse(text=myString3))  # now we tell R to look at the string as a command and execute it
#risk_data1<-rbind(a,risk_data)
a4<-eval(parse(text=myString4))  # now we tell R to look at the string as a command and execute it
#risk_data1<-rbind(a,risk_data)
a5<-eval(parse(text=myString5))  # now we tell R to look at the string as a command and execute it
#risk_data1<-rbind(a,risk_data)
a6<-eval(parse(text=myString6))  # now we tell R to look at the string as a command and execute it
#risk_data1<-rbind(a,risk_data)
a7<-eval(parse(text=myString7))  # now we tell R to look at the string as a command and execute it
#risk_data1<-rbind(a,risk_data)
a8<-eval(parse(text=myString8))  # now we tell R to look at the string as a command and execute it
#risk_data1<-rbind(a,risk_data)



PRIM_np$rule1<- as.factor(ifelse(PRIM_np$ID %in% a1$ID,1,0) )
PRIM_np$rule2<- as.factor(ifelse(PRIM_np$ID %in% a2$ID,1,0) )
PRIM_np$rule3<- as.factor(ifelse(PRIM_np$ID %in% a3$ID,1,0) )
PRIM_np$rule4<- as.factor(ifelse(PRIM_np$ID %in% a4$ID,1,0) )
PRIM_np$rule5<- as.factor(ifelse(PRIM_np$ID %in% a5$ID,1,0) )
PRIM_np$rule6<- as.factor(ifelse(PRIM_np$ID %in% a6$ID,1,0) )
PRIM_np$rule7<- as.factor(ifelse(PRIM_np$ID %in% a7$ID,1,0) )
PRIM_np$rule8<- as.factor(ifelse(PRIM_np$ID %in% a8$ID,1,0) )
str(PRIM_np)

# TAKE the box with different rules as variables to glm

#AIC to select significant variables
fit_rules <- glm( delta ~., data  = select(PRIM_np, -ID), family = gaussian)
# Select most predictive variables
AIC <- stepAIC(fit_rules)
# FYI show formula of the final model
formula(AIC)

summary(fit_rules)
coefficients <-summary.glm(fit_rules)$coefficients

write.csv( coefficients , file='fit_rules.csv')





# TAKE the box with max_coverage as a variable to glm
fit_max_coverage <- glm( delta ~., data  = select(PRIM_np, -ID), family = gaussian)
   
coefficients <-summary.glm(fit_max_coverage)$coefficients
write.csv( coefficients , file='glm_fit_max_coverage.csv')

fit_max_coverage <- glm(
  delta  ~  age+ sex+ nr_medication+ COPD_morb+
  migraine_morb+  nr_contacts_resp+ osteop_morb+ poor_immune_response+ alcmisb_morb+ practice_size
  + flu_epi+ season+ nr_contacts_infection+ nr_prescriptions_AB
  +Depressie_morb+ hartfalen_morb
,
  data  = select(PRIM_np, -ID),
  family=gaussian)

# delete:aangaf_morb
m_delta_t<-mean(PRIM_np$delta,na.rm=TRUE)
get_round_table <- function(x,y,z){ 
  #repeat prim in unintersted group
  m_delta_t<-mean(PRIM_np$delta,na.rm=TRUE)
  round.table <-data.frame(matrix(ncol=(8)))
  colnames(round.table)<-c("alpha","beta","rule","mean","betas","coverage","patient number", "number of rounds")
  num_t<-nrow(PRIM_np)
   round = 0
  for (i in x){
    alpha<-i/100
    for (j in y){ 
      beta<-j/100
      #for(r in 0:3){
      #  cat("iteratio: ", r, "\n")
     # PRIM_np<-raw_data()
      interest_data<-data.frame(matrix(ncol=19))
      colnames(interest_data)<-colnames(PRIM_np)
      temp <- PRIM_np
     # repeat{
        #set.seed(1234)
        subg<-subgroup.discovery::prim.cover(delta~.-ID , data = PRIM_np,
                                             peeling.quantile=alpha,
                                             min.support=beta, 
                                             plot= FALSE, 
                                             optimal.box= "2se")
        rule<-subg$covers[[1]]$superrule
        
        myString <- paste("subset(PRIM_np,", 
                          paste(gsub(',','&', toString(rule)), 
                                ")"
                          ))# this will glue three strings together
        # a is the subset of PRIM_np that follow the rule
        a<-eval(parse(text=myString))  # now we tell R to look at the string as a command and execute it
       interest_data<-rbind(a,interest_data)   
       
        box<- ifelse(PRIM_np$ID %in% a$ID,1,0) 
        
        # PRIM_np[,ncol(PRIM_np)+1]<-PRIM_np$box
        # names(PRIM_np)[ncol(PRIM_np)]<-paste0("box_",i)
        
        #Calculate metrics
        num<-nrow(a)
       # m_delta<-sum(subset(PRIM_np, box==1)$delta, na.rm=TRUE)/nrow(a)
        m_delta<-sum(a$delta, na.rm=TRUE)/nrow(a)
        # m_delta<-sum(subset(PRIM_np, box==1)$delta,na.rm=TRUE)/nrow(PRIM_np)     confirm denominator!!!
     
        betas<-nrow(a)/nrow(PRIM_np)
        coverage<-(m_delta_t-m_delta)*betas
        other <- PRIM_np[!(PRIM_np$ID  %in% a$ID),]  #subset(PRIM_np, box==0)
        PRIM_np <- other
       
        round.table [r+1,]<-c(alpha,beta,toString(rule),m_delta,betas,coverage,num,round)
      
        #if (m_delta >= 1.5*m_delta_t)
      #    {round.table$`number of rounds`[10*i+j-10] <- round  #???
      #  break
     # }
   # }  
        
        beta<-(j/100)*num_t/nrow(other)
        round = round + 1
        cat("nrow all: ", nrow(temp), " ; mean: ", mean(temp$delta), "\n")
            cat("nrow subgroup: ", nrow(a), " ; mean: ", mean(a$delta), "\n")
        
 # }  
  if (z==1) 
    return (round.table)
  else{
    if (z==0)
      return (interest_data)
    else{
      print("Please input 0 or 1")
    }
  }
  
  } 
  } 
  }  

interest_data<-get_round_table(0) 
round.table<-get_round_table(5,10,1) 
write.csv(round.table, file = "round.table.csv")
round.table7<-get_round_table(5,7,1) 
interest_data7<-get_round_table(5,7,0)
write.csv(round.table7, file = "round.table7.csv")
write.csv(interest_data7, file = "interest_data7.csv")

round.table7<-read.csv( file = "round.table7.csv", na.strings=c("NA","NaN", " ",""), colClasses="factor")

#1
round.table7$rule[1]

myString_1 <- paste("subset(PRIM_np,", 
                    paste(gsub(',','&', round.table7$rule[1]), 
                          "& ID)"
                    ))     # this will glue three strings together
# a_1 is the subset that follow the rule

a_1<-eval(parse(text=myString_1))  # now we tell R to look at the string as a command and execute it
#interest_data1<-rbind(a,interest_data)

PRIM_np$max_coverage_1<- ifelse(PRIM_np$ID %in% a_1$ID,1,0) 
#2
round.table7$rule[2]

myString_2 <- paste("subset(PRIM_np,", 
                    paste(gsub(',','&', round.table7$rule[2]), 
                          "& ID)"
                    ))     # this will glue three strings together
# a_2 is the subset that follow the rule

a_2<-eval(parse(text=myString_2))  # now we tell R to look at the string as a command and execute it
#interest_data2<-rbind(a,interest_data)

PRIM_np$max_coverage_2<- ifelse(PRIM_np$ID %in% a_2$ID,1,0) 

#3
round.table7$rule[3]

myString_3 <- paste("subset(PRIM_np,", 
                    paste(gsub(',','&', round.table7$rule[3]), 
                          "& ID)"
                    ))     # this will glue three strings together
# a_3 is the subset that follow the rule

a_3<-eval(parse(text=myString_3))  # now we tell R to look at the string as a command and execute it
#interest_data3<-rbind(a,interest_data)

PRIM_np$max_coverage_3<- ifelse(PRIM_np$ID %in% a_3$ID,1,0) 




str(PRIM_np)

# TAKE the box with max_coverage as variables to glm


#AIC to select significant variables
fit_max_coverage <- glm( delta ~., data  = select(PRIM_np, -ID), family = gaussian)
# Select most predictive variables
AIC <- stepAIC(fit_max_coverage)
# FYI show formula of the final model
formula(AIC)

summary(fit_max_coverage)
coefficients <-summary.glm(fit_max_coverage)$coefficients
write.csv( coefficients , file='AIC_max_coverage.csv')




# PRIM with - delta coming from binary AB without postalcode  
PRIM_np <- subset(data_first_treatment_12ow_relevant_0.4, select = c(age, sex, nr_medication,COPD_morb, migraine_morb, nr_contacts_resp, osteop_morb,poor_immune_response, alcmisb_morb,practice_size, Depressie_morb,aangaf_morb,hartfalen_morb,flu_epi,season, nr_contacts_infection, nr_prescriptions_AB, delta, ID))

prim.table_np_mdelta<-data.frame(matrix(ncol=(6)))
colnames(prim.table_np_mdelta)<-c("alpha","beta","number_cov","rule_cov","number_div","rule_div")

set.seed(1234)

for (i in 5:10){
  alpha<-i/100
  for (j in 1:10){
    beta<-j/100
    p.cov<-subgroup.discovery::prim.cover(-delta~.-max_coverage, data = PRIM_np,
                                          peeling.quantile=alpha,
                                          min.support=beta, 
                                          plot= FALSE, 
                                          optimal.box= "2se")
    rule_cov<-p.cov$covers[[1]]$superrule
    p.div <- prim.diversify(-delta ~ . -max_coverage,
                            data = PRIM_np,
                            n = 4,
                            peeling.quantile = alpha,
                            min.support = beta,
                            plot = FALSE,
                            optimal.box = "2se")
    rule_div<-p.div$attempts[[1]]$superrule
    
    prim.table_np_mdelta[10*i+j-10,]<-c(alpha,beta,length(rule_cov),toString(rule_cov),length(rule_div),toString(rule_div))
    
  }
}
#Error in combn(1:nr.of.attempts, 2) : n < m

write.csv(prim.table_np_mdelta, file='prim.table_np_minus.csv')  

#lapply(split(my$symbol, my$character), function(x)
 # if(length(x)>1) {
 #   combn(x, 2, simplify=FALSE)
 # }
 # else x)


#Prevalence of the original population
m_average_MINUS<- -mean(PRIM_np$delta,na.rm=TRUE)

####put different rules into subset automatically & put the string of rule into a vector

#prim.table.r_np<-read.csv(file = "H:/qww/AMC/Utrecht/NIVEL/Thamar/20190102/prim.table_np.csv")

#Prevalence and coverage !!!!!
for (i in 1:48){
  
  myString <- paste("subset(PRIM_np,", 
                    paste(gsub(',','&',prim.table_np_mdelta$rule_cov[i]), 
                          ")"
                    ))# this will glue three strings together
  
  a<-eval(parse(text=myString))  # now we tell R to look at the string as a command and execute it
  
  box<- ifelse(PRIM_np$ID %in% a$ID,1,0) 
  
  #PRIM_np[,ncol(PRIM_np)+1]<-PRIM_np$box
  #names(PRIM_np)[ncol(PRIM_np)]<-paste0("box_",i)
  
  m_delta<-sum(subset(PRIM_np, box==1)$delta,na.rm=TRUE)/nrow(a)
  
  prim.table_np_mdelta$m_delta[i]<-m_delta
  
  betat <-nrow(a)/nrow(PRIM_np)
  prim.table_np_mdelta$betat[i]<- betat
  
  prim.table_np_mdelta$coverage1[i]<-(m_delta-m_average)*betat

  
  write.csv( prim.table_np_mdelta, file='prim.table_np_coverage_m.csv')
  
}

set.seed(1234)
p.cov_m<-subgroup.discovery::prim.cover(-delta~., data = PRIM_np,
                                        peeling.quantile=0.05,
                                        min.support=0.04, 
                                        plot= TRUE, 
                                        optimal.box= "2se")


rule_cov_m <- p.cov_m$covers[[1]]$superrule
summary(p.cov_m)

#interst box

#interest_data_m<-data.frame(matrix(ncol=20))
#colnames(interest_data_m)<-colnames(PRIM_np)

rule_m<-p.cov_m$covers[[1]]$superrule

myString_m <- paste("subset(PRIM_np,", 
                   paste(gsub(',','&', toString(rule_m)), 
                         "& ID)"
                   ))     # this will glue three strings together
# a_m is the subset that follow the rule

a_m<-eval(parse(text=myString_m))  # now we tell R to look at the string as a command and execute it
#interest_data1<-rbind(a,interest_data)

PRIM_np$max_coverage_m<- ifelse(PRIM_np$ID %in% a_m$ID,1,0) 


str(PRIM_np)

# TAKE the box with max_coverage as a variable to glm



fit_max_coverage_m <- glm( -delta ~., data  = select(PRIM_np, -ID), family = gaussian)
summary(fit_max_coverage_m)
coefficients_m <-summary.glm(fit_max_coverage_m)$coefficients
write.csv( coefficients_m , file='glmm_fit_max_coverage.csv')


fit_max_coverage_m1 <- glm( -delta ~., data  = select(PRIM_np, -c(`ID`,`max_coverage`)), family = gaussian)
summary(fit_max_coverage_m1)
coefficients_m1 <-summary.glm(fit_max_coverage_m1)$coefficients
write.csv( coefficients_m1 , file='glmm_fit_max_coverage1.csv')


























# PRIM with delta coming from binary AB without NA variables:  nr_contacts_resp,nr_contacts_infection, nr_prescriptions_AB
PRIM_nm <- subset(data_first_treatment_12ow_relevant_0.4, select = c(age, sex, nr_medication,COPD_morb, migraine_morb, osteop_morb,poor_immune_response, alcmisb_morb,practice_size, Depressie_morb,aangaf_morb,hartfalen_morb,flu_epi,season,postalcode,  delta))

prim.table_nm<-data.frame(matrix(ncol=(6)))
colnames(prim.table_nm)<-c("alpha","beta","number_cov","rule_cov","number_div","rule_div")

set.seed(1234)

for (i in 1:10){
  alpha<-i/100
  for (j in 1:10){
    beta<-j/100
    p.cov<-subgroup.discovery::prim.cover(delta~., data = PRIM_nm,
                                          peeling.quantile=alpha,
                                          min.support=beta, 
                                          plot= FALSE, 
                                          optimal.box= "2se")
    rule_cov<-p.cov$covers[[1]]$superrule
    p.div <- prim.diversify(delta ~ .,
                            data = PRIM_nm,
                            n = 4,
                            peeling.quantile = alpha,
                            min.support = beta,
                            plot = FALSE,
                            optimal.box = "2se")
    rule_div<-p.div$attempts[[1]]$superrule
    
    prim.table_nm[10*i+j-10,]<-c(alpha,beta,length(rule_cov),toString(rule_cov),length(rule_div),toString(rule_div))
    
  }
}
write.csv(prim.table_nm, file='prim.table_nm.csv')  

# PRIM with delta coming from binary AB without postalcode & NA variables:  nr_contacts_resp,nr_contacts_infection, nr_prescriptions_AB,postalcode
PRIM_npm <- subset(data_first_treatment_12ow_relevant_0.4, select = c(age, sex, nr_medication,COPD_morb, migraine_morb, osteop_morb,poor_immune_response, alcmisb_morb,practice_size, Depressie_morb,aangaf_morb,hartfalen_morb,flu_epi,season, delta))

prim.table_npm<-data.frame(matrix(ncol=(6)))
colnames(prim.table_npm)<-c("alpha","beta","number_cov","rule_cov","number_div","rule_div")

set.seed(1234)

for (i in 1:10){
  alpha<-i/100
  for (j in 1:10){
    beta<-j/100
    p.cov<-subgroup.discovery::prim.cover(delta~., data = PRIM_nm,
                                          peeling.quantile=alpha,
                                          min.support=beta, 
                                          plot= FALSE, 
                                          optimal.box= "2se")
    rule_cov<-p.cov$covers[[1]]$superrule
    p.div <- prim.diversify(delta ~ .,
                            data = PRIM_nm,
                            n = 4,
                            peeling.quantile = alpha,
                            min.support = beta,
                            plot = FALSE,
                            optimal.box = "2se")
    rule_div<-p.div$attempts[[1]]$superrule
    
    prim.table_npm[10*i+j-10,]<-c(alpha,beta,length(rule_cov),toString(rule_cov),length(rule_div),toString(rule_div))
    
  }
}
write.csv(prim.table_npm, file='prim.table_npm.csv') 

# PRIM with delta coming from  AB TYPE 0 VS 1
PRIM_1 <- subset(data_first_treatment_12ow_relevant_0.4, select = c(age, sex, nr_medication,COPD_morb, migraine_morb, nr_contacts_resp, osteop_morb,poor_immune_response, alcmisb_morb,practice_size, Depressie_morb,aangaf_morb,hartfalen_morb,flu_epi,season,postalcode, nr_contacts_infection, nr_prescriptions_AB, delta_1))

prim.table_1<-data.frame(matrix(ncol=(6)))
colnames(prim.table_1)<-c("alpha","beta","number_cov","rule_cov","number_div","rule_div")

set.seed(1234)

for (i in 1:10){
  alpha<-i/100
  for (j in 1:10){
    beta<-j/100
    p.cov<-subgroup.discovery::prim.cover(delta_1~., data = PRIM_1,
                                          peeling.quantile=alpha,
                                          min.support=beta, 
                                          plot= FALSE, 
                                          optimal.box= "2se")
    rule_cov<-p.cov$covers[[1]]$superrule
    p.div <- prim.diversify(delta_1 ~ .,
                            data = PRIM_1,
                            n = 4,
                            peeling.quantile = alpha,
                            min.support = beta,
                            plot = FALSE,
                            optimal.box = "2se")
    rule_div<-p.div$attempts[[1]]$superrule
    
    prim.table_1[10*i+j-10,]<-c(alpha,beta,length(rule_cov),toString(rule_cov),length(rule_div),toString(rule_div))
    
  }
}
write.csv(prim.table_1, file='prim.table_1.csv') 

# PRIM with delta coming from  AB TYPE 0 VS 2
PRIM_2 <- subset(data_first_treatment_12ow_relevant_0.4, select = c(age, sex, nr_medication,COPD_morb, migraine_morb, nr_contacts_resp, osteop_morb,poor_immune_response, alcmisb_morb,practice_size, Depressie_morb,aangaf_morb,hartfalen_morb,flu_epi,season,postalcode, nr_contacts_infection, nr_prescriptions_AB,delta_2))

prim.table_2<-data.frame(matrix(ncol=(6)))
colnames(prim.table_2)<-c("alpha","beta","number_cov","rule_cov","number_div","rule_div")

set.seed(1234)

for (i in 1:10){
  alpha<-i/100
  for (j in 1:10){
    beta<-j/100
    p.cov<-subgroup.discovery::prim.cover(delta_2~., data = PRIM_2,
                                          peeling.quantile=alpha,
                                          min.support=beta, 
                                          plot= FALSE, 
                                          optimal.box= "2se")
    rule_cov<-p.cov$covers[[1]]$superrule
    p.div <- prim.diversify(delta_2 ~ .,
                            data = PRIM_2,
                            n = 4,
                            peeling.quantile = alpha,
                            min.support = beta,
                            plot = FALSE,
                            optimal.box = "2se")
    rule_div<-p.div$attempts[[1]]$superrule
    
    prim.table_2[10*i+j-10,]<-c(alpha,beta,length(rule_cov),toString(rule_cov),length(rule_div),toString(rule_div))
    
  }
}
write.csv(prim.table_2, file='prim.table_2.csv') 

# PRIM with delta coming from  AB TYPE 0 VS 3
PRIM_3 <- subset(data_first_treatment_12ow_relevant_0.4, select = c(age, sex, nr_medication,COPD_morb, migraine_morb, nr_contacts_resp, osteop_morb,poor_immune_response, alcmisb_morb,practice_size, Depressie_morb,aangaf_morb,hartfalen_morb,flu_epi,season,postalcode, nr_contacts_infection, nr_prescriptions_AB,delta_3))

prim.table_3<-data.frame(matrix(ncol=(6)))
colnames(prim.table_3)<-c("alpha","beta","number_cov","rule_cov","number_div","rule_div")

set.seed(1234)

for (i in 1:10){
  alpha<-i/100
  for (j in 1:10){
    beta<-j/100
    p.cov<-subgroup.discovery::prim.cover(delta_3~., data = PRIM_3,
                                          peeling.quantile=alpha,
                                          min.support=beta, 
                                          plot= FALSE, 
                                          optimal.box= "2se")
    rule_cov<-p.cov$covers[[1]]$superrule
    p.div <- prim.diversify(delta_3 ~ .,
                            data = PRIM_3,
                            n = 4,
                            peeling.quantile = alpha,
                            min.support = beta,
                            plot = FALSE,
                            optimal.box = "2se")
    rule_div<-p.div$attempts[[1]]$superrule
    
    prim.table_3[10*i+j-10,]<-c(alpha,beta,length(rule_cov),toString(rule_cov),length(rule_div),toString(rule_div))
    
  }
}
write.csv(prim.table_3, file='prim.table_3.csv') 

# PRIM with delta coming from  AB TYPE 0 VS 4
PRIM_4 <- subset(data_first_treatment_12ow_relevant_0.4, select = c(age, sex, nr_medication,COPD_morb, migraine_morb, nr_contacts_resp, osteop_morb,poor_immune_response, alcmisb_morb,practice_size, Depressie_morb,aangaf_morb,hartfalen_morb,flu_epi,season,postalcode, nr_contacts_infection, nr_prescriptions_AB, delta_4))

prim.table_4<-data.frame(matrix(ncol=(6)))
colnames(prim.table_4)<-c("alpha","beta","number_cov","rule_cov","number_div","rule_div")

set.seed(1234)

for (i in 1:10){
  alpha<-i/100
  for (j in 1:10){
    beta<-j/100
    p.cov<-subgroup.discovery::prim.cover(delta_4~., data = PRIM_4,
                                          peeling.quantile=alpha,
                                          min.support=beta, 
                                          plot= FALSE, 
                                          optimal.box= "2se")
    rule_cov<-p.cov$covers[[1]]$superrule
    p.div <- prim.diversify(delta_4 ~ .,
                            data = PRIM_4,
                            n = 4,
                            peeling.quantile = alpha,
                            min.support = beta,
                            plot = FALSE,
                            optimal.box = "2se")
    rule_div<-p.div$attempts[[1]]$superrule
    
    prim.table_4[10*i+j-10,]<-c(alpha,beta,length(rule_cov),toString(rule_cov),length(rule_div),toString(rule_div))
    
  }
}

write.csv(prim.table_4, file='prim.table_4.csv') 


memory.limit() 
memory.limit(size=30000)


#interst box
risk_data1<-data.frame(matrix(ncol=20))
colnames(risk_data1)<-colnames(PRIM)

rule1<-p.cov$covers[[1]]$superrule

myString1 <- paste("subset(PRIM,", 
                   paste(gsub(',','&', toString(rule1)), 
                         "& ID)"
                   ))     # this will glue three strings together
# a is the subset of PRIM_np that follow the rule

a1<-eval(parse(text=myString1))  # now we tell R to look at the string as a command and execute it
#risk_data1<-rbind(a,risk_data)

PRIM$interst<- ifelse(PRIM$ID %in% a1$ID,1,0) 

interest<-mutate(PRIM,     
                 ifelse(PRIM$ID %in% a1$ID,1,0))




#Prevalence of the original population
m_average<-mean(PRIM$delta,na.rm=TRUE)

####put different rules into subset automatically & put the string of rule into a vector
#prim.table.r<-read.csv(file = "F:/博士/pumc/课题???/AMC/Utrecht/NIVEL/Thamar/20190102/prim.table.csv")
#prim.table.r_np<-read.csv(file = "H:/qww/AMC/Utrecht/NIVEL/Thamar/20190102/prim.table.csv")

#Prevalence and coverage !!!!!
for (i in 1:93){
  
  myString <- paste("subset(PRIM,", 
                    paste(gsub(',','&',prim.table$rule_cov[i]), 
                          ")"
                    ))# this will glue three strings together
  
  a<-eval(parse(text=myString))  # now we tell R to look at the string as a command and execute it
  
  PRIM$box<- ifelse(PRIM$ID %in% a$ID,1,0) 
  
  PRIM[,ncol(PRIM)+1]<-PRIM$box
  names(PRIM)[ncol(PRIM)]<-paste0("box_",i)
  
  m_delta<-sum(subset(PRIM, box==1)$delta, na.rm=TRUE)/nrow(a)
  
  prim.table$m_delta[i]<-m_delta
  
  betat <-nrow(a)/nrow(PRIM)
  prim.table$betat[i]<- betat
  
  prim.table$coverage1[i]<-(m_delta-m_average)*betat
  prim.table$coverage2[i]<-(m_average-m_delta)*betat
  
  write.csv( prim.table, file='prim.table_coverage.csv')
  
}



#prim.table.r_np<-read.csv(file = "H:/qww/AMC/Utrecht/NIVEL/Thamar/20190102/prim.table_np.csv")

#Prevalence and coverage !!!!!
for (i in 41:100){
  
  myString <- paste("subset(PRIM_np,", 
                    paste(gsub(',','&',prim.table_np$rule_cov[i]), 
                          ")"
                    ))# this will glue three strings together
  
  a<-eval(parse(text=myString))  # now we tell R to look at the string as a command and execute it
  
  PRIM_np$box<- ifelse(PRIM_np$ID %in% a$ID,1,0) 
  
  PRIM_np[,ncol(PRIM_np)+1]<-PRIM_np$box
  names(PRIM_np)[ncol(PRIM_np)]<-paste0("box_",i)
  
  m_delta<-sum(subset(PRIM_np, box==1)$delta,na.rm=TRUE)/nrow(a)
  
  prim.table_np$m_delta[i]<-m_delta
  
  betat <-nrow(a)/nrow(PRIM_np)
  prim.table_np$betat[i]<- betat
  
  prim.table_np$coverage1[i]<-(m_delta-m_average)*betat
  prim.table_np$coverage2[i]<-(m_average-m_delta)*betat
  
  write.csv( prim.table_np, file='prim.table_np_coverage.csv')
  
}

setwd("F:/博士/pumc/课题???/AMC/Utrecht/NIVEL/Thamar/20190102")
prim.table.r_np<-read.csv(file = "F:/博士/pumc/课题???/AMC/Utrecht/NIVEL/Thamar/20190102/prim.table_np.csv")

for (i in 47){
  
  myString <- paste("subset(PRIM_np,", 
                    paste(gsub(',','&',prim.table.r_np$rule_cov[i]), 
                          ")"
                    ))#
}

fit <- glm(
  as.formula(paste("delta ~ ", paste(names( as.formula(myString)), collapse="+"),sep="")),
  data  = PRIM_np, family = gaussian)




fit <- glm(
  as.formula(paste("delta ~ ", paste(names(select(PRIM,-delta)), collapse="+"),sep="")),
  data  = PRIM, family = gaussian)

fit_delta<- summary(fit)
fit_delta

