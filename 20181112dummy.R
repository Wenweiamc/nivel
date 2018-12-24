
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
# install.packages('Matching')
# install.packages('reshape2')
# install.packages('stddiff')
# install.packages('randomForestSRC')
# install.packages('subgroup.discovery')
#install.packages("installr")
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
library('plyr')
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
library(mice)
library(lubridate)
library(miceadds)
library(testthat)
#updateR()
###########################
# %% setwd
###########################
setwd("C:/Users/mcschut/Documents/wip/chianti/data/processed/180523/")
setwd("F:/博士/pumc/课题�?/AMC/Utrecht/NIVEL/Thamar/20180823/")
setwd("H:/qww/AMC/Utrecht/NIVEL/Thamar/20180823/")

###########################
# %% read data and convert variables
###########################

# read in data as factor per default
data_first_treatment <-read.csv(file = "dummyfile_firsttreatment.csv", na.strings=c("NA","NaN", " ",""), colClasses="factor")
data_first_treatment<-read.csv(file = "F:/博士/pumc/课题�?/AMC/Utrecht/NIVEL/Thamar/20180523/dummyfile_firsttreatment.csv", na.strings=c("NA","NaN", " ",""), colClasses="factor")
data_first_treatment<-read.csv(file = "H:/qww/AMC/Utrecht/NIVEL/Thamar/20180523//dummyfile_firsttreatment.csv", na.strings=c("NA","NaN", " ",""), colClasses="factor")
data_first_treatment<-read_csv(file = "H:/qww/AMC/Utrecht/NIVEL/Thamar/20180523/dummyfile_onlynose.csv")
data_first_treatment<-read.csv(file = "F:/博士/pumc/课题�?/AMC/Utrecht/NIVEL/Thamar/20180523/dummyfile_onlynose.csv")

# convert selected variables to numeric
cols.num <- c("age","nr_chron3", 'practice_size', 'nr_medication','nr_contacts_infection','nr_prescriptions_AB' ,'nr_contacts_resp', "days_prev_cont","CRP_values")
data_first_treatment[cols.num] <- sapply(data_first_treatment[cols.num],as.numeric)
data_first_treatment$date <- dmy(data_first_treatment$date)
str(data_first_treatment$date)
### Provide descriptives of all variables
z<-dfSummary(data_first_treatment)
write.table(z, file="descriptives_raw_dummy.csv", sep = ",")
hist(data_first_treatment$ days_prev_cont)
nrow(subset(data_first_treatment,outcome_2==1 & outcome_4==0))/nrow(subset(data_first_treatment,outcome_4==0))

###date
#data_first_treatment$endtime <- dmy(rep("31dec2014",999))

mean(month(data_first_treatment$date)==12)
mean(month(data_first_treatment$date)<4)

data_first_treatment_12ow <- mutate(data_first_treatment, 
                                    outcome_weight=ifelse(month(date)==12 & is.na(outcome_4),(31-day(data_first_treatment$date))/28, 1))

data_first_treatment_12ow <- mutate(data_first_treatment, 
                                    ID=seq.int(nrow(data_first_treatment_12ow)), 
                                    week=week(date),
                                    flu_epi=ifelse(week(date) %in% c(5:8, 10,11,49:53),1,0),
                                    outcome_weight=ifelse(month(date)==12 & is.na(outcome_4),(31-day(data_first_treatment$date))/28, 1))

data_first_treatment_12ow$season[month(data_first_treatment_12ow$date) %in% c(3:5)] <- "spring"
data_first_treatment_12ow$season[month(data_first_treatment_12ow$date) %in% c(6:8)] <- "summer"
data_first_treatment_12ow$season[month(data_first_treatment_12ow$date) %in% c(9:11)] <- "autumn"
data_first_treatment_12ow$season[month(data_first_treatment_12ow$date) %in% c(1,2,12)] <- "winter"
data_first_treatment_12ow$season <- factor(data_first_treatment_12ow$season, ordered = TRUE, levels = c("spring", "summer", "autumn", "winter"))
str(data_first_treatment_12ow$week)

###########################
# %% select variables
###########################
vars_selected <-read.csv(file = "variable_selections.csv",sep=',')

vars_treatment   <- as.character(filter(vars_selected,treatment == 1)$variable)
vars_outcome     <- as.character(filter(vars_selected,outcome == 1)$variable)
vars_other       <- as.character(filter(vars_selected,other == 1)$variable)
vars_relevant    <- as.character(filter(vars_selected,relevant == 1)$variable)
vars_relevant_nto    <- as.character(filter(vars_selected,relevant_nto == 1)$variable)

###########################
# %% descriptive statistics and remove variables with variance = 0 @@@ still gives warning messages about deprecated
###########################

# which variables have variance = 0?
vars_zero = lapply(data_first_treatment_12ow, var, na.rm = TRUE) != 0
vars_zero_names = names(vars_zero[vars_zero == FALSE])

# drop variables with variance = 0
data_first_treatment_12ow_v0<- dplyr::select(data_first_treatment_12ow,-one_of(vars_zero_names))

# remove dropped variables from variable selection
vars_relevant_nto_v0 <- intersect(vars_relevant_nto,names(data_first_treatment_12ow_v0)) 


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
# remove dropped variables from variable selection
vars_relevant_0.4 <- intersect(vars_relevant,names(data_first_treatment_12ow_relevant_0.4))

###########################
# %% descriptive statistics and remove variables with variance = 0 @@@ still gives warning messages about deprecated
###########################

# which variables have variance = 0?
vars_zero = lapply(data_first_treatment_12ow_relevant_0.4, var, na.rm = TRUE) != 0
vars_zero_names = names(vars_zero[vars_zero == FALSE])
#all(duplicated(x)[-1L])

# drop variables with variance = 0
data_first_treatment_12ow_relevant_0.4_v0<- dplyr::select(data_first_treatment_12ow_relevant_0.4,-one_of(vars_zero_names))

# remove dropped variables from variable selection
vars_relevant_0.4_v0 <- intersect(vars_relevant,names(data_first_treatment_12ow_relevant_0.4_v0)) 


###########################
# %% remove rows with any missing values
###########################

# FYI how many rows are deleted
dim(data_first_treatment_12ow_relevant_0.4_v0)[1] - dim(na.omit(data_first_treatment_12ow_relevant_0.4_v0))[1]

data_first_treatment_12ow_relevant_0.4_v0_na.omit <- na.omit(data_first_treatment_12ow_relevant_0.4_v0)

vars_relevant_0.4_v0_na.omit <- names(data_first_treatment_12ow_relevant_0.4_v0_na.omit)

###########################
# %% descriptive statistics and remove variables with variance = 0 @@@ still gives warning messages about deprecated
###########################

# which variables have variance = 0?
vars_zero_na.omit = lapply(data_first_treatment_12ow_relevant_0.4_v0_na.omit, var, na.rm = TRUE) != 0
vars_zero_na.omit_names = names(vars_zero_na.omit[vars_zero_na.omit == FALSE])
#all(duplicated(x)[-1L])

# drop variables with variance = 0
data_first_treatment_12ow_relevant_0.4_na.omit_v0<- dplyr::select(data_first_treatment_12ow_relevant_0.4_v0_na.omit,-one_of(vars_zero_na.omit_names))

# remove dropped variables from variable selection
vars_relevant_0.4_na.omit_v0 <- intersect(vars_relevant,names(data_first_treatment_12ow_relevant_0.4_v0_na.omit)) 



###########################
# %% print table on
###########################

a<-print(CreateTableOne(vars=names(data_first_treatment_12ow_relevant_0.4),
                     strata="AB_nose_infection",
                     data=data_first_treatment_12ow_relevant_0.4[,names(data_first_treatment_12ow_relevant_0.4) %in% vars_relevant_0.4, drop = F],
                     test=TRUE))

print(CreateTableOne(vars=names(data_first_treatment_12ow$CRP_values),
                        strata="AB_nose_infection",
                        data=data_first_treatment_12ow,
                        test=TRUE))


b<-print(CreateTableOne(vars=names(data_first_treatment_12ow_relevant_0.4),
                        strata="type_AB_nose",
                        data=data_first_treatment_12ow_relevant_0.4[,names(data_first_treatment_12ow_relevant_0.4) %in% vars_relevant_0.4, drop = F],
                        test=TRUE))

write.table(a, file="F:/??ʿ/pumc/??????/AMC/Utrecht/NIVEL/Thamar/20180823/Unmatched_ab.csv", sep = ",")
write.table(b, file="F:/??ʿ/pumc/??????/AMC/Utrecht/NIVEL/Thamar/20180823/Unmatched_ab.csv", sep = ",")

###########################
# %% STEP 1: select variables with OR > 1%
###########################

# calculate OR1
model_OR <-glm(
  outcome_4 ~ AB_nose_infection,
  family = binomial,
  data = data_first_treatment_12ow_relevant_0.4_v0 )
OR1<- exp(model_OR$coefficients[2])

# FYI model summary
summary(model_OR)

# calculate rates for relevant variables

vars_relevant_0.4_v0_nto <- vars_relevant_0.4_v0[!vars_relevant_0.4_v0 %in% c("outcome_4","AB_nose_infection","type_AB_nose")]

rate <- sapply(vars_relevant_0.4_v0_nto,
       function(varname){
         md <-glm(
                as.formula(paste("outcome_4 ~ AB_nose_infection + ",varname)),
                family=binomial(link=logit),
                data=data_first_treatment_12ow_relevant_0.4_v0
              )
         rate <- abs(exp(summary(md)$coef[2])-OR1)/OR1
       }
     )

# drop the variables that have OR =< 1%
vars_OR <- labels(rate)[unlist(rate)> 0.01]

# drop ".AB_nose_infection1" from labels
vars_OR <- gsub(".AB_nose_infection1", "", vars_OR)

###########################
# %% STEP 2: AIC to select co-variables: outcome
###########################

model_AICo <- glm(
  outcome_4 ~ .,
  weights = outcome_weight,
  data = dplyr::select(data_first_treatment_12ow_relevant_0.4_na.omit_v0, -vars_treatment),
  family=binomial)

# Select most predictive variables
AICo <- stepAIC(model_AICo)
#Error in stepAIC(model_AICo) : 
#  number of rows in use has changed: remove missing values?


# FYI show formula of the final model
formula(AICo)

# show the selected variables
vars_AICo <- attr(terms(AICo),"term.labels")

###########################
# %% STEP 3: AIC to select co-variables: treatment
###########################

# @@@ should we remove type_AB_nose completely?
model_AICt <- glm(
  AB_nose_infection ~ .,
  data = select(data_first_treatment_12ow_relevant_0.4_na.omit_v0[!names(data_first_treatment_12ow_relevant_0.4_na.omit_v0) %in% vars_outcome],-c('type_AB_nose')),
  family=binomial)

# Select most predictive variables
AICt <- stepAIC(model_AICt)

# FYI show formula of the final model
formula(AICt)

# Show the selected variables
vars_AICt <- attr(terms(AICt),"term.labels")

###########################
# %% Step 4 univariate relation outcome ~ selected treatment variables
###########################

models <- lapply(vars_AICt,
  function(x) {
    glm(substitute(outcome_4 ~ i, list(i = as.name(x))),
        family = binomial,
        data = select(data_first_treatment_12ow_relevant_0.4_na.omit_v0,-vars_treatment))
  }
)

lapply(models, summary)

###########################
# %% Determine confounder variables @@@ todo ask ameen how we do this
###########################
vars_confounders = c(
  "age",
  "sex",
  "nr_medication",
  "COPD_morb",
  "migraine_morb",
  "hartfalen_morb",
  "nr_contacts_resp",
  "osteop_morb",
  "poor_immune_response",
  "alcmisb_morb",
  "postalcode",
  "nr_contacts_infection",
  "nr_prescriptions_AB"
)

data_confounders_to <- data_first_treatment_12ow_relevant[c(
  "outcome_4","AB_nose_infection","type_AB_nose", "outcome_weight", "ID", 
  "age",
  "sex",
  "nr_medication",
  "COPD_morb",
  "migraine_morb",
  "hartfalen_morb",
  "nr_contacts_resp",
  "osteop_morb",
  "poor_immune_response",
  "alcmisb_morb",
  "postalcode",
  "nr_contacts_infection",
  "nr_prescriptions_AB"
)]        


data_confounders <- data_first_treatment_12ow_relevant[c(
  "ID",
  "age",
  "sex",
  "nr_medication",
  "COPD_morb",
  "migraine_morb",
  "hartfalen_morb",
  "nr_contacts_resp",
  "osteop_morb",
  "poor_immune_response",
  "alcmisb_morb",
  "postalcode",
  "nr_contacts_infection",
  "nr_prescriptions_AB"
)] 

s<-dfSummary(data_confounders_to)
write.table(s, file="descriptives_dummycto.csv", sep = ",")


#Multivariate Imputation by Chained Equations
data_confounders_mi<-mice(data_confounders, m=5,seed = 1234)

head(data_confounders_mi$imp$nr_contacts_resp,5)
summary(data_confounders_mi)
densityplot(data_confounders_mi)
data_confounders_mi$loggedEvents
tail(data_confounders_mi$loggedEvents,3)

pred <- make.predictorMatrix(data_confounders)
pred[c("nr_contacts_resp", "postalcode"), c("nr_contacts_resp", "postalcode")] <- 0
pred[c("nr_prescriptions_AB", "postalcode"), c("nr_prescriptions_AB", "postalcode")] <- 0
pred[c("nr_contacts_infection", "postalcode"), c("nr_contacts_infection", "postalcode")] <- 0

pred
mi<-mice(data_confounders,pred=pred, m=5,seed = 1234)
class(mi)

data_confounders_mi<-data_confounders_mi$data
class(data_confounders_mi)

#write.table(mi, file="descriptives_c_mi.csv", sep = ",")

data_confounders_mi_to <-merge(data_confounders_mi,data_confounders_to[,c("outcome_4","AB_nose_infection","type_AB_nose", "outcome_weight", "ID")], by="ID")


summary(glm(outcome_4 ~ nr_contacts_resp, data= data_confounders_mi_to, family = binomial))
summary(glm(outcome_4 ~ nr_prescriptions_AB, data= data_confounders_mi_to, family = binomial))
summary(glm(outcome_4 ~ nr_contacts_infection, data= data_confounders_mi_to, family = binomial))
summary(glm(AB_nose_infection ~ nr_contacts_resp, data= data_confounders_mi_to, family = binomial))
summary(glm(AB_nose_infection ~ nr_prescriptions_AB, data= data_confounders_mi_to, family = binomial))
summary(glm(AB_nose_infection ~ nr_contacts_infection, data= data_confounders_mi_to, family = binomial))


model_fit<-with(mi, glm(outcome_4~AB_nose_infection,family = binomial))
model_pooled<-pool(model_fit)
summary(model_pooled)

model_fit_multi<-with(mi, glm(
  as.formula(paste("outcome_4 ~ AB_nose_infection + ", paste(vars_confounders, collapse="+"),sep="")),
  family = binomial))
model_pooled_multi<-pool(model_fit_multi)
summary(model_pooled_multi)





data_confounders_mi<-mice(data_confounders, m=5,seed = 1234)
#Warning message:
 # Number of logged events: 25
#http://stefvanbuuren.name/fimd/sec-knowledge.html
head(data_confounders_mi$imp$nr_contacts_resp,5)
summary(data_confounders_mi)
densityplot(data_confounders_mi)
data_confounders_mi$loggedEvents
tail(data_confounders_mi$loggedEvents,3)

pred <- make.predictorMatrix(data_confounders)
pred[c("nr_contacts_resp", "postalcode"), c("nr_contacts_resp", "postalcode")] <- 0
pred[c("nr_prescriptions_AB", "postalcode"), c("nr_prescriptions_AB", "postalcode")] <- 0
pred[c("nr_contacts_infection", "postalcode"), c("nr_contacts_infection", "postalcode")] <- 0

pred
data_confounders_mi<-mice(data_confounders,pred=pred, m=5,seed = 1234)

mi<-dfSummary(data_confounders_mi)
write.table(mi, file="descriptives_dummyc_mi.csv", sep = ",")



model_fit<-with(data_confounders_to_mi, glm(outcome_4~AB_nose_infection,family = binomial))
model_pooled<-pool(model_fit)
summary(model_pooled)

model_fit_multi<-with(data_confounders_to_mi, glm(
  as.formula(paste("outcome_4 ~ AB_nose_infection + ", paste(vars_confounders, collapse="+"),sep="")),
  family = binomial))
model_pooled_multi<-pool(model_fit_multi)
summary(model_pooled_multi)


###########################
# %% method1: IPTW-Compute wights
###########################
#propensity score model
model_PS <-glm(
  as.formula(paste("AB_nose_infection ~ ",paste(vars_confounders,collapse="+"),sep="")),
  family = "binomial",
  data = data_confounders_to)

##value of propensity score for each subject
ps<-predict(model_PS, type = "response")

#create weights
weight<-ifelse(data_confounders_to$AB_nose_infection==1,1/(ps),1/(1-ps))

#apply weights to data
weighteddata<-svydesign(ids = ~1, data = data_confounders_to, weights = ~weight)

#weighted table 1
weightedtable_cof<-svyCreateTableOne(vars = vars_confounders , strata = "AB_nose_infection", data=weighteddata, test = TRUE)
weightedtable_all<-svyCreateTableOne(vars = , strata = "AB_nose_infection", data=weighteddata, test = TRUE)
## show table with SMD
print(weightedtable_cof, smd=TRUE)
print(weightedtable_all, smd=TRUE)

#to get a weighted mean for a single covariate(eg.age) directly

mean(weight[data_confounders_to$AB_nose_infection==1]*data_confounders_to$age[data_confounders_to$AB_nose_infection==1])/(mean(weight[data_confounders_to$AB_nose_infection==1]))

###########################
# %% Compute IPTW
###########################
library(tableone)
library(sandwich)
library(survey)

#get causal relative risk(risk difference). weighted GLM
glm.obj<-glm(outcome_4~AB_nose_infection, data = data_confounders_to, weights = weight,family = quasibinomial)

#summary(glm.obj)
betaiptw<-coef(glm.obj)

#to properly account for weighting, use asymptotic(sandwich) variance
SE<-sqrt(diag(vcovBS(glm.obj)))

causalrr<-exp(betaiptw[2])
lcl<-exp(betaiptw[2]-1.96*SE[2])
url<-exp(betaiptw[2]+1.96*SE[2])
c(lcl,causalrr,url)


SE_HC<-sqrt(diag(vcovHC(glm.obj,type="HC0")))
causalrd<-exp(betaiptw[2])
lcl_HC<-exp(betaiptw[2]-1.96*SE[2])
url_HC<-exp(betaiptw[2]+1.96*SE[2])
c(lcl_HC,causalrd,url_HC)

library(ipw)
#let's fit the same models using the R package IPW

eval(parse(text=paste("weightmodel<-ipwpoint(exposure=AB_nose_infection, data = data_confounders_to, family = \"binomial\", link = \"logit\",
                   denominator=~ ",paste(vars_confounders,collapse="+"),")",sep="")))
weightmodel

summary(weightmodel$ipw.weights)
ipwplot(weights = weightmodel$ipw.weights, logscale = FALSE, main = "weights", xlim = c(0,9))
#fit a marginal structural model(risk difference)
msm<- svyglm(as.numeric(outcome_4) ~ AB_nose_infection, design = svydesign(~1,
                                                               weights = ~ weight, 
                                                               data = data_confounders_to, 
                                                               family = binomial))
#Error in `$<-.data.frame`(`*tmp*`, ".survey.prob.weights", value = c(0.000316083817916795,  : 
#                                                                       replacement has 5735 rows, data has 586
coef(msm)
confint(msm)
#truncated weights
truncweight<-replace(weight, weight>10,10)
#get causal risk difference
glm.obj<-glm(outcome_4 ~ AB_nose_infection, weights = truncweight,family = quasibinomial(link ="identity"))
summary(glm.obj)


###########################
# %% method2: Compute propensity scores
###########################
model_PS <-glm(
  as.formula(paste("AB_nose_infection ~ ",paste(vars_confounders,collapse="+"),sep="")),
  family = "binomial",
  data = data_first_treatment_12ow_relevant_0.4_na.omit_v0)

# add propensity score to data set with all covariates, but without missing
data_first_treatment_12ow_relevant_0.4_na.omit_v0$pscore <- model_PS$fitted.values

# plot propensity score
labs <- paste("Did patient actually received treatment?:", c("No", "Yes"))
data_first_treatment_12ow_relevant_0.4_na.omit_v0 %>%
  mutate(AB_nose_infection = ifelse(AB_nose_infection == 0, labs[1], labs[2])) %>%
  ggplot(aes(x = pscore)) +
  geom_histogram(color = "white",bins = 50) +
  facet_wrap(~AB_nose_infection) +
  xlab("Probability of getting AB") +
  theme_bw()

#`stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

###########################
# %% Propensity score matching
###########################

model_Match <- matchit(
  as.formula(paste("AB_nose_infection ~ ",paste(vars_confounders,collapse="+"),sep="")),
  data=data_first_treatment_12ow_relevant_0.4_na.omit_v0,replace= TRUE,
  method = "nearest")
#Warning message:
 # In matchit2nearest(c(`2` = 1L, `4` = 0L, `6` = 1L, `9` = 0L, `11` = 0L,  :
  #                       Fewer control than treated units and matching without replacement.  Not all treated units will receive a match.  Treated units will be matched in the order specified by m.order: largest

# get data from matchit model
data_Match<- match.data(model_Match)

# FYI model summary
summary(model_Match)

# plot matching
plot(model_Match, type = "jitter")
plot(model_Match, type = "hist")

###########################
# %% method3: Propensity score matching with ps
###########################

data_first_treatment_12ow_relevant$AB<-0
data_first_treatment_12ow_relevant$AB[as.numeric(data_first_treatment_12ow_relevant$AB_nose_infection)==2]<-1
#data_confounders_to$AB[as.numeric(data_confounders_to$AB_nose_infection)==1]<-0

set.seed(1)
PS_Model<-ps(
  as.formula(paste("AB ~ ",paste(vars_relevant_nto_v0,collapse="+"),sep="")),
  data = data_first_treatment_12ow_relevant,
  verbose = FALSE, 
  estimand = "ATE")



data_confounders_to$AB<-0
data_confounders_to$AB[as.numeric(data_confounders_to$AB_nose_infection)==2]<-1
#data_confounders_to$AB[as.numeric(data_confounders_to$AB_nose_infection)==1]<-0

set.seed(1)
PS_Model<-ps(
  as.formula(paste("AB ~ ",paste(vars_confounders,collapse="+"),sep="")),
  #  AB~age+sex+nr_medication+COPD_morb+migraine_morb+hartfalen_morb+nr_contacts_resp+osteop_morb+poor_immune_response+alcmisb_morb+postalcode,
  data = data_confounders_to,
  # n.trees = 10000,
  # interaction.depth = 3,
  # shrinkage = 0.01,
  #  bag.fraction = 1.0,
  # perm.test.iters=0,
  # print.level = 2,
  # iterlim = 1000,
  verbose = FALSE, 
  estimand = "ATE")
# stop.method = c("ks.mean", "es.mean"), 
# sampw = NULL, 
# multinom = FALSE)

plot(PS_Model, plots = 1)
plot(PS_Model, plots = 2)
plot(PS_Model, plots = 3)
plot(PS_Model, plots = 4)
plot(PS_Model, plots = 5)

summary(PS_Model$gbm.obj,
        n.trees=PS_Model$desc$ks.mean.ATE$n.trees,
        plot=TRUE)
summary(PS_Model)
ps.balance<-bal.table(PS_Model)
ps.balance

data_confounders_to$W<- get.weights(PS_Model,stop.method = "es.mean" )
#Error in eval(predvars, data, env) : object 'w' not found

design.ps <- svydesign(ids=~1, weights = ~ W*outcome_weight, data = data_confounders_to)
#svychisq(~sixMonthSurvive + abcix, design = design.ps)

###########################
# %% calculate SMD - before matching
###########################

# determine datatypes per variable @@@ check if this works correctly
nums <- sapply(data_first_treatment_12ow_relevant_0.4, is.numeric)
cats <- sapply(data_first_treatment_12ow_relevant_0.4_na.omit_v0, is.factor)

# add treatment variable to data
nums['AB_nose_infection'] = TRUE
cats['AB_nose_infection'] = TRUE

# compute stddiffs
c<-stddiff.numeric(data_first_treatment_12ow_relevant_0.4[,nums],gcol=1,vcol=c(2:dim(data_first_treatment_12ow_relevant_0.4[,nums])[2]))
d<-stddiff.binary(data_first_treatment_12ow_relevant_0.4_na.omit_v0[,cats],gcol=1,vcol=c(2:dim(data_first_treatment_12ow_relevant_0.4_na.omit_v0[,cats])[2]))

#Warning messages:
#  1: In sqrt((p1 * (1 - p1) + p2 * (1 - p2))/2) : NaNs produced
#2: In sqrt((p1 * (1 - p1) + p2 * (1 - p2))/2) : NaNs produced

write.table(c, file="smdUnmatched_nums.csv", sep = ",")
write.table(d, file="smdUnmatched_cats.csv", sep = ",")


###########################
# %% calculate SMD - after matching
###########################

# determine datatypes per variable @@@ check if this works correctly
nums_m <- sapply(data_Match, is.numeric)
cats_m <- sapply(data_Match, is.factor)

# add treatment variable to data
nums_m ['AB_nose_infection'] = TRUE
cats_m ['AB_nose_infection'] = TRUE

# compute stddiffs
e <- stddiff.numeric(data=data_Match[,nums_m],gcol=1,vcol=c(2:dim(data_Match[,nums_m])[2]))
f <- stddiff.binary(data=data_Match[,cats_m],gcol=1,vcol=c(2:dim(data_Match[,cats_m])[2]))

#Warning messages:
#  1: In sqrt((p1 * (1 - p1) + p2 * (1 - p2))/2) : NaNs produced
#2: In sqrt((p1 * (1 - p1) + p2 * (1 - p2))/2) : NaNs produced

write.table(e, file="smdmatched_nums.csv", sep = ",")
write.table(f, file="smdmatched_cats.csv", sep = ",")

###########################
# %% calculate SMD - differences before/after matching
###########################

# @@@ todo

###########################
# %% GLM before matching
###########################
#### logistic univariate
uni<-glm(outcome_4~AB_nose_infection, data= data_first_treatment_12ow_relevant_0.4, family = binomial)

summary(uni)

cbind( exp(coef(uni)), exp(summary(uni)$coefficients[,1] - 1.96*summary(uni)$coefficients[,2]), exp(summary(uni)$coefficients[,1] + 1.96*summary(uni)$coefficients[,2]) )

#### logistic multivariate

unicov <-glm(
            as.formula(paste("outcome_4 ~ AB_nose_infection + ", paste(vars_confounders, collapse="+"),sep="")),
            family = "binomial",
            data = data_first_treatment_12ow_relevant_0.4
           )

#Error in eval(predvars, data, env) : object 'CRP_values' not found
# we might delete 40% NA first
summary(unicov)
cbind( exp(coef(unicov)), exp(summary(unicov)$coefficients[,1] - 1.96*summary(unicov)$coefficients[,2]), exp(summary(unicov)$coefficients[,1] + 1.96*summary(unicov)$coefficients[,2]) )

###########################
# %% GLM after matching
###########################
uni_match <- glm(outcome_4 ~ AB_nose_infection, family = "binomial", data = data_Match)
summary(uni_match)

cbind( exp(coef(uni_match)), exp(summary(uni_match)$coefficients[,1] - 1.96*summary(uni_match)$coefficients[,2]), exp(summary(uni_match)$coefficients[,1] + 1.96*summary(uni_match)$coefficients[,2]) )
###########################
# %% running of multinomial propensity scores
###########################

model_MNPS <- mnps(
  as.formula(paste("type_AB_nose ~ ",paste(vars_relevant_nto_v0,collapse="+"),sep="")),
  data = data_first_treatment_12ow,
  estimand = "ATE",
  verbose = FALSE,
  stop.method = c("es.mean", "ks.mean"),
  n.trees = 5000)

model_MNPS_cto <- mnps(
  as.formula(paste("type_AB_nose ~ ",paste(vars_confounders,collapse="+"),sep="")),
  data = data_confounders_to,
  estimand = "ATE",
  verbose = FALSE,
  stop.method = c("es.mean", "ks.mean"),
  n.trees = 3000)

summary(model_MNPS)
plot(model_MNPS, plots = 1) 
plot(model_MNPS, plots = 2, subset = "es.mean")
data_first_treatment_12ow_relevant_0.4_na.omit_v0$w <- get.weights(data_first_treatment_12ow_relevant_0.4_na.omit_v0, stop.method = "es.mean")

design.mnps <- svydesign(ids=~1, weights=~w, data=data_first_treatment_12ow_relevant_0.4_na.omit_v0)#
glm1 <- svyglm(outcome_4 ~ as.factor(type_AB_nose), design = design.mnps, family=binomial)


summary(model_MNPS_cto)
plot(model_MNPS_cto, plots = 1) 
plot(model_MNPS_cto, plots = 2, subset = "es.mean")
data_confounders_to$w <- get.weights(model_MNPS_cto, stop.method = "es.mean")

design.mnps <- svydesign(ids=~1, weights=~w, data=data_confounders_to)

## survey
glm_cto <- svyglm(outcome_4 ~ type_AB_nose, design = design.mnps,family=quasibinomial)
summary(glm_cto)

cbind( exp(coef(glm_cto)), exp(summary(glm_cto)$coefficients[,1] - 1.96*summary(glm_cto)$coefficients[,2]), exp(summary(glm_cto)$coefficients[,1] + 1.96*summary(glm_cto)$coefficients[,2]) )

###########################
# %% synthetic random forest
###########################

### treatment using subset data with only no treatment AB 
AB_0 <- data_first_treatment_12ow_relevant_0.4%>% filter (type_AB_nose == 0) ## control
regF_0 <- rfsrcSyn(
  as.formula(paste("outcome_4 ~ ", paste(vars_confounders, collapse="+"),sep="")),
  data  = AB_0)

### using that synthetic forest to predict outcome of all participants (so wether they had treatment of not)
pred.Syn_type_0 <- rfsrcSyn(object = regF_0, newdata = data_first_treatment_12ow_relevant_0.4)

### treatment using subset data with treatment AB 
AB_1 <- data_first_treatment_12ow_relevant_0.4%>% filter (AB_nose_infection == 1)

regF_1 <- rfsrcSyn(
  as.formula(paste("outcome_4 ~ ", paste(vars_confounders, collapse="+"),sep="")),
  data  = AB_1)
  
pred.Syn_AB_1 <- rfsrcSyn(object = regF_1, newdata = data_first_treatment_12ow_relevant_0.4_na.omit_v0)

# calculate difference
data_first_treatment_12ow_relevant_0.4_na.omit_v0$delta <- (pred.Syn_AB_1$rfSynPred$predicted -  pred.Syn_type_0$rfSynPred$predicted)[,2]


fit <- glm(
  as.formula(paste("delta ~ ", paste(vars_confounders, collapse="+"),sep="")),
  data  = data_first_treatment_12ow_relevant_0.4_na.omit_v0, family = gaussian)

fit_delta<- summary(fit)
fit_delta

###loop for type 1-5
for (j in 1:5) {
  
  AB_j <- data_first_treatment_12ow_relevant_0.4_na.omit_v0%>% filter (type_AB_nose == j)
  
  regF_j <- rfsrcSyn(outcome_4 ~ age + nr_medication + kanker_morb + coronhartz_morb +  RA_morb + alcmisb_morb + COPD_morb + hartritme_morb + migraine_morb + nr_prescriptions_AB +nr_chron3 + practice_size + chrnek_morb + hartfalen_morb + nr_contacts_infection + nr_contacts_resp + postalcode, data  = AB_j)
  
  pred.Syn_type_j <- rfsrcSyn(object = regF_j, newdata = data_first_treatment_12ow_relevant_0.4_na.omit_v0)
  # calculate difference
  data_first_treatment_12ow_relevant_0.4_na.omit_v0$delta_j <- (pred.Syn_type_j$rfSynPred$predicted -  pred.Syn_type_0$rfSynPred$predicted)[,2]
  
  
  fit_j <- glm(delta_j ~ age + nr_medication + kanker_morb + coronhartz_morb +  RA_morb + alcmisb_morb + COPD_morb + hartritme_morb + migraine_morb + nr_prescriptions_AB +nr_chron3 + practice_size + chrnek_morb + hartfalen_morb + nr_contacts_infection + nr_contacts_resp + postalcode,  data  = data_first_treatment_12ow_relevant_0.4_na.omit_v0, family = gaussian)
  fit_delta_j<- summary(fit_j)
  
  variable <- names(fit_j$coefficients)
  coef <- coef(fit_delta_j)[,1]
  p_value <- coef(fit_delta_j)[,4]
  
  table = data.frame(variable, coef, p_value)
  #write.csv(table, file=sprintf('P:/NZR/NZR-001/Innovatieve Onderzoeksmethoden/2. Machine learning antibiotica bij ontstekingen/03. Dataverwerking/4. research files/fit_delta_%s.csv',j))
  write.csv(table, file=sprintf('H:/qww/AMC/Utrecht/NIVEL/Thamar/20180823/fit_delta_%s.csv',j))
  
  data_first_treatment_12ow_relevant_0.4_na.omit_v0[,ncol(data_first_treatment_12ow_relevant_0.4_na.omit_v0)+1]<-data_first_treatment_12ow_relevant_0.4_na.omit_v0$delta_j
  names(data_first_treatment_12ow_relevant_0.4_na.omit_v0)[ncol(data_first_treatment_12ow_relevant_0.4_na.omit_v0)]<-paste0("delta_",j)
}
###########################
# %% make confusion matrix
###########################

#confusionmatrix <- subset(variables_omit_type, select = c(outcome_4, AB_nose_infection, delta))
#confusionmatrix$delta_dicho[confusionmatrix$delta < 0 ] <- 0
#confusionmatrix$delta_dicho[confusionmatrix$delta > 0 ] <- 1
#table(confusionmatrix$delta_dicho, confusionmatrix$outcome_4)
#table(confusionmatrix$delta_dicho, confusionmatrix$AB_nose_infection)

###########################
# %% model validation 
###########################

## test-set performance

set.seed(2)
train<-sample(1:nrow(data_first_treatment_12ow_relevant_0.4_na.omit_v0),0.8*nrow(data_first_treatment_12ow_relevant_0.4_na.omit_v0))
data_first_treatment_12ow_relevant_0.4_na.omit_v0.train<-data_first_treatment_12ow_relevant_0.4_na.omit_v0[train,]
class(data_first_treatment_12ow_relevant_0.4_na.omit_v0.train)

data_first_treatment_12ow_relevant_0.4_na.omit_v0.test<-data_first_treatment_12ow_relevant_0.4_na.omit_v0[-train,]
outcome_4.test<-data_first_treatment_12ow_relevant_0.4_na.omit_v0$outcome_4[-train]  

### Build SRF model in train subset, predic SRF model in test subset

regF <- rfsrcSyn(
  as.formula(paste("outcome_4 ~ ", paste(vars_confounders, collapse="+"),sep="")),
  data  = data_first_treatment_12ow_relevant_0.4_na.omit_v0.train)

### using that synthetic forest to predict outcome of test participants (so wether they had treatment of not)
pred.Syn <- rfsrcSyn(object = regF, newdata = data_first_treatment_12ow_relevant_0.4_na.omit_v0.test)


pred<- pred.Syn$ rfSynPred$predicted
data_first_treatment_12ow_relevant_0.4_na.omit_v0$pred_truetreatment<- ifelse(data_first_treatment_12ow_relevant_0.4_na.omit_v0$AB_nose_infection == 1,pred[1], pred[0])

sum(data_first_treatment_12ow_relevant_0.4_na.omit_v0$pred_truetreatment, na.rm=TRUE)

library("Hmisc")
plsmo(pred_truetreatment,data_first_treatment_12ow_relevant_0.4_na.omit_v0$AB_nose_infection == 1, xlab="predictions", ylab="% events") # This is a smoother and gives the calibration graph. The default of smoothing window is 2/3. Calibration is not bad.
plsmo(pred_truetreatment,data_first_treatment_12ow_relevant_0.4_na.omit_v0$AB_nose_infection == 1, xlab="predictions", ylab="% events", f=1/3) # bumpy

plsmo(pred_truetreatment, data_first_treatment_12ow_relevant_0.4_na.omit_v0$AB_nose_infection == 1, xlab="predictions", ylab="% events", group = data_first_treatment_12ow_relevant_0.4_na.omit_v0$age > median(data_first_treatment_12ow_relevant_0.4_na.omit_v0$age)) # check per group (here above and below median BMI)
# how to define imaginary line vs full line???

range(pred_truetreatment, na.rm=TRUE) # very good range but let's also look now at distribution of predicitons:
plsmo(pred_truetreatment, data_first_treatment_12ow_relevant_0.4_na.omit_v0$AB_nose_infection == 1, xlab="predictions", ylab="% events", datadensity=T, na.rm=TRUE) # you need to zoom in on the graph to see the tick marks
hist(pred_truetreatment) # or just use a histogram


data_first_treatment_12ow_relevant_0.4_na.omit_v0.test$OutcomeCstat<-0
data_first_treatment_12ow_relevant_0.4_na.omit_v0$treatCstat[as.numeric(data_first_treatment_12ow_relevant_0.4_na.omit_v0$AB_nose_infection)==2]<-1
data_first_treatment_12ow_relevant_0.4_na.omit_v0$treatCstat[as.numeric(data_first_treatment_12ow_relevant_0.4_na.omit_v0$AB_nose_infection)==1]<-0

somers2(as.numeric(pred[,2]),as.numeric(data_first_treatment_12ow_relevant_0.4_na.omit_v0.test$treatCstat))

  
auc <- somers2(pred_truetreatment, data_first_treatment_12ow_relevant_0.4_na.omit_v0$treatCstat, na.rm = TRUE )  
(auc <- somers2(pred_truetreatment, data_first_treatment_12ow_relevant_0.4_na.omit_v0$AB_nose_infection )["C"]) # this is the same as somers2(predictions, pima$testC == 1)["C"] but more compact
brier   <- mean((pred_t1-data_first_treatment_12ow_relevant_0.4_na.omit_v0$AB_nose_infection)^2)

library("rms")
# Let us get unbiased estimates of the performance
dd <- datadist(data_first_treatment_12ow_relevant_0.4_na.omit_v0)  ###how to explain???
options(datadist='dd')

model <- lrm( as.formula(paste("outcome_4 ~ ", paste(vars_confounders, collapse="+"),sep="")), x=T, y=T, data=data_first_treatment_12ow_relevant_0.4_na.omit_v0)
v <- validate(model, method="boot", B=100, pr=F)
v # look at table of (bias corrected) performance measure
v[1,5] # This is the adjusted Dxy
v[1,5]/2 + 0.5 # and this is the AUC
#write.csv(pimaWithout, file="MyFile.csv")



###########################
# %% subgroup analysis
###########################
library(subgroup.discovery)

PRIM <- subset(data_first_treatment_12ow_relevant_0.4_na.omit_v0, select = c(age, nr_medication, kanker_morb, coronhartz_morb,  RA_morb , alcmisb_morb ,
                                           COPD_morb, hartritme_morb, migraine_morb,   nr_prescriptions_AB, nr_chron3,practice_size,chrnek_morb ,hartfalen_morb, delta))
prim.table<-data.frame(matrix(ncol=(6)))
colnames(prim.table)<-c("alpha","beta","number_cov","rule_cov","number_div","rule_div")

set.seed(1234)

for (i in 1:10){
  alpha<-i/100
  for (j in 1:10){
    beta<-j/100
    p.cov<-subgroup.discovery::prim.cover(delta~., data = PRIM,
                                          peeling.quantile=alpha,
                                          min.support=beta, 
                                          plot= FALSE, 
                                          optimal.box= "2se")
    rule_cov<-p.cov$covers[[1]]$superrule
    p.div <- prim.diversify(delta ~ .,
                                      data = PRIM,
                                      n = 4,
                                      peeling.quantile = alpha,
                                      min.support = beta,
                                      plot = FALSE,
                                      optimal.box = "2se")
    rule_div<-p.div$attempts[[1]]$superrule
    
    prim.table[10*i+j-10,]<-c(alpha,beta,length(rule_cov),toString(rule_cov),length(rule_div),toString(rule_div))
    
  }
}
# PRIM with delta coming from  AB TYPE 0 VS 1
PRIM_1 <- subset(data_first_treatment_12ow_relevant_0.4_na.omit_v0, select = c(age, nr_medication, kanker_morb, coronhartz_morb,  RA_morb , alcmisb_morb ,
                                                                       COPD_morb, hartritme_morb, migraine_morb,   nr_prescriptions_AB, nr_chron3,practice_size,chrnek_morb ,hartfalen_morb, delta_1))
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

# PRIM with delta coming from  AB TYPE 0 VS 2
PRIM_2 <- subset(data_first_treatment_12ow_relevant_0.4_na.omit_v0, select = c(age, nr_medication, kanker_morb, coronhartz_morb,  RA_morb , alcmisb_morb ,
                                                                       COPD_morb, hartritme_morb, migraine_morb,   nr_prescriptions_AB, nr_chron3,practice_size,chrnek_morb ,hartfalen_morb, delta_2))
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

# PRIM with delta coming from  AB TYPE 0 VS 3
PRIM_3 <- subset(data_first_treatment_12ow_relevant_0.4_na.omit_v0, select = c(age, nr_medication, kanker_morb, coronhartz_morb,  RA_morb , alcmisb_morb ,
                                                                       COPD_morb, hartritme_morb, migraine_morb,   nr_prescriptions_AB, nr_chron3,practice_size,chrnek_morb ,hartfalen_morb, delta_3))
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

# PRIM with delta coming from  AB TYPE 0 VS 4
PRIM_4 <- subset(data_first_treatment_12ow_relevant_0.4_na.omit_v0, select = c(age, nr_medication, kanker_morb, coronhartz_morb,  RA_morb , alcmisb_morb ,
                                                                       COPD_morb, hartritme_morb, migraine_morb,   nr_prescriptions_AB, nr_chron3,practice_size,chrnek_morb ,hartfalen_morb, delta_4))
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

save(regF_0, file = "H:/qww/AMC/Utrecht/NIVEL/syntax/20181113.RData")
load("H:/qww/AMC/Utrecht/NIVEL/syntax/20181113.RData")

save(g,file="test.RData",compress=F)

save.image()
load(".RData")

system.time(load('test.RData'))
system.time(saveRDS(g,file="test2.RData",compress=F))
system.time(a<-readRDS('test2.RData'))

gc()

