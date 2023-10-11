# ---- R ANALYSIS CODE FOR "AGE SPECIFIC EFFECTS OF WORKING TIME CONTROL ON
# ---- PSYCHOLOGICAL DETACHMENT AND RELAXATION IN THE GERMAN WORKING POPULATION
# ---- CODE AUTHOR: MARIEBELLE KAUS 

# ---- Data used in analyses: BAuA Working Time Survey 2015 and 2017.
# ---- A scientific use file (SUF) of the data set can be requested at
# ---- https://www.baua.de/DE/Angebote/Forschungsdaten/Arbeitszeitbefragung.html
# ---- The SUF includes all variables that are relevant for this study.
# ---- Please follow the BAuA instructions to merge the 2015 and 2017 datasets

# ---- License CC BY 4.0


# preparing R

install.packages("devtools")
library(devtools)
devtools::install_github("kthorstmann/horst")

library(psych)
library(haven)
library(dplyr)
library(Hmisc)
library(lavaan)
library(horst)
library(tibble)
library(estimatr)
library(broom)
library(margins)
library(ggplot2)

setwd("") # SET YOUR WORKING DIRECTORY


# load data

d_orig <- read_sav("BAuA-AZB2015_2017_merged.sav") # RENAME IF NEEDED


# filter sample

d_filt_wav1 <- 
  dplyr::filter(d_orig, 
                AX103==1|2|3|7|8,        #dependently employed 2015
                AX201>=35,               #full time working 2015
                AXAlter_vgr>=23,         #not younger than 23
                AXAlter_vgr<=63)         #not older than 63

d_filt_stle <- 
  dplyr::filter(d_filt_wav1, 
                (BXErstteilnahme==2015 & #participating 2015 AND 2017
                  BX201>=35 &            #full time working 2017
                  BX112a==1) |           #no change of employer
                 is.na(BXErstteilnahme)) #participating only 2015


# select variables

d_slct_stle <- 
  dplyr::select(d_filt_stle, 
                BXErstteilnahme, #first participation (na=only 2015)
                AXS1, #gender
                AX701, #marriage status 2015
                AX702, #married & living with partner 2015
                AX703, #unmarried & living with partner 2015
                AX800_vgr, #number of individuals in household 2015
                AX803, #child under 18 in household 2015
                AX700c, #qualification
                AX119, #supervisor function 2015
                BX418, #physical/brain work 2017
                AX217, #working times between 7am and 7pm 2015
                AX201, #duration of typical weekly working times 2015
                AX400_3, #frequency of time+performance pressure 2015
                AXAlter_vgr, #age 2015
                AX211, #working time control (wtc): start and end      
                AX243_1, #wtc: breaks                                  
                AX243_2, #wtc: hours                                   
                AX243_3, #wtc: days and vacation                       
                BX308_1, #detachment 2017
                BX308_2) #relaxation 2017

# define variables for study

d_slct_stle$stayer <- 
  factor(with(d_slct_stle, 
              ifelse((is.na(BXErstteilnahme)),0,1))) #staying in study

d_slct_stle$gender_w <- 
  factor(with(d_slct_stle, 
              ifelse((AXS1==2),1,0))) #gender (female)

d_slct_stle$partner_yes <- 
  factor(with(d_slct_stle, 
              ifelse(((((AX701==1)|(AX701==5))&(AX702==1)))|
                      (((AX701!=1)&(AX701!=5))&(AX703==1)),1,0))) #liv. w. partner

d_slct_stle$child_yes <- 
 factor(with(d_slct_stle, 
             ifelse(((AX800_vgr>1)&(AX803==1)),1,0))) #living with children

d_slct_stle$education_high <- 
  factor(with(d_slct_stle, 
              ifelse((AX700c==3)|(AX700c==4)|(AX700c==5)|
                     (AX700c==6),1,0))) #higher education

d_slct_stle$supervisor_yes <- 
 factor(with(d_slct_stle, 
             ifelse((AX119==1),1,0))) #supervisor function

d_slct_stle$intellectualwork_yes <- 
  factor(with(d_slct_stle, 
              ifelse((BX418==1),1,0))) #intellectual work

d_slct_stle$daywork_yes <- 
  factor(with(d_slct_stle, 
              ifelse((AX217==1),1,0))) #regular daywork

d_slct_stle$wtduration <- 
  as.numeric(d_slct_stle$AX201) #duration of typical weekly working time

d_slct_stle$jobpressure_frequent <- 
  factor(with(d_slct_stle, 
              ifelse((AX400_3==1),1,0))) #frequent jobpressure

d_slct_stle$age <- 
  as.numeric(d_slct_stle$AXAlter_vgr) #age

d_slct_stle$wtc_start <- 
  as.numeric(d_slct_stle$AX211) #wtc start                

d_slct_stle$wtc_breaks <- 
  as.numeric(d_slct_stle$AX243_1) #wtc breaks             

d_slct_stle$wtc_hours <- 
  as.numeric(d_slct_stle$AX243_2) #wtc hours             

d_slct_stle$wtc_days <- 
  as.numeric(d_slct_stle$AX243_3) #wtc days            

d_slct_stle$wtc_scale <- 
  rowMeans(subset
           (d_slct_stle, select=c(wtc_start, wtc_breaks, wtc_hours, wtc_days)), 
           na.rm=T) #wtc scale

d_slct_stle$ia_age_wtcstart <- 
  (d_slct_stle$age)*(d_slct_stle$wtc_start) #interaction age*wtc start

d_slct_stle$ia_age_wtcbreaks <- 
  (d_slct_stle$age)*(d_slct_stle$wtc_breaks) #interaction age*wtc breaks

d_slct_stle$ia_age_wtchours <- 
  (d_slct_stle$age)*(d_slct_stle$wtc_hours) #interaction age*wtc hours

d_slct_stle$ia_age_wtcdays <- 
  (d_slct_stle$age)*(d_slct_stle$wtc_days) #interaction age*wtc days

d_slct_stle$ia_age_wtcscale <- 
  (d_slct_stle$age)*(d_slct_stle$wtc_scale) #interaction age*wtc scale

d_slct_stle$detachment <- 
  as.numeric(d_slct_stle$BX308_1) #detachment

d_slct_stle$relaxation <- 
  as.numeric(d_slct_stle$BX308_2) #relaxation


# create dataset for attrition analysis

d_stle <- 
  dplyr::select(d_slct_stle,
                stayer,
                gender_w,
                partner_yes,
                child_yes,
                education_high,
                supervisor_yes,
                intellectualwork_yes,
                daywork_yes,
                wtduration,
                jobpressure_frequent,
                age,
                wtc_start,
                wtc_breaks,
                wtc_hours,
                wtc_days,
                wtc_scale,
                ia_age_wtcstart,
                ia_age_wtcbreaks,
                ia_age_wtchours,
                ia_age_wtcdays,
                ia_age_wtcscale,
                detachment,
                relaxation)


# overview for attrition analysis

sink(file="attrition_analysis.txt")

print("n of full-time working employees aged 23-63 responding at wave1:")
nrow(d_filt_wav1)

print("n of leavers after wave1:")
nrow(subset(d_stle, d_stle$stayer==0))

print("n of stayers, still in full-time employment, with no change of employer:")
nrow(subset(d_stle, d_stle$stayer==1))


# logistic regression for attrition analysis

print("logistic regression:")
attr_lr <- glm(formula = stayer ~ 
               gender_w +
               partner_yes +
               child_yes +
               education_high +
               supervisor_yes +
               daywork_yes +
               wtduration +
               jobpressure_frequent +
               age +
               wtc_scale,
               family = binomial,
               data = d_stle)

summary(attr_lr)

print("regression coefficients as OR:")
exp(cbind(coef(attr_lr), confint(attr_lr)))

print("chi2:")
attr_lr_chi2 <- attr_lr$null.deviance - attr_lr$deviance
attr_lr_chi2

print("p value for chi2:")
1-pchisq(attr_lr_chi2, df=attr_lr$df.null-attr_lr$df.residual)


# t-tests for attrition analysis

print("series of independent t-tests:")
t.test((as.numeric(d_stle$gender_w)-1) ~ stayer,
       data=d_stle, alternative="two.sided")
t.test((as.numeric(d_stle$partner_yes)-1) ~ stayer, 
       data=d_stle, alternative="two.sided")
t.test((as.numeric(d_stle$child_yes)-1) ~ stayer, 
       data=d_stle, alternative="two.sided")
t.test((as.numeric(d_stle$education_high)-1) ~ stayer, 
       data=d_stle, alternative="two.sided")
t.test((as.numeric(d_stle$supervisor_yes)-1) ~ stayer, 
       data=d_stle, alternative="two.sided")
t.test((as.numeric(d_stle$daywork_yes)-1) ~ stayer, 
       data=d_stle, alternative="two.sided")
t.test(wtduration ~ stayer, data=d_stle, alternative="two.sided")
t.test((as.numeric(d_stle$jobpressure_frequent)-1) ~ stayer, 
       data=d_stle, alternative="two.sided")
t.test(age ~ stayer, data=d_stle, alternative="two.sided")
t.test(wtc_scale ~ stayer, data=d_stle, alternative="two.sided")


# variances for variance comparisons for attrition analysis

print("variances for variance comparisons:")

d_le <- subset(d_stle, d_stle$stayer==0) #create leavers subsample
d    <- subset(d_stle, d_stle$stayer==1) #create stayers subsample (=final data)

as.data.frame(# creating table with variance in stayers sample, variance 
              # in leavers sample, variance in whole (st+le) sample
  tibble(
  variable = c("gender_w", 
               "partner_yes", 
               "child_yes", 
               "education_high", 
               "supervisor_yes", 
               "daywork_yes",
               "wtduration",
               "jobpressure_frequent",
               "age",
               "wtc_scale"),
  var_stayers = num(c(var(as.numeric(d$gender_w), na.rm = T),
                      var(as.numeric(d$partner_yes), na.rm = T),
                      var(as.numeric(d$child_yes), na.rm = T),
                      var(as.numeric(d$education_high), na.rm = T),
                      var(as.numeric(d$supervisor_yes), na.rm = T),
                      var(as.numeric(d$daywork_yes), na.rm = T),
                      var(as.numeric(d$wtduration), na.rm = T),
                      var(as.numeric(d$jobpressure_frequent), na.rm = T),
                      var(as.numeric(d$age), na.rm = T),
                      var(as.numeric(d$wtc_scale), na.rm = T)), digits=3),
  var_leavers = num(c(var(as.numeric(d_le$gender_w), na.rm = T),
                      var(as.numeric(d_le$partner_yes), na.rm = T),
                      var(as.numeric(d_le$child_yes), na.rm = T),
                      var(as.numeric(d_le$education_high), na.rm = T),
                      var(as.numeric(d_le$supervisor_yes), na.rm = T),
                      var(as.numeric(d_le$daywork_yes), na.rm = T),
                      var(as.numeric(d_le$wtduration), na.rm = T),
                      var(as.numeric(d_le$jobpressure_frequent), na.rm = T),
                      var(as.numeric(d_le$age), na.rm = T),
                      var(as.numeric(d_le$wtc_scale), na.rm = T)), digits=3),
  var_all = num(c(var(as.numeric(d_stle$gender_w), na.rm = T),
                  var(as.numeric(d_stle$partner_yes), na.rm = T),
                  var(as.numeric(d_stle$child_yes), na.rm = T),
                  var(as.numeric(d_stle$education_high), na.rm = T),
                  var(as.numeric(d_stle$supervisor_yes), na.rm = T),
                  var(as.numeric(d_stle$daywork_yes), na.rm = T),
                  var(as.numeric(d_stle$wtduration), na.rm = T),
                  var(as.numeric(d_stle$jobpressure_frequent), na.rm = T),
                  var(as.numeric(d_stle$age), na.rm = T),
                  var(as.numeric(d_stle$wtc_scale), na.rm = T)), digits=3)))

# For variance comparisons, chi-square was calculated by the formula of 
# Hays (1988), which is [(N of stayers)−1] multiplied by (variance of stayers) 
# divided by (variance of whole sample). The N per variable can be found in 
# "descriptives_correlations_alpha_omega.txt"

sink(file=NULL)


# dataset with final sample, used in all following analyses

d <- subset(d_stle, d_stle$stayer==1)


# descriptives, correlations, alpha & omegaW

sink(file="descriptives_correlations_alpha_omega.txt") # as txt file

print("descriptives")
print(as.data.frame(tibble(data.frame(
  variables = c("stayer",
                "gender_w", 
                "partner_yes", 
                "child_yes", 
                "education_high", 
                "supervisor_yes",
                "intellectual_work",
                "daywork_yes",
                "wtduration",
                "jobpressure_frequent",
                "age",
                "wtc_start",
                "wtc_breaks",
                "wtc_hours",
                "wtc_days",
                "wtc_scale",
                "ia_age_wtc_start",
                "ia_age_wtc_breaks",
                "ia_age_wtc_hours",
                "ia_age_wtc_days",
                "ia_age_wtc_scale",
                "detachment",
                "relaxation"), 
  psych::describe(d, quant=c(.10,.25,.50,.75,.90),
                             check=T))), n=Inf, width = Inf))

print("correlations")
print(as.data.frame(tibble(tidy(rcorr(as.matrix(d), type="pearson")),
            na="na"), n=Inf, width=Inf)) #correlations table

print("wtc_scale: alpha")
psych::alpha(subset(d, select = c(wtc_start, wtc_breaks, wtc_hours, wtc_days)))

print("wtc_scale: cfa fit-measures")
wtcscale_l <- 'wtcscale_lavaan =~ wtc_start + wtc_breaks + wtc_hours + wtc_days'
fit <- cfa(wtcscale_l, data=d)
summary(fit, fit.measures=T)

print("wtc_scale: omegaW")
omegaW(fit)

sink(file=NULL)


# plots for inspection of mlr assumptions

list_wtc <-
  list(d$wtc_scale,
        d$wtc_scale,
        d$wtc_start,
        d$wtc_start,
        d$wtc_breaks,
        d$wtc_breaks,
        d$wtc_hours,
        d$wtc_hours,
        d$wtc_days,
        d$wtc_days)

list_rec <-
  rep(list(d$detachment, 
           d$relaxation),5)

list_names <- 
  c("detachment_wtc_scale",
    "relaxation_wtc_scale",
    "detachment_wtc_start",
    "relaxation_wtc_start",
    "detachment_wtc_breaks",
    "relaxation_wtc_breaks",
    "detachment_wtc_hours",
    "relaxation_wtc_hours",
    "detachment_wtc_days",
    "relaxation_wtc_days")

for(i in 1:length(list_wtc)) {   

  tiff(paste0("assmp_", list_names[i], "_scat_plot.tiff"),  
       units="in", width=5, height=4, res=100)
  plot(list_wtc[[i]], list_rec[[i]])        # scatter plots of bivariate
  abline(lm(list_rec[[i]] ~ list_wtc[[i]])) # regression models (wtc-recovery)
  dev.off()
  
  mlr_ols <- 
    lm(formula = list_rec[[i]] ~ 
                 gender_w + 
                 partner_yes + 
                 child_yes + 
                 education_high + 
                 supervisor_yes + 
                 intellectualwork_yes + 
                 daywork_yes +
                 wtduration + 
                 jobpressure_frequent +
                 age +
                 list_wtc[[i]] +
                 age*list_wtc[[i]], 
       data=d)

  tiff(paste0("mlr_asmp_", list_names[i], "_resi_plot.tiff"), 
       units="in", width=5, height=4, res=100)  
  plot(mlr_ols, which=1) #residuals plots of mlr models
  dev.off()

  tiff(paste0("mlr_asmp_",  list_names[i],  "_scal_plot.tiff"), 
       units="in", width=5, height=4, res=100)  
  plot(mlr_ols, which=3) #scale location plots of mlr models
  dev.off()
  
  tiff(paste0("mlr_asmp_",  list_names[i], "_hist_st.tiff"), 
       units="in", width=5, height=4, res=100) 
  hist(rstandard(mlr_ols)) #histogram of standardized residuals
  dev.off()

  tiff(paste0("mlr_asmp_", list_names[i], "_hist_unst.tiff"), 
       units="in", width=5, height=4, res=100)  
  hist(residuals(mlr_ols)) #histogram of unstandardized residuals
  dev.off()

  tiff(paste0("mlr_asmp_", list_names[i], "_qq_plot.tiff"), 
       units="in", width=5, height=4, res=100) 
  plot(mlr_ols, which=2) #qq plot
  dev.off()
  
}


# mlr with heteroskedasticity-consistent covariance matrix estimator (H2)

list_wtc <-
  list(wtc_scale = d$wtc_scale,
       wtc_scale = d$wtc_scale,
       wtc_start = d$wtc_start,
       wtc_start = d$wtc_start,
       wtc_breaks = d$wtc_breaks,
       wtc_breaks = d$wtc_breaks,
       wtc_hours = d$wtc_hours,
       wtc_hours = d$wtc_hours,
       wtc_days = d$wtc_days,
       wtc_days = d$wtc_days)

list_rec <-
  rep(list(detachment = d$detachment, 
           relaxation = d$relaxation),5)

list_names <- 
  c("detachment_wtc_scale",
    "relaxation_wtc_scale",
    "detachment_wtc_start",
    "relaxation_wtc_start",
    "detachment_wtc_breaks",
    "relaxation_wtc_breaks",
    "detachment_wtc_hours",
    "relaxation_wtc_hours",
    "detachment_wtc_days",
    "relaxation_wtc_days")

mlr1 <- # conducting mlr models
  lapply(paste(names(list_rec), 
               "~ 
               gender_w +
               partner_yes +
               child_yes +
               education_high +
               supervisor_yes +
               intellectualwork_yes +
               daywork_yes +
               wtduration +
               jobpressure_frequent +
               age"),  
         function(x) lm_robust(as.formula(x), data = d))

mlr2 <- 
  lapply(paste(names(list_rec), 
               "~ 
               gender_w +
               partner_yes +
               child_yes +
               education_high +
               supervisor_yes +
               intellectualwork_yes +
               daywork_yes +
               wtduration +
               jobpressure_frequent +
               age +", 
               names(list_wtc)),  
         function(x) lm_robust(as.formula(x), data = d))

mlr3 <- 
  lapply(paste(names(list_rec), 
               "~ 
               gender_w +
               partner_yes +
               child_yes +
               education_high +
               supervisor_yes +
               intellectualwork_yes +
               daywork_yes +
               wtduration +
               jobpressure_frequent +
               age +", 
               names(list_wtc), "+", 
               names(list_wtc), "*age"),  
         function(x) lm_robust(as.formula(x), data = d))

sink(file="mlr.txt") # save mlr summaries as txt file

for (i in 1:length(mlr3)) {
  print(list_names[i])
  
  print("Model 1:")
  print(as.data.frame(tibble(tidy(mlr1[[i]])))) # summary 1
  print(as.data.frame(tibble(glance(mlr1[[i]])))) # summary 2
  
  print("Model 2:")
  print(as.data.frame(tibble(tidy(mlr2[[i]])))) # summary 1
  print(as.data.frame(tibble(glance(mlr2[[i]])))) # summary 2
  
  print("Model:")
  print(as.data.frame(tibble(tidy(mlr3[[i]])))) # summary 1
  print(as.data.frame(tibble(glance(mlr3[[i]])))) # summary 2
}

sink(file=NULL)


# average marginal effects tables

sink(file="ame_tables.txt") #save ame tables as txt file

for (i in 1:length(mlr3)) {
 
  print(list_names[i])
  
  print("AME for each year of age:")
  
  print(summary(margins::margins(mlr3[[i]], data=d, 
        variables=paste(names(list_wtc)[[i]]), at=list(age=c(23:63)))))
  
  print("AME for age mean:")
  
  print(summary(margins::margins(mlr3[[i]], data=d, 
                variables=paste(names(list_wtc)[[i]]), 
                at=list(age=mean(d$age)))))
  }

sink(file=NULL)


# create datasets for plots 

for (i in 1:length(mlr3)) { 
  
  write.csv2(cplot(mlr3[[i]], "age", # datasets for ame plots
             dx=paste(names(list_wtc)[[i]]), 
             what = "effect", draw=F), 
             file=paste0("ame_plotdata_", list_names[i], ".csv"))
  
}


# plotting average marginal effects 

list_amepl_yax <- rep(c("Average Marginal Effect on Detachment", 
                        "Average Marginal Effect on Relaxation"), 5)

for(i in 1:length(list_names)) {
  ameplot_d <- 
    read.csv2(paste0("ame_plotdata_", list_names[i], ".csv"))
  
  jpeg(paste0("ame_plot_", list_names[i], ".jpeg"), # save following as jpeg
       units="in", width=5, height=4, res=100)
  
  print(
    ggplot(ameplot_d, aes(x=xvals, y=yvals))+
      scale_x_continuous(breaks = c(23,28,33,38,43,48,53,58,63), 
                         minor_breaks=c(23,24,25,26,27,28,29,30,31,32,
                                        33,34,35,36,37,38,39,40,41,42,
                                        43,44,45,46,47,48,49,50,51,52,
                                        53,54,55,56,57,58,59,60,61,62,63), 
                         name = "Age", expand = c(0,0))+
      scale_y_continuous(name = list_amepl_yax[i], 
                         limits=c(-0.09,0.2), n.breaks=6)+
      theme_classic()+
      theme(panel.grid.minor.x =element_line(), 
            panel.grid.major.x =element_line(), 
            text=element_text(size=14, family="Arial"))+
      geom_ribbon(aes(x=xvals, ymin=lower, ymax=upper, group=1), alpha=0.2)+
      geom_line(aes(x=xvals, y=yvals, group=1), linewidth=0.7)+
      geom_hline(yintercept = 0, linetype="dashed", linewidth=0.7))

    dev.off()
}
