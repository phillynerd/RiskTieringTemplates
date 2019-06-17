#Overview####
#This script uses the baseindiv_spread object created in uspTieringTemplate
#baseindiv_spread - Individual level, spread wide format, derived from tidy and indiv datasets

#This script explores the data, creates the necessary categorizations and binary variables for service utilization, and 
#then runs and assesses the cox models.

#Prior specs for kids tiering model, since this is an update (but only using last year of service):
#Gender
#Race
#Age: 14 vs under 14
#FirstAIPever
#Dx ASD and/or ID
#LOS (<10, 11-20, 21-30, 31+) -> explore this a bit more
#DHS
#302 v 201
#AfterServiceFlags/PriorServiceFlags, including flag for no service utilization, although that flag may just be redundant
#Prior DHS/Prior AIP
#CBH eligible > 50%


#Libraries####
library(skimr)
library(tidyverse)
library(survival)
library(survminer)
library(magrittr)
library(broom)  


#figuring out categories for dif variables####

#age
ggplot(baseindiv_spread, aes(x = Age)) + geom_bar() #confirms use of 14+ vs under 14 as cut; consistent w prior analysis

#los
ggplot(baseindiv_spread, aes(x = LOS)) + geom_bar()

baseindiv_spread %>%
  mutate(LOScat = ifelse(LOS <= 10, "10 and under", 
                         ifelse(LOS <= 20, "11 - 20",
                                ifelse(LOS <= 30, "21-30", "31+")))) %>%
  group_by(LOScat) %>%
  summarize(n = n(),
            perc = round(n/nrow(baseindiv_spread) * 100, 2)) #going to keep this bc it's what I used in orig

hist(baseindiv_spread$LOS, 
     breaks = seq(from = 0, to = 300, by = 7))

hist(baseindiv_spread$LOS, 
     breaks = seq(from = 0, to = 300, by = 10))

#eligibility 
hist(baseindiv_spread$Yr1Prior_Elig, breaks = 2)

baseindiv_spread %>%
  mutate(eligcat = ifelse(Yr1Prior_Elig >= 180, "More than 50%", "LT50%")) %>%
  group_by(eligcat) %>%
  summarize(n = n()) %>%
  mutate(perc = n/nrow(baseindiv_spread))

#N AIPs in yr1 prior
table(baseindiv_spread$N_AIPDC_1YrPrior) #go with 0 or 1+ as categories, since most are no prior hx of AIP


#Creating dataset for analysis & Defining Cats####
baseindiv_cats <- baseindiv_spread %>%
  ungroup() %>% #bc base table is currently grouped by masterBK, which prevents all factors from working
  select(-N_AIPAdmits_1YrAfter, -AuthorizationNumber, -Readmit_Auth, -Readmit_SB, 
         -N_AIPDC_2YrPrior, -N_AIPDC_3YrPrior,
         -starts_with("Yr2"), -starts_with("Yr3"), 
         -ends_with("HostHomes"), -ends_with("SteppingStones"))  %>%
  mutate(After_AcutePartial = as.factor(ifelse(is.na(After_AcutePartial), 'No', 'Yes')),
         After_BHRS_CTSS = as.factor(ifelse(is.na(After_BHRS_CTSS), "No", "Yes")),
         After_FBS = as.factor(ifelse(is.na(After_FBS), "No", "Yes")),
         After_OP = as.factor(ifelse(is.na(After_OP), "No", "Yes")),
         After_Rehab = as.factor(ifelse(is.na(After_Rehab), "No", "Yes")),
         After_RTF = as.factor(ifelse(is.na(After_RTF), "No", "Yes")),
         After_STS = as.factor(ifelse(is.na(After_STS), "No", "Yes")),
         After_TCM = as.factor(ifelse(is.na(After_TCM), "No", "Yes"))) %>% #converting after services to binary
  mutate(Yr1Prior_AcutePartial = as.factor(ifelse(is.na(Yr1Prior_AcutePartial), "No", "Yes")),
          Yr1Prior_BHRS_CTSS = as.factor(ifelse(is.na(Yr1Prior_BHRS_CTSS), "No", "Yes")),
          Yr1Prior_FBS = as.factor(ifelse(is.na(Yr1Prior_FBS), "No", "Yes")),
          Yr1Prior_OP = as.factor(ifelse(is.na(Yr1Prior_OP), "No", "Yes")),
          Yr1Prior_Rehab = as.factor(ifelse(is.na(Yr1Prior_Rehab), "No", "Yes")),
          Yr1Prior_RTF = as.factor(ifelse(is.na(Yr1Prior_RTF), "No", "Yes")),
          Yr1Prior_STS = as.factor(ifelse(is.na(Yr1Prior_STS), "No", "Yes")),
          Yr1Prior_TCM = as.factor(ifelse(is.na(Yr1Prior_TCM), "No", "Yes")),
          Yr1Prior_DHS = as.factor(ifelse(is.na(Yr1Prior_DHS), "No", "Yes"))) %>% #converting prior services to binary
  mutate(Yr1Prior_AnyAIP = as.factor(ifelse(is.na(N_AIPDC_1YrPrior), "No", "Yes")),
         Age14up = as.factor(ifelse(Age <14, "Under 14", "14+")),
         LOScat = as.factor(ifelse(LOS <= 10, "10 and under", 
                          ifelse(LOS <= 20, "11 - 20",
                                ifelse(LOS <= 30, "21-30", "31+")))),
         eligcat = as.factor(ifelse(is.na(Yr1Prior_Elig), "Less than 50%", 
                                 ifelse(Yr1Prior_Elig <= 182, "Less than 50%", "50% +" ))),
         ASDID = as.factor(ifelse(ASD_Measure_Year == "Y" | ID_Measure_Year == "Y", "ASD or ID", "No ASD or ID")),
         Voluntary = as.factor(ifelse(LOC_Label == "(100-1) ACUTE HOSPITAL SERVICES", "Voluntary", "Involuntary"))) %>%
  mutate(Flag_Readmit = ifelse(Flag_Readmit == "Yes", 1, 0)) %>%
  select(-Yr1Prior_Elig, -ASD_Measure_Year, -ID_Measure_Year, -N_AIPDC_1YrPrior, -LOC_Label) 


colnames(baseindiv_cats)

skim(baseindiv_cats)

str(baseindiv_cats$Flag_Readmit)


#used this code to spot check conversions from NA to 0
#sum(is.na(baseindiv_spread$Yr1Prior_DHS))

#K-M Curves####

survival_object <- Surv(time = baseindiv_cats$SurvivalDays, event = baseindiv_cats$Flag_Readmit)
survival_object

#By LOS
ReadmxLOS <- survfit(survival_object ~ baseindiv_cats$LOScat)
summary(ReadmxLOS)

ggsurvplot(ReadmxLOS, data = baseindiv_cats, pval = TRUE,
           title = "Survival Probability by LOS in AIP",
           legend.title = "", 
           xlab = "Days since Discharge")

#By DHS during
ReadmxDHS <- survfit(survival_object ~ baseindiv_cats$Flag_Active_DHS_Status)
ggsurvplot(ReadmxDHS, data = baseindiv_cats, pval = TRUE,
           title = "Survival Probability by DHS in AIP",
           legend.title = "",
           xlab = "Days since Discharge")

#By age
ReadmxAge <- survfit(survival_object ~ baseindiv_cats$Age14up)
ggsurvplot(ReadmxAge, data = baseindiv_cats, pval = TRUE,
           title = "Survival Probability by Age in AIP",
           legend.title = "",
           xlab = "Days since Discharge")

ReadmxAge2 <- survfit(survival_object ~ baseindiv_cats$AgeCat)
ggsurvplot(ReadmxAge2, data = baseindiv_cats, pval = TRUE,
           title = "Survival Probability by Age in AIP",
           legend.title = "",
           xlab = "Days since Discharge")

table(baseindiv_cats$AgeCat)


#By ASD/ID
ReadmxASDID <- survfit(survival_object ~ baseindiv_cats$ASDID)
ggsurvplot(ReadmxASDID, data = baseindiv_cats, pval = TRUE,
           title = "Survival Probability by ASD/ID in AIP",
           legend.title = "",
           xlab = "Days since Discharge")

#Cox Model####
fit.coxph <- coxph(survival_object ~ 
                     Gender_Label+ RaceCat+ Age14up+ ASDID+ 
                    MH_SA+ LOScat+ Flag_Active_DHS_Status+ Voluntary+ FirstEpEver+
                   After_AcutePartial+ After_BHRS_CTSS+ After_FBS+ After_OP+ After_Rehab+ 
                      After_RTF+ After_STS+ After_TCM+
                   Yr1Prior_AcutePartial+ Yr1Prior_BHRS_CTSS+ Yr1Prior_FBS+ Yr1Prior_OP+ Yr1Prior_Rehab+ 
                      Yr1Prior_RTF+ Yr1Prior_STS+ Yr1Prior_TCM+
                   Yr1Prior_DHS+ Yr1Prior_AnyAIP+ eligcat,
                     data = baseindiv_cats)

ggforest(fit.coxph, data = baseindiv_cats)

#without broom
#coxCI<-as.data.frame(summary(fit.coxph)$conf.int)
#coxCI %<>%
 # rownames_to_column(var = "Variable")


#coxCoef<-as.data.frame(summary(fit.coxph)$coefficients)
#coxCoef %<>%
 # rownames_to_column(var = "Variable")


#CoxOutputs <- coxCI %>%
 # inner_join(select(coxCoef, Variable,'Pr(>|z|)', 'se(coef)', z), by = "Variable") %>%
#  rename(pvalue = 'Pr(>|z|)',
 #        se.coef = 'se(coef)')

#with broom
CoxOutputs <- fit.coxph %>% 
  tidy() %>%
  mutate(HR = exp(estimate),
         sig = as.factor(ifelse(p.value <= .05, "Yes", "No")),
         HRconf.low = exp(conf.low),
         HRconf.high = exp(conf.high),
         RiskImpact = as.factor(ifelse(sig == "No", "No Impact",
                    ifelse(HR < 1, "Decreases Risk", "Increases Risk"))))


CoxOutputs %<>% 
  mutate(factors = ifelse(term=="Gender_LabelFemale", "Female",
                          ifelse(term=="RaceCatHispanic", "Hispanic (ref: Black)",
                                 ifelse(term=="RaceCatWhite", "White (ref: Black)",
                                        ifelse(term=="RaceCatOther", "Other (ref: Black)",
                                               ifelse(term=="Age14upUnder 14", "Under 14 (ref: 14+)",
                                                      ifelse(term=="ASDIDNo ASD or ID", "No ASD/ID",
                                                             ifelse(term=="MH_SASA", "SA Hx",
                                                                    ifelse(term=="LOScat11 - 20", "LOS 11-20 (ref: <= 10)",
                    ifelse(term=="LOScat21-30", "LOS 21-30 (ref: <= 10)",
                           ifelse(term=="LOScat31+", "LOS 31+ (ref: <= 10)",
                                  ifelse(term=="Flag_Active_DHS_StatusYes", "DHS during AIP",
                                         ifelse(term=="VoluntaryVoluntary", "Voluntary Commitment",
                                                ifelse(term=="FirstEpEverY", "First AIP Ever",
                                                       ifelse(term=="After_AcutePartialYes", "FUP: Acute Partial",
                                                              ifelse(term=="After_BHRS_CTSSYes", "FUP: BHRS/CTSS", 
                                                                     ifelse(term=="After_FBSYes", "FUP: FB",
                    ifelse(term=="After_OPYes", "FUP: OP",
                           ifelse(term=="After_RehabYes", "FUP: Rehab",
                                  ifelse(term=="After_RTFYes", "FUP: RTF",
                                         ifelse(term=="After_STSYes", "FUP: STS",
                                                ifelse(term=="After_TCMYes", "FUP: TCM",
                                                       ifelse(term=="Yr1Prior_AcutePartialYes","YrPrior: Acute Partial",
                    ifelse(term=="Yr1Prior_BHRS_CTSSYes", "YrPrior: BHRS/CTSS",
                           ifelse(term=="Yr1Prior_FBSYes", "YrPrior: FB",
                                  ifelse(term=="Yr1Prior_OPYes", "YrPrior: OP",
                                         ifelse(term=="Yr1Prior_RehabYes", "YrPrior: Rehab",
                                                ifelse(term=="Yr1Prior_RTFYes", "YrPrior: RTF",
                                                       ifelse(term=="Yr1Prior_STSYes", "YrPrior: STS",
                                                              ifelse(term=="Yr1Prior_TCMYes", "YrPrior: TCM",
                                                                     ifelse(term=="Yr1Prior_DHSYes", "YrPrior: DHS",
                                                                            ifelse(term == "eligcatLess than 50%", "Elig < 50% of Yr (ref: 50+%)",
                                                                              ifelse(term=="Yr1Prior_AnyAIPYes", "Yr1Prior: AIP", "ERROR"))))))))))
                                                              ))))))))))))))))))))))) 

#plotting results of cox####
#all factors
CoxOutputs %>% 
  ggplot(aes(x = factors, y = HR, color = RiskImpact)) +
  geom_point( size = 4) +
  geom_linerange(aes(x = factors, ymin = HRconf.low, ymax = HRconf.high, color = RiskImpact), 
                 stat = 'identity', 
                 size = 5, alpha = .3) +
  geom_hline(yintercept = 1, size = 2, alpha = .4) +
  labs(title = "Impact of Service Utilization and Demographics \non Risk of 180 Day Readmission",
       subtitle = "AIP Discharges Among Children, 7/2013 - 6/2018",
       x = "Factors",
       y = "Hazard Ratio (1 = Not different from reference group)",
       color = "Risk compared to ref group") +
  scale_y_sqrt(breaks = seq(0,4,.5)) +
  scale_color_manual(values = c("#2daf38", "#db0404", "#777676")) +
  coord_flip() +
  theme_cleveland() +
  theme(panel.background = element_blank())

#Sig factors only
CoxOutputs %>% 
  filter(sig == "Yes") %>% 
  ggplot(aes(x = factors, y = HR, color = RiskImpact)) +
  geom_point( size = 4) +
  geom_linerange(aes(x = factors, ymin = HRconf.low, ymax = HRconf.high, color = RiskImpact), 
                 stat = 'identity', 
                 size = 5, alpha = .3) +
  geom_hline(yintercept = 1, size = 2, alpha = .4) +
  geom_text(aes(label = round(HR,1)), vjust = -1, size = 3) +
  labs(title = "Impact of Service Utilization and Demographics \non Risk of 180 Day Readmission",
       subtitle = "AIP Discharges Among Children, 7/2013 - 6/2018",
       x = "Factors",
       y = "Hazard Ratio (1 = Not different from reference group)",
       color = "Risk compared to ref group") +
  scale_y_sqrt(breaks = seq(0,4,.5)) +
  scale_color_manual(values = c("#2daf38", "#db0404", "#777676")) +
  coord_flip() +
  theme_cleveland() + 
  theme(panel.background = element_rect(fill = "#f2f2f2"))
