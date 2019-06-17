#Overview####
#This script uses the baseindiv_spread object created in uspTieringTemplate
#baseindiv_spread - Individual level, spread wide format, derived from tidy and indiv datasets

#This script explores the data, creates the necessary categorizations and binary variables for service utilization, and 
#then runs and assesses the cox models.

#First explore age and LOS variables a bit more to determine categorical variables for specific LOC of interest

#make sure binary variables in this run of model are recoded appropriately/included 
#and/or commented out for defining baseindiv_cats

#do all the model output renames for this specific model when defining CoxOutputs

#Libraries####
library(skimr)
library(tidyverse)
library(survival)
library(survminer)
library(magrittr)
library(broom)  

#UPDATE In-Text Parameters####
#This updates all labels, captions, axes accordingly
IndexLOC <- 'Detox'
IndexDaystoReadmit <- '90'
IndexTimeframe <- '10/2016 and 9/2018'
ReadmitLOC <- 'Detox'
IndexAgeGroup <- 'Adult'

#figuring out categories for dif variables####

#age
ggplot(baseindiv_spread, aes(x = Age)) + geom_bar() #bimodal; with very low use under 26; 
#went back and added agecat2 -  18-30, 31-40, 41-50, 51+



#los
ggplot(baseindiv_spread, aes(x = LOS)) + geom_bar()
table(baseindiv_spread$LOS)

baseindiv_spread %>%
  mutate(LOScat = ifelse(LOS < 4 , "LT 5 days", "5+ days")) %>% #4 is the break bc it's really a count of nights, so 4 nights = 5 days
  group_by(LOScat) %>%
  summarize(n = n(),
            perc = round(n/nrow(baseindiv_spread) * 100, 2)) #going to run w/under 14/14+ bc it's what I used in orig; but also check 10 or less/11+

#baseindiv_spread %>% 
 # ggplot(aes(x = LOS)) +
  #geom_histogram(binwidth = 5)




#eligibility 
hist(baseindiv_spread$Yr1Prior_Elig, breaks = 2)

baseindiv_spread %>%
  mutate(eligcat = ifelse(is.na(Yr1Prior_Elig), "no elig",
                          ifelse(Yr1Prior_Elig >= 182, "More than 50%", "LT50%"))) %>%
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
         -ends_with("EAC"), -ends_with("RTFA"), -ends_with("JoH"))  %>%
  mutate(After_23HrBed = as.factor(ifelse(is.na(After_23HrBed), 'No', 'Yes')),
         After_ACT = as.factor(ifelse(is.na(After_ACT), "No", "Yes")),
 #        After_AcutePartial = as.factor(ifelse(is.na(After_AcutePartial), "No", "Yes")),
         After_BHSICM = as.factor(ifelse(is.na(After_BHSICM), "No", "Yes")),
         After_CIRC = as.factor(ifelse(is.na(After_CIRC), "No", "Yes")),
        # After_Detox = as.factor(ifelse(is.na(After_Detox), "No", "Yes")),
         After_IOP = as.factor(ifelse(is.na(After_IOP), "No", "Yes")),
         After_MAT = as.factor(ifelse(is.na(After_MAT), "No", "Yes")),
         After_OP = as.factor(ifelse(is.na(After_OP), "No", "Yes")),
         After_Rehab = as.factor(ifelse(is.na(After_Rehab), "No", "Yes")),
         After_Subacute = as.factor(ifelse(is.na(After_Subacute), "No", "Yes")),
         After_TCM = as.factor(ifelse(is.na(After_TCM), "No", "Yes"))
         ) %>% #converting after services to binary
  mutate(Yr1Prior_23HrBed = as.factor(ifelse(is.na(Yr1Prior_23HrBed), 'No', 'Yes')),
         Yr1Prior_ACT = as.factor(ifelse(is.na(Yr1Prior_ACT), "No", "Yes")),
#         Yr1Prior_AcutePartial = as.factor(ifelse(is.na(Yr1Prior_AcutePartial), "No", "Yes")),
         Yr1Prior_BHSICM = as.factor(ifelse(is.na(Yr1Prior_BHSICM), "No", "Yes")),
         Yr1Prior_CIRC = as.factor(ifelse(is.na(Yr1Prior_CIRC), "No", "Yes")),
       #  Yr1Prior_Detox = as.factor(ifelse(is.na(Yr1Prior_Detox), "No", "Yes")),
         Yr1Prior_IOP = as.factor(ifelse(is.na(Yr1Prior_IOP), "No", "Yes")),
         Yr1Prior_MAT = as.factor(ifelse(is.na(Yr1Prior_MAT), "No", "Yes")),
         Yr1Prior_OP = as.factor(ifelse(is.na(Yr1Prior_OP), "No", "Yes")),
         Yr1Prior_Rehab = as.factor(ifelse(is.na(Yr1Prior_Rehab), "No", "Yes")),
         Yr1Prior_Subacute = as.factor(ifelse(is.na(Yr1Prior_Subacute), "No", "Yes")),
         Yr1Prior_TCM = as.factor(ifelse(is.na(Yr1Prior_TCM), "No", "Yes"))
         ) %>% #converting prior services to binary
  mutate(Yr1Prior_AnyAIP = as.factor(ifelse(is.na(N_AIPDC_1YrPrior), "No", "Yes")),
         LOScat = as.factor(ifelse(LOS < 4, "LT 5 days", "5+ days")),
         eligcat = as.factor(ifelse(is.na(Yr1Prior_Elig), "No Elig", 
                                 ifelse(Yr1Prior_Elig <= 182, "Less than 50%", "50% +" ))),
         AgeCat2 = factor(AgeCat2, order = F))  %>%
  mutate(Flag_Readmit = ifelse(Flag_Readmit == "Yes", 1, 0)) %>%
  select(-Yr1Prior_Elig, -ASD_Measure_Year, -ID_Measure_Year, -N_AIPDC_1YrPrior, -LOC_Label) 


colnames(baseindiv_cats)

skim(baseindiv_cats)

str(baseindiv_cats$Flag_Readmit)


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


#By age
ReadmxAge <- survfit(survival_object ~ baseindiv_cats$AgeCat2)

ggsurvplot(ReadmxAge, data = baseindiv_cats, pval = TRUE,
           title = "Survival Probability by Age in AIP",
           legend.title = "",
           xlab = "Days since Discharge")


#Cox Model####
fit.coxph <- coxph(survival_object ~ 
                     Gender_Label + RaceCat + AgeCat2 +
                     LOScat + FirstEpEver +
                   After_23HrBed + #After_ACT + #After_AcutePartial + 
                     After_BHSICM + #After_CIRC + #After_Detox +
                     After_IOP + After_MAT + After_OP + After_Rehab + 
                      After_Subacute + After_TCM + 
                   Yr1Prior_23HrBed + #Yr1Prior_ACT + #Yr1Prior_AcutePartial + 
                      Yr1Prior_BHSICM + #Yr1Prior_CIRC + #Yr1Prior_Detox +
                     Yr1Prior_IOP + Yr1Prior_MAT + 
                      Yr1Prior_OP + Yr1Prior_Rehab + Yr1Prior_Subacute + Yr1Prior_TCM +
                   Yr1Prior_AnyAIP +  eligcat,
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


CoxOutputs %>% distinct(term, factors) %>% View() #check to make sure all factors below are appropriately renamed

CoxOutputs %<>%
  mutate(factors = ifelse(term=="Gender_LabelFemale", "Female",
                          ifelse(term=="RaceCatHispanic", "Hispanic (ref: White)",
                                 ifelse(term=="RaceCatBlack", "Black (ref: White)",
                                        ifelse(term=="RaceCatOther", "Other (ref: White)",
                                               ifelse(term=="AgeCat231 to 40", "31 to 40 (ref: 18-30yo)",
                                                      ifelse(term=="AgeCat241 to 50", "41-50 (ref: 18-30yo)",
                                                             ifelse(term=="AgeCat251+", "51+ (ref: 18-30yo)",
                                                                    
                           
                              ifelse(term=="LOScatLT 5 days", "LOS Under 5 (ref: 5+)",
                                        
                                                ifelse(term=="FirstEpEverY", "First AIP Ever",
                                                       ifelse(term=="After_23HrBedYes", "FUP: 23 Hr Bed",
                                                              ifelse(term=="After_ACTYes", "FUP: ACT", 
                                                                    
                    ifelse(term=="After_BHSICMYes", "FUP: BHSI-CM",
                           ifelse(term=="After_CIRCYes", "FUP: CIRC",
                                  
                                  ifelse(term=="After_IOPYes", "FUP: IOP",
                                         ifelse(term=="After_MATYes", "FUP: MAT",
                                                ifelse(term=="After_OPYes", "FUP: OP",
                                                       ifelse(term=="After_RehabYes", "FUP: Rehab",
                                                              ifelse(term=="After_SubacuteYes", "FUP: Subacute",
                                                                     ifelse(term=="After_TCMYes", "FUP: TCM",
                    ifelse(term=="Yr1Prior_23HrBedYes", "Yr1Prior: 23 Hr Bed",
                           ifelse(term=="Yr1Prior_ACTYes", "Yr1Prior: ACT", 
                                  ifelse(term=="Yr1Prior_AcutePartialYes", "Yr1Prior: Acute Partial",
                                         ifelse(term=="Yr1Prior_BHSICMYes", "Yr1Prior: BHSI-CM",
                                                ifelse(term=="Yr1Prior_CIRCYes", "Yr1Prior: CIRC",
                                                      
                                                       ifelse(term=="Yr1Prior_IOPYes", "Yr1Prior: IOP",
                                                              ifelse(term=="Yr1Prior_MATYes", "Yr1Prior: MAT",
                                                                     ifelse(term=="Yr1Prior_OPYes", "Yr1Prior: OP",
                                                                            ifelse(term=="Yr1Prior_RehabYes", "Yr1Prior: Rehab",
                                                                                   ifelse(term=="Yr1Prior_SubacuteYes", "Yr1Prior: Subacute",
                                                                                          ifelse(term=="Yr1Prior_TCMYes", "Yr1Prior: TCM",

                ifelse(term == "eligcatLess than 50%", "Yr1Prior: Elig < 50% of Yr (ref: 50+%)",
                       ifelse(term == "eligcatNo Elig", "Yr1Prior: Elig 0 Days (ref: 50+%)",
                            ifelse(term=="Yr1Prior_AnyAIPYes", "Yr1Prior: AIP", "ERROR")
                            )))))))))))))))))))))))))))))))))
                                                               

#plotting results of cox####
#all factors
CoxOutputs %>% 
  ggplot(aes(x = factors, y = HR, color = RiskImpact)) +
  geom_point( size = 4) +
  geom_linerange(aes(x = factors, ymin = HRconf.low, ymax = HRconf.high, color = RiskImpact), 
                 stat = 'identity', 
                 size = 5, alpha = .3) +
  geom_hline(yintercept = 1, size = 2, alpha = .4) +
  labs(title = paste0("Impact of Service Utilization and Demographics \non Risk of ", IndexDaystoReadmit ," Day Readmission"),
       subtitle = paste0(IndexLOC, " Discharges Among ", IndexAgeGroup, "s, ", IndexTimeframe),
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
  labs(title = paste0("Impact of Service Utilization and Demographics \non Risk of ",IndexDaystoReadmit ," Day Readmission"),
       subtitle = paste0(IndexLOC, " Discharges Among ", IndexAgeGroup, "s, ", IndexTimeframe),
       x = "Factors",
       y = "Hazard Ratio (1 = Not different from reference group)",
       color = "Risk compared to ref group") +
  scale_y_sqrt(breaks = seq(0,4,.5)) +
  scale_color_manual(values = c("#2daf38", "#db0404", "#777676")) +
  coord_flip() +
  theme_cleveland() + 
  theme(panel.background = element_rect(fill = "#f2f2f2"))
