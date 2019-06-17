#Overview####
#This is a template that can be used to clean all uspTieringAnalysisTemplate outputs.  There are some sections 
#that will require updating depending on the actual specifications of the individual run, but they should be minimal.


#SQL parameters####
#paste specific SQL parameters below#
#USE [SSRS_SP]
#GO

#DECLARE	@return_value int

#EXEC	@return_value = [dbo].[uspTierAnalysisTemplate]
#@PeriodBegin = N'07/01/2013',
#@PeriodEnd = N'06/30/2018',
#@CohortEnd = N'12/31/2018',
#@Days = 180,
#@Age = 2,
#@IndexLOC = N'100.001, 100.004',
#@ReadmitLOC = N'100.001, 100.004'

#SELECT	'Return Value' = @return_value

#GO

#Libraries####
library(tidyverse)
library(lubridate)
library(skimr)
library(visdat)
library(tidylog)

#UPDATE importing data ####

SQLoutput <- "SQLoutput_4-5-2019.csv"

basedata <- read_csv(SQLoutput)

skim(basedata)

#cleaning and casting####
#No changes needed to this section

#casting factors
basedata$Gender_Label <- as_factor(basedata$Gender_Label)
basedata$RaceEthnic_Label <- as_factor(basedata$RaceEthnic_Label)
basedata$ASD_Measure_Year <- as_factor(basedata$ASD_Measure_Year)
basedata$ID_Measure_Year <- as_factor(basedata$ID_Measure_Year)
basedata$LOC_Label <- as_factor(basedata$LOC_Label)
basedata$FirstEpEver <- as_factor(basedata$FirstEpEver)
basedata$MH_SA <- as_factor(basedata$MH_SA)
basedata$BA_Category <- as_factor(basedata$BA_Category)
basedata$Measure_Name <- as_factor(basedata$Measure_Name)

#casting as strings
basedata$AuthorizationNumber <- as.character(basedata$AuthorizationNumber)
basedata$Client_Master_BK <- as.character(basedata$Client_Master_BK)

##dates
basedata$ServiceBegin <- mdy(basedata$ServiceBegin)
basedata$ServiceEnd <- mdy(basedata$ServiceEnd)


#dealing with variables with NAs as string, recasting as necessary after "NA" is replaced with NA 
basedata$Readmit_Auth[basedata$Readmit_Auth == 0] <- NA
basedata$Readmit_Auth <- as.character(basedata$Readmit_Auth)

basedata$Readmit_SB[basedata$Readmit_SB == "NA"] <- NA
basedata$Readmit_SB <- mdy(basedata$Readmit_SB)

basedata$Measure_Period[basedata$Measure_Period == "NA"] <- NA
basedata$Measure_Period <- as_factor(basedata$Measure_Period)

basedata$LOC[basedata$LOC == "NA"] <- NA
basedata$LOC <- as_factor(basedata$LOC)

skim(basedata)




# Creating basetidy_startingpoint####

MaxSurvival <- 180

#This code creates basetidy_startingpoint - selecting columns, filtering, and adding new columns
#this is my initial base long format tidy dataset, includes all service information, with several 
#filters applied after demographic exploration below.  

#This dataset will be used to later check things like short readmissions, short LOS, etc.
#Those analyses, which are in the next section, need to be checked manually depending on the specific analysis

basetidy_startingpoint <- basedata %>%
  select(-Ep180, -Post_Discharge_RTF_Start, -Post_Discharge_Days_to_RTF, 
         -Clinical_Category, -HasServ_Before_Readmit, 
         -Flag_BHRS_18plus_Mths_Before_Admission, -Flag_Active_DHS_Status) %>%
  mutate(LOS = as.numeric(ServiceEnd - ServiceBegin),
         Flag_Readmit = as_factor(ifelse(is.na(Readmit_Auth), "No", "Yes")),
         RaceCat = as_factor(ifelse(RaceEthnic_Label == "WHITE", "White",
                                    ifelse(RaceEthnic_Label == "BLACK OR AFRICAN AMERICAN", "Black",
                                           ifelse(RaceEthnic_Label == "HISPANIC", "Hispanic", 
                                                  ifelse(RaceEthnic_Label %in% c("OTHER", "ASIAN", "N.AMER.INDIAN/ALASKAN NATIVE"), "Other", NA))))),
         AgeCat = as.ordered(ifelse(Age<=25, "18 to 25",
                                    ifelse(Age <= 35, "26 to 35",
                                           ifelse(Age <= 45, "36 to 45", 
                                                  ifelse(Age <= 55, "46 to 55", "56+"))))), #based on info I saw below on age dist
         SurvivalDays = ifelse(is.na(Days_to_Readmit), MaxSurvival, Days_to_Readmit))

#UPDATE creating and exploring basetidy_startingpoint_indiv####

#DONT UPDATE THIS: This is the individual-level version of basetidy_startingpoint, which I later use to check quick readmits and short LOS

basetidy_startingpoint_indiv <- basetidy_startingpoint %>%
  group_by(Client_Master_BK) %>%
  select(-BA_Category, -Day_Count, -Measure_Name, -Measure_Period, -LOC, -RaceEthnic_Label)  %>%
  distinct() 

#UPDATE THESE AS NEEDED
#these next two will vary depending on what I find and what LOC's i'm working with;
#this will impact filters in final basetidy below

#checking out quick readmits -- should be 0 now with the other stuff filtered out in base dataset
basetidy_startingpoint_indiv %>%
  select(AuthorizationNumber, Readmit_Auth, Days_to_Readmit, LOS) %>%
  filter(Days_to_Readmit < 1) %>%
  ungroup() %>%
  count() #166 cases with days to readmit <= 1; 23 cases w same day readmit, decision; remove same day readmits since might be transfers

#checking out short LOS -- should be 0 with new base datasets
basetidy_startingpoint_indiv %>%
  select(LOS) %>%
  filter(LOS == 0) %>% #may need to change this parameter depending on what makes sense for the LOC
  ungroup() %>%
  summarize(n = n()) #593 1 day stays; 
#164 same day discharges: decision - remove all LOS == 0 days, going to update TidyIndiv set to reflect this


#UPDATE creating final basetidy####
#Based on findings and decisions made in previous sections, update the first part of the code
#Second part of the code should be fine for all cases, but doublecheck it

basetidy <- basetidy_startingpoint %>%
  filter(LOS > 0 & (is.na(Days_to_Readmit) | Days_to_Readmit > 1)) #filter added after first pass on demographics, removes LOS == 0 and days to readmit of 0 since could be transfer

basetidy <- basetidy %>%
  filter(!(BA_Category %in% c("Before") & Measure_Name %in% c("Prior Service") & is.na(LOC))) %>% #filters out day counts for services not included in LOC groupings in before
  filter(!(BA_Category %in% c("Before") & is.na(Measure_Period) & is.na(LOC))) %>% #filters out cases measure_period is blank, LOC is blank, in before bc all day counts are 0
  filter(!(BA_Category %in% c("After") & Measure_Name %in% c("After Service") & is.na(LOC) & Measure_Period %in% c(paste0(MaxSurvival, " Days")))) %>%  #filters out day counts for services not included in LOC groupings in after
  filter(!(BA_Category %in% c("After") & Measure_Name %in% c("IP Admissions") & is.na(Measure_Period))) %>% #filtering out days in AIP  where measure_period == NA always 0
  filter(!(BA_Category %in% c("Before") & Measure_Name %in% c("DHS"))) %>% #removes DHS before categories that aren't applicable for adults
  filter(!(BA_Category %in% c("Before") & Measure_Name %in% c("Additional Service") & LOC %in% c("(450-1) HOST HOMES - DHS CHILD", "(450-2) HOST HOMES - NON-DHS CHILD"))) #weird additional service grouping for adults

#creating basetidy_indiv from basetidy; for demographics only ####

#basetidy_indiv: creating unique individual dataset for demos/basics, excluding services
#Derived from RTFtidy; doesn't include service utilization info, just for demographics/characteristics of index

basetidy_indiv <- basetidy %>%
  group_by(Client_Master_BK) %>%
  select(-BA_Category, -Day_Count, -Measure_Name, -Measure_Period, -LOC, -RaceEthnic_Label)  %>%
  distinct() 

colnames(basetidy_indiv)

# basic demographics for individuals using basetidy_indiv####

#Gender
basetidy_indiv %>%
  group_by(Gender_Label) %>%
  summarize(n = n()) %>%
  mutate(percent = n/sum(n)*100)

#AgeCat
basetidy_indiv %>%
  group_by(AgeCat) %>%
  summarize(n = n()) %>%
  mutate(percent = n/sum(n)*100)

#RaceCat
basetidy_indiv %>%
  group_by(RaceCat) %>%
  summarize(n = n()) %>%
  mutate(percent = n/sum(n)*100) %>%
  arrange(desc(percent))

#LOS
basetidy_indiv %>%
  group_by(Flag_Readmit) %>%
  summarize(AvgLOS = mean(LOS),
            MedLOS = median(LOS),
            ModeLOS = mode(LOS),
            SDLOS = sd(LOS),
            IQRLOS = IQR(LOS),
            minLOS = min(LOS),
            maxLOS = max(LOS))


#LOC  
basetidy_indiv %>%
  group_by(LOC_Label) %>%
  summarize(n = n()) %>%
  mutate(percent = n/sum(n)*100) %>%
  arrange(desc(percent))

#MH_SA  
basetidy_indiv %>%
  group_by(MH_SA) %>%
  summarize(n = n()) %>%
  mutate(percent = n/sum(n)*100) %>%
  arrange(desc(percent))


#first Index ever
basetidy_indiv %>%
  group_by(FirstEpEver) %>%
  summarize(n = n()) %>%
  mutate(percent = n/sum(n)*100)

#Readmit flag 
basetidy_indiv %>%
  group_by(Flag_Readmit) %>%
  summarize(n = n()) %>%
  mutate(percent = n/sum(n)*100) %>%
  arrange(desc(percent))

#Days to readmit  
basetidy_indiv %>%
  group_by(Flag_Readmit) %>%
  filter(!is.na(Days_to_Readmit)) %>%
  summarize(AvgDTR = mean(Days_to_Readmit),
            MedDTR = median(Days_to_Readmit),
            ModeDTR = mode(Days_to_Readmit),
            SDDTR = sd(Days_to_Readmit),
            IQRDTR = IQR(Days_to_Readmit),
            minDTR = min(Days_to_Readmit),
            maxDTR = max(Days_to_Readmit))



#Creating basetidy_servsummary - avg days & N ppl using each serv cat  ####


basetidy_servsummary <- basetidy %>%
  group_by(Client_Master_BK, BA_Category, Measure_Name, Measure_Period, LOC) %>%
  select(Client_Master_BK, BA_Category, Measure_Name, Measure_Period, LOC, Day_Count) %>%
  summarize(Days = sum(Day_Count)) %>% #need this step bc some folks have 2 lines of data for same LOC. Not sure why stored proc code would do that.
  group_by (BA_Category, Measure_Name, Measure_Period, LOC) %>%
  summarize(N_UsingService = n(),
            AvgDays = mean(Days, na.rm = TRUE),
            MedDays = median(Days, na.rm = TRUE),
            Perc_UsingService = n()/nrow(basetidy_indiv) * 100) %>% 
  


View(basetidy_servsummary)


#Creating basetidy_aftersummary - adjusted summary data for after services ####
# taking into account the amount of time people were out  


#basic formula is total days in service / total days in community * 100 = days of service per 100 days in community
basetidy_aftersummary <- basetidy %>%
  filter(BA_Category %in% c("After") & Measure_Name %in% c("After Service")) %>%
  group_by(Client_Master_BK, BA_Category, Measure_Name, Measure_Period, LOC, SurvivalDays) %>%
  select(Client_Master_BK, BA_Category, Measure_Name, Measure_Period, LOC, Day_Count, SurvivalDays) %>%
  summarize(Days = sum(Day_Count)) %>%#need this step bc some folks have 2 lines of data for same LOC. Not sure why stored proc code would do that.
  mutate(RateDays100 = Days/SurvivalDays * 100) %>% 
  group_by (BA_Category, Measure_Name, Measure_Period, LOC) %>%
  summarize(N_UsingService = n(),
            Perc_UsingService = n()/nrow(basetidy_indiv) * 100, 
            AvgRateDays100 = mean(RateDays100, na.rm = TRUE))

View(basetidy_aftersummary)

#UPDATE Creating individual level complete dataset for modeling ####
#You will need to replace '180' with whatever the total time period factor is
#this dataset is the one i usually use in SPSS for risk modeling

colnames(basetidy)



baseindiv_spread <-basetidy %>% 
  select(Client_Master_BK, BA_Category, Measure_Name, Measure_Period, LOC, Day_Count) %>%
  mutate(Client_Master_BK = as.character(Client_Master_BK)) %>%
  group_by(Client_Master_BK, BA_Category, Measure_Name, Measure_Period, LOC) %>%
  summarize(NDays = sum(Day_Count)) %>%
  unite(TimeLOC, BA_Category, Measure_Name, Measure_Period, LOC) %>%
  spread(TimeLOC, NDays) %>%
  select(-("After_After Service_NA_NA")) %>% #colnames()
  rename(After_23HrBed = "After_After Service_180 Days_23-Hour Bed",
         After_ACT = "After_After Service_180 Days_ACT",          
         After_AcutePartial = "After_After Service_180 Days_Acute Partial Adults",                 
         After_BHSICM = "After_After Service_180 Days_BHSI Case Mgmt", 
         After_CIRC = "After_After Service_180 Days_CIRC",
         After_IOP = "After_After Service_180 Days_IOP",   
         After_MAT = "After_After Service_180 Days_Methadone",
         After_OP = "After_After Service_180 Days_Outpatient", 
         After_Rehab = "After_After Service_180 Days_Rehab",
         After_Subacute = "After_After Service_180 Days_Subacute",   
         After_TCM = "After_After Service_180 Days_TCM",
         N_AIPAdmits_1YrAfter = "After_IP Admissions_180 Days_IP",                 
         #prior - additional services, elig, aip
         Yr1Prior_EAC = "Before_Additional Service_Year 1_EAC",     
         Yr1Prior_JoH =  "Before_Additional Service_Year 1_JoH",
         Yr1Prior_RTFA =  "Before_Additional Service_Year 1_RTFA",
         Yr2Prior_EAC =  "Before_Additional Service_Year 2_EAC",     
         Yr2Prior_JoH = "Before_Additional Service_Year 2_JoH",
         Yr2Prior_RTFA = "Before_Additional Service_Year 2_RTFA",
         Yr3Prior_EAC =  "Before_Additional Service_Year 3_EAC",     
         Yr3Prior_JoH = "Before_Additional Service_Year 3_JoH",
         Yr3Prior_RTFA = "Before_Additional Service_Year 3_RTFA",
         Yr1Prior_Elig = "Before_Eligibility_Year 1_NA",                    
         Yr2Prior_Elig =  "Before_Eligibility_Year 2_NA",                    
         Yr3Prior_Elig =  "Before_Eligibility_Year 3_NA",                    
         N_AIPDC_1YrPrior = "Before_IP Discharges_Year 1_IP",                  
         N_AIPDC_2YrPrior =  "Before_IP Discharges_Year 2_IP",                  
         N_AIPDC_3YrPrior =  "Before_IP Discharges_Year 3_IP", 
         #prior yr 1
         Yr1Prior_23HrBed = "Before_Prior Service_Year 1_23-Hour Bed",
         Yr1Prior_ACT = "Before_Prior Service_Year 1_ACT",          
         Yr1Prior_AcutePartial = "Before_Prior Service_Year 1_Acute Partial Adults",                 
         Yr1Prior_BHSICM = "Before_Prior Service_Year 1_BHSI Case Mgmt", 
         Yr1Prior_CIRC = "Before_Prior Service_Year 1_CIRC",
         Yr1Prior_IOP = "Before_Prior Service_Year 1_IOP",   
         Yr1Prior_MAT = "Before_Prior Service_Year 1_Methadone",
         Yr1Prior_OP = "Before_Prior Service_Year 1_Outpatient", 
         Yr1Prior_Rehab = "Before_Prior Service_Year 1_Rehab",
         Yr1Prior_Subacute = "Before_Prior Service_Year 1_Subacute",   
         Yr1Prior_TCM = "Before_Prior Service_Year 1_TCM",
         #prior yr 2
         Yr2Prior_23HrBed = "Before_Prior Service_Year 2_23-Hour Bed",
         Yr2Prior_ACT = "Before_Prior Service_Year 2_ACT",          
         Yr2Prior_AcutePartial = "Before_Prior Service_Year 2_Acute Partial Adults",                 
         Yr2Prior_BHSICM = "Before_Prior Service_Year 2_BHSI Case Mgmt", 
         Yr2Prior_CIRC = "Before_Prior Service_Year 2_CIRC",
         Yr2Prior_IOP = "Before_Prior Service_Year 2_IOP",   
         Yr2Prior_MAT = "Before_Prior Service_Year 2_Methadone",
         Yr2Prior_OP = "Before_Prior Service_Year 2_Outpatient", 
         Yr2Prior_Rehab = "Before_Prior Service_Year 2_Rehab",
         Yr2Prior_Subacute = "Before_Prior Service_Year 2_Subacute",   
         Yr2Prior_TCM = "Before_Prior Service_Year 2_TCM",
         #prior yr 3
         Yr3Prior_23HrBed = "Before_Prior Service_Year 3_23-Hour Bed",
         Yr3Prior_ACT = "Before_Prior Service_Year 3_ACT",          
         Yr3Prior_AcutePartial = "Before_Prior Service_Year 3_Acute Partial Adults",                 
         Yr3Prior_BHSICM = "Before_Prior Service_Year 3_BHSI Case Mgmt", 
         Yr3Prior_CIRC = "Before_Prior Service_Year 3_CIRC",
         Yr3Prior_IOP = "Before_Prior Service_Year 3_IOP",   
         Yr3Prior_MAT = "Before_Prior Service_Year 3_Methadone",
         Yr3Prior_OP = "Before_Prior Service_Year 3_Outpatient", 
         Yr3Prior_Rehab = "Before_Prior Service_Year 3_Rehab",
         Yr3Prior_Subacute = "Before_Prior Service_Year 3_Subacute",   
         Yr3Prior_TCM = "Before_Prior Service_Year 3_TCM") %>%
  right_join(basetidy_indiv, by = "Client_Master_BK")

colnames(baseindiv_spread)

#write_excel_csv(baseindiv_spread, path = "baseindiv_spread.csv", na = "") 

#write.csv(basetidy_indiv, "basetidy_indiv.csv") 

#basetidy - long, cleaned dataset in tidy format
#write.csv(basetidy, "basetidy.csv") 

