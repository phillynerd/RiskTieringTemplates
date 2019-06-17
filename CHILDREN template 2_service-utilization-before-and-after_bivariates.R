#Overview####
#objects used here are all defined in 'CHILDREN template 1 for basic cleaning and reshaping'

#basetidy - long, cleaned dataset in tidy format
  #write.csv(basetidy, "basetidy.csv") 


#basetidy_indiv - derived from above dataset, doesn't include service utilization info, just for demographics/characteristics of index
  #write.csv(basetidy_indiv, "basetidy_indiv.csv") 

#baseindiv_spread - Individual level, spread wide format, derived from tidy and indiv datasets.  Essentially the format excel is in, minus service flags
  #write.csv(basetidy_indiv, "basetidy_indiv.csv") 

#UPDATE In-Text Parameters####
#This updates all labels, captions, axes accordingly

IndexLOC <- 'AIP'
IndexDaystoReadmit <- '180'
IndexTimeframe <- '7/2013 and 6/2018'
ReadmitLOC <- 'AIP'



#Libraries####
library(tidyverse)
library(lubridate)
library(stringr)
library(kableExtra)
library(gridExtra)
library(ggpubr)


#Distribution of Index Episodes by LOC####

basetidy_indiv %>%
  group_by(LOC_Label) %>%
  summarize(n = n()) %>%
  mutate(percent = n/sum(n)*100,
         LOC = word(LOC_Label, 1 )) %>%
  arrange(desc(percent)) %>%
  ggplot(aes(x = reorder(LOC, -percent), y = percent, fill = LOC_Label)) +
  geom_bar(stat = "identity") +
  labs(title = paste0("Index ", IndexLOC, " Episodes by LOC"), 
       subtitle = paste0("First ", IndexLOC, "  Discharge between ", IndexTimeframe),
       fill = "Level of Care") +
  xlab("Level of Care") +
  ylab("Percent of All Index Episodes") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "") 

#Creating Objects for Basic Demographic Table####
#Gender
Gender <- basetidy_indiv %>%
  group_by(Gender_Label) %>%
  summarize(n = n()) %>%
  mutate(percent = n/sum(n)*100,
         Demos = Gender_Label)

#AgeCat
Age <- basetidy_indiv %>%
  group_by(AgeCat) %>%
  summarize(n = n()) %>%
  mutate(percent = n/sum(n)*100,
         Demos = ordered(AgeCat, levels = c("10 and under", "11 to 13", "14 to 17"))) %>%
  arrange(Demos)

#RaceCat
Race <- basetidy_indiv %>%
  group_by(RaceCat) %>%
  summarize(n = n()) %>%
  mutate(percent = n/sum(n)*100,
         Demos = RaceCat) %>%
  arrange(desc(percent))

#MH_SA  
MHSA <- basetidy_indiv %>%
  mutate(MH_SA = recode(MH_SA, "MH" = "No", "SA" = "Yes")) %>%
  group_by(MH_SA) %>%
  summarize(n = n()) %>%
  mutate(percent = n/sum(n)*100,
         Demos = MH_SA) %>%
  arrange(desc(percent))

#ASD
ASD <- basetidy_indiv %>%
  mutate(ASD_Measure_Year = recode(ASD_Measure_Year, "N" = "No ASD", "Y" = "ASD Dx")) %>%
  group_by(ASD_Measure_Year) %>%
  summarize(n = n()) %>%
  mutate(percent = n/sum(n)*100,
         Demos = ASD_Measure_Year) %>%
  arrange(desc(percent)) 

#ID
ID <- basetidy_indiv %>%
  mutate(ID_Measure_Year = recode(ID_Measure_Year, "N" = "No ID", "Y" = "ID Dx")) %>%
  group_by(ID_Measure_Year) %>%
  summarize(n = n()) %>%
  mutate(percent = n/sum(n)*100,
         Demos = ID_Measure_Year) %>%
  arrange(desc(percent)) 

#ASD or ID
ASDID <- basetidy_indiv %>%
  mutate(ASDID = ifelse(ASD_Measure_Year == "Y" | ID_Measure_Year == "Y", "ASD or ID", "No ASD or ID")) %>%
  group_by(ASDID) %>%
  summarize(n = n()) %>%
  mutate(percent = n/sum(n)*100,
         Demos = ASDID) %>%
  arrange(desc(percent)) 



#BHRS 18+ months  

levels(basetidy_indiv$Flag_BHRS_18plus_Mths_Before_Admission)

BHRS <- basetidy_indiv %>%
  mutate(Flag_BHRS_18plus_Mths_Before_Admission = recode(Flag_BHRS_18plus_Mths_Before_Admission, 
                                                         "No" = "Does not have BHRS episode >= 18 months", 
                                                         "Yes" = "Has BHRS episode >= 18 months")) %>%
  group_by(Flag_BHRS_18plus_Mths_Before_Admission) %>%
  summarize(n = n()) %>%
  mutate(percent = n/sum(n)*100,
         ServUtil = Flag_BHRS_18plus_Mths_Before_Admission) %>%
  arrange(desc(percent))

#DHS  
levels(basetidy_indiv$Flag_Active_DHS_Status)

DHS <- basetidy_indiv %>%
  mutate(Flag_Active_DHS_Status = recode(Flag_Active_DHS_Status, 
                                         "No" = paste0("No DHS during ", IndexLOC), "Yes" = paste0("DHS during  ", IndexLOC))) %>%
  group_by(Flag_Active_DHS_Status) %>%
  summarize(n = n()) %>%
  mutate(percent = n/sum(n)*100,
         ServUtil = Flag_Active_DHS_Status) %>%
  arrange(desc(percent))

#first episode ever in this LOC
levels(basetidy_indiv$FirstEpEver)

Firstbase <- basetidy_indiv %>%
  mutate(FirstEpEver = recode(FirstEpEver, 
                              "N" = paste0("Prior ", IndexLOC, "  Hx"), "Y" = paste0("First ", IndexLOC, " ever"))) %>%
  group_by(FirstEpEver) %>%
  summarize(n = n()) %>%
  mutate(percent = n/sum(n)*100,
         ServUtil = FirstEpEver)

#Readmit flag 
levels(basetidy_indiv$Flag_Readmit)

Readmit <- basetidy_indiv %>%
  mutate(Flag_Readmit = recode(Flag_Readmit, 
                               "No" = "No Readmit", "Yes" = paste0("Readmit within ", IndexDaystoReadmit))) %>%
  group_by(Flag_Readmit) %>%
  summarize(n = n()) %>%
  mutate(percent = n/sum(n)*100,
         ServUtil = Flag_Readmit) %>%
  arrange(desc(percent))

#rbinding for demographics table####
Gender %>%
  bind_rows (Age, Race, MHSA, ASD, ID, ASDID) %>%
  select(Demos, n, percent) %>%
  mutate(percent = round(percent, 1)) %>%
  kable(caption = paste0("Member-Level Demographics during Index  ", IndexLOC, " Episode")) %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE) %>%
  group_rows("Gender", 1,2) %>%
  group_rows("Age at Admission", 3,5) %>%
  group_rows("Race", 6,9) %>%
  group_rows("Does member have SA hx", 10,11) %>%
  group_rows("Does member have ASD and/or ID diagnosis", 12, 17) 




#rbinding for service hx####
Firstbase %>%
  bind_rows(DHS, BHRS, Readmit) %>%
  select(ServUtil, n, percent) %>%
  mutate(percent = round(percent, 1)) %>%
  kable(caption = paste0("Select Service Use Related to Index  ", IndexLOC, " Episode")) %>%
  kable_styling(bootstrap_options = "striped", full_width = TRUE) %>%
  group_rows(paste0("Is this the member's first ", IndexLOC, " episode"), 1, 2) %>%
  group_rows("Is member DHS involved at time of Index Episode", 3, 4) %>%
  group_rows("Member's BHRS history at time of Index Episode", 5, 6) %>%
  group_rows(paste0("Does member readmit to ", ReadmitLOC, " within ", IndexDaystoReadmit ," days"), 7, 8)


#Defining objects for index episode LOS and readmit tables####

#Days to readmit  
DaystoRead <- basetidy_indiv %>%
  group_by(Flag_Readmit) %>%
  filter(!is.na(Days_to_Readmit)) %>%
  summarize(AvgDays = mean(Days_to_Readmit),
            MedDays = median(Days_to_Readmit),
            SDDays = sd(Days_to_Readmit),
            IQRDays = IQR(Days_to_Readmit),
            minDays = min(Days_to_Readmit),
            maxDays = max(Days_to_Readmit)) %>%
  mutate(Measure = ifelse(Flag_Readmit == "No", "No", "Days to Readmission")) %>%
  select(Measure, AvgDays, MedDays, SDDays, IQRDays, minDays, maxDays)



#LOS
LOSreadm <- basetidy_indiv %>%
  group_by(Flag_Readmit) %>%
  summarize(AvgDays = mean(LOS),
            MedDays = median(LOS),
            SDDays = sd(LOS),
            IQRDays = IQR(LOS),
            minDays = min(LOS),
            maxDays = max(LOS)) %>%
  mutate(Measure = ifelse(Flag_Readmit == "No", "LOS for those that did not readmit", "LOS for those that did readmit")) %>%
  select(Measure, AvgDays, MedDays, SDDays, IQRDays, minDays, maxDays)



OverallLOS <- data.frame ("Measure" = "Overall LOS", 
                          "AvgDays" = mean(basetidy_indiv$LOS), 
                          "MedDays" = median(basetidy_indiv$LOS), 
                          "SDDays" = sd(basetidy_indiv$LOS),
                          "IQRDays" = IQR(basetidy_indiv$LOS), 
                          "minDays" = min(basetidy_indiv$LOS), 
                          "maxDays" = max(basetidy_indiv$LOS))



#rbinding creating distribution tables for index####
OverallLOS %>%
  bind_rows(LOSreadm, DaystoRead) %>%
  mutate(AvgDays = round(AvgDays, 1),
         MedDays = round(MedDays, 1),
         SDDays = round(SDDays, 1),
         IQRDays = round(IQRDays, 1)) %>%
  kable(caption = "Distribution of Length of Stay and Days to Readmission") %>%
  kable_styling(bootstrap_options = "striped", full_width = TRUE) %>%
  group_rows("Length of Stay in base", 1, 3) %>%
  group_rows(paste0("Days to Readmission Among Those that Readmit (Max Days Possible = ", IndexDaystoReadmit ,")"), 4, 4)



#Boxplots describing index admission####
#overall LOS
basetidy_indiv %>%
  ggplot(aes(y = LOS)) +
  geom_boxplot(fill = 'blue', alpha = .2) +
  labs(title = paste0("Distribution of LOS in ", IndexLOC), 
       subtitle = paste0("Index Episodes between ", IndexTimeframe),
       caption = paste0("N members: ", nrow(basetidy_indiv))) +
  ylab("LOS (Days)") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "none")

#los by readmission status
basetidy_indiv %>%
  ggplot(aes(y = LOS, color = Flag_Readmit, fill = Flag_Readmit)) +
  geom_boxplot(alpha = .2) +
  labs(title = paste0("Distribution of LOS in ", IndexLOC, " by Readmission Status"), 
       subtitle = paste0("Index Episodes between ", IndexTimeframe), 
       caption = paste0("N members that readmit: ", nrow(basetidy_indiv[basetidy_indiv$Flag_Readmit == "Yes",]), 
                        "; \nN members that don't readmit: ", nrow(basetidy_indiv[basetidy_indiv$Flag_Readmit == "No",])),
       fill = "Did Member Readmit") +
  ylab("LOS (Days)") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.text = element_text(size = 8),
        #legend.title = element_text(size = 8),
        axis.title.y = element_blank()
  ) + #removes x axis labels
  guides(color = FALSE) #removes redundant legend for the color

#Distribution of Days to Readmission
basetidy_indiv %>%
  filter(Flag_Readmit == "Yes") %>%
  ggplot(aes(y = Days_to_Readmit, fill = Flag_Readmit)) +
  geom_boxplot(alpha = .2) +
  labs(title = "Distribution of In-Community Days \nAmong Those that Readmit", 
       subtitle = paste0("Index Episodes between ", IndexTimeframe),
       caption = paste0("N of Members that Readmit: ", 
                        nrow(basetidy_indiv[basetidy_indiv$Flag_Readmit == "Yes",]), 
                        "/", 
                        nrow(basetidy_indiv))
  ) +
  ylab("Days to Readmission") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "none")


#LOS by Subpopulations, boxplots ####
#coord_cartesian is to zoom in on boxplots if necessary/lots of outliers

##LOS by gender
figgen <- basetidy_indiv %>%
  ggplot(aes(x = Gender_Label, y = LOS, color = Gender_Label, fill = Gender_Label)) +
  geom_boxplot(alpha = .2, notch = TRUE) +
  labs(title = "Gender", 
       #subtitle = paste0("Index Episodes between ", IndexTimeframe), 
       #caption = paste0("N members that readmit: ", nrow(basetidy_indiv[basetidy_indiv$Flag_Readmit == "Yes",]), 
       #               "; \nN members that don't readmit: ", nrow(basetidy_indiv[basetidy_indiv$Flag_Readmit == "No",])),
       fill = "Gender") +
  ylab("LOS (Days)") +
  xlab("Gender") +
  theme(axis.text.x = element_text(),
        axis.ticks.x = element_blank(),
        #legend.text = element_text(size = 8),
        #legend.title = element_text(size = 8),
        legend.position = "none"
  ) + #removes x axis labels
  guides(color = FALSE)  +
  coord_cartesian(ylim = c(0, 60))

##LOS by race
figrace <- basetidy_indiv %>%
  ggplot(aes(x = RaceCat, y = LOS, color = RaceCat, fill = RaceCat)) +
  geom_boxplot(alpha = .2, notch = T) +
  labs(title = "Race", 
       #subtitle = paste0("Index Episodes between ", IndexTimeframe), 
       #caption = paste0("N members that readmit: ", nrow(basetidy_indiv[basetidy_indiv$Flag_Readmit == "Yes",]), 
       #               "; \nN members that don't readmit: ", nrow(basetidy_indiv[basetidy_indiv$Flag_Readmit == "No",])),
       fill = "Race") +
  ylab("LOS (Days)") +
  xlab("Race") +
  theme(axis.text.x = element_text(),
        axis.ticks.x = element_blank(),
        #legend.text = element_text(size = 8),
        #legend.title = element_text(size = 8),
        legend.position = "none"
  ) + #removes x axis labels
  guides(color = FALSE) +
  coord_cartesian(ylim = c(0, 60))

##LOS by age
figage <-basetidy_indiv %>%
  ggplot(aes(x = AgeCat, y = LOS, color = AgeCat, fill = AgeCat)) +
  geom_boxplot(alpha = .2, notch = T) +
  labs(title = "Age", 
       #subtitle = paste0("Index Episodes between ", IndexTimeframe), 
       #caption = paste0("N members that readmit: ", nrow(basetidy_indiv[basetidy_indiv$Flag_Readmit == "Yes",]), 
       #               "; \nN members that don't readmit: ", nrow(basetidy_indiv[basetidy_indiv$Flag_Readmit == "No",])),
       fill = "Age at Admission") +
  ylab("LOS (Days)") +
  xlab("Age Category at Admission") +
  theme(axis.text.x = element_text(),
        axis.ticks.x = element_blank(),
        #legend.text = element_text(size = 8),
        #legend.title = element_text(size = 8),
        legend.position = "none"
  ) + #removes x axis labels
  guides(color = FALSE) +
  coord_cartesian(ylim = c(0, 60))


##LOS by asd
figasd <-basetidy_indiv %>%
  ggplot(aes(x = ASD_Measure_Year, y = LOS, color = ASD_Measure_Year, fill = ASD_Measure_Year)) +
  geom_boxplot(alpha = .2, notch = T) +
  labs(title = "ASD Diagnosis", 
       #subtitle = paste0("Index Episodes between ", IndexTimeframe), 
       #caption = paste0("N members that readmit: ", nrow(basetidy_indiv[basetidy_indiv$Flag_Readmit == "Yes",]), 
       #               "; \nN members that don't readmit: ", nrow(basetidy_indiv[basetidy_indiv$Flag_Readmit == "No",])),
       fill = "Current ASD Dx") +
  ylab("LOS (Days)") +
  xlab("ASD Diagnosis") +
  theme(axis.text.x = element_text(),
        axis.ticks.x = element_blank(),
        #legend.text = element_text(size = 8),
        #legend.title = element_text(size = 8),
        legend.position = "none"
  ) + #removes x axis labels
  guides(color = FALSE) +
  coord_cartesian(ylim = c(0, 60))

##LOS by dhs
figdhs <- basetidy_indiv %>%
  ggplot(aes(x = Flag_Active_DHS_Status , y = LOS, color = Flag_Active_DHS_Status, fill = Flag_Active_DHS_Status)) +
  geom_boxplot(alpha = .2, notch = T) +
  labs(title = "DHS Status", 
       #subtitle = paste0("Index Episodes between ", IndexTimeframe), 
       #caption = paste0("N members that readmit: ", nrow(basetidy_indiv[basetidy_indiv$Flag_Readmit == "Yes",]), 
       #               "; \nN members that don't readmit: ", nrow(basetidy_indiv[basetidy_indiv$Flag_Readmit == "No",])),
       fill = "Current DHS Status") +
  ylab("LOS (Days)") +
  xlab("DHS Status") +
  theme(axis.text.x = element_text(),
        axis.ticks.x = element_blank(),
        #legend.text = element_text(size = 8),
        #legend.title = element_text(size = 8),
        legend.position = "none"
  ) + #removes x axis labels
  guides(color = FALSE) +
  coord_cartesian(ylim = c(0, 100))

figsubLOS <- ggarrange(figgen, figage, figrace, figasd, figdhs,
                       nrow = 2, ncol = 3
)

annotate_figure(figsubLOS,
                top = "Figure 3. Distribution of Index LOS by Select Subpopulations")


#readmission by Subpopulations, boxplots####
figgen1 <- basetidy_indiv %>%
  ggplot(aes(x = Gender_Label, y = Days_to_Readmit, color = Gender_Label, fill = Gender_Label)) +
  geom_boxplot(alpha = .2, notch = T) +
  labs(title = "Gender", 
       #subtitle = paste0("Index Episodes between ", IndexTimeframe), 
       #caption = paste0("N members that readmit: ", nrow(basetidy_indiv[basetidy_indiv$Flag_Readmit == "Yes",]), 
       #               "; \nN members that don't readmit: ", nrow(basetidy_indiv[basetidy_indiv$Flag_Readmit == "No",])),
       fill = "Gender") +
  ylab("Days to Readmission") +
  theme(axis.text.x = element_text(),
        axis.title.x = element_blank(),
        #legend.text = element_text(size = 8),
        #legend.title = element_text(size = 8),
        legend.position = "none"
  ) + #removes x axis labels
  guides(color = FALSE)  

figrace1 <- basetidy_indiv %>%
  ggplot(aes(x = RaceCat, y = Days_to_Readmit, color = RaceCat, fill = RaceCat)) +
  geom_boxplot(alpha = .2, notch = T) +
  labs(title = "Race", 
       #subtitle = paste0("Index Episodes between ", IndexTimeframe), 
       #caption = paste0("N members that readmit: ", nrow(basetidy_indiv[basetidy_indiv$Flag_Readmit == "Yes",]), 
       #               "; \nN members that don't readmit: ", nrow(basetidy_indiv[basetidy_indiv$Flag_Readmit == "No",])),
       fill = "Race") +
  ylab("Days to Readmission") +
  theme(axis.text.x = element_text(),
        axis.title.x = element_blank(),
        #legend.text = element_text(size = 8),
        #legend.title = element_text(size = 8),
        legend.position = "none",
        axis.title.y = element_blank()
  ) + #removes x axis labels
  guides(color = FALSE) 

figage1 <- basetidy_indiv %>%
  ggplot(aes(x = AgeCat, y = Days_to_Readmit, color = AgeCat, fill = AgeCat)) +
  geom_boxplot(alpha = .2, notch = T) +
  labs(title = "Age at Admit", 
       #subtitle = paste0("Index Episodes between ", IndexTimeframe), 
       #caption = paste0("N members that readmit: ", nrow(basetidy_indiv[basetidy_indiv$Flag_Readmit == "Yes",]), 
       #               "; \nN members that don't readmit: ", nrow(basetidy_indiv[basetidy_indiv$Flag_Readmit == "No",])),
       fill = "Age at Admission") +
  ylab("Days to Readmission") +
  theme(axis.text.x = element_text(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        #legend.text = element_text(size = 8),
        #legend.title = element_text(size = 8),
        legend.position = "none",
        axis.title.y = element_blank()
  ) + #removes x axis labels
  guides(color = FALSE) 



figasd1 <- basetidy_indiv %>%
  ggplot(aes(x = ASD_Measure_Year, y = Days_to_Readmit, color = ASD_Measure_Year, fill = ASD_Measure_Year)) +
  geom_boxplot(alpha = .2, notch = T) +
  labs(title = "ASD Diagnosis", 
       #subtitle = paste0("Index Episodes between ", IndexTimeframe), 
       #caption = paste0("N members that readmit: ", nrow(basetidy_indiv[basetidy_indiv$Flag_Readmit == "Yes",]), 
       #               "; \nN members that don't readmit: ", nrow(basetidy_indiv[basetidy_indiv$Flag_Readmit == "No",])),
       fill = "Current ASD Dx") +
  ylab("Days to Readmission") +
  theme(axis.text.x = element_text(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        #legend.text = element_text(size = 8),
        #legend.title = element_text(size = 8),
        legend.position = "none"
  ) + 
  guides(color = FALSE) 

figdhs1 <- basetidy_indiv %>%
  ggplot(aes(x = Flag_Active_DHS_Status , y = Days_to_Readmit, color = Flag_Active_DHS_Status, fill = Flag_Active_DHS_Status)) +
  geom_boxplot(alpha = .2, notch = T) +
  labs(title = "DHS Status", 
       #subtitle = paste0("Index Episodes between ", IndexTimeframe), 
       #caption = paste0("N members that readmit: ", nrow(basetidy_indiv[basetidy_indiv$Flag_Readmit == "Yes",]), 
       #               "; \nN members that don't readmit: ", nrow(basetidy_indiv[basetidy_indiv$Flag_Readmit == "No",])),
       fill = "Current DHS Status") +
  ylab("Days to Readmission") +
  theme(axis.text.x = element_text(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        #legend.text = element_text(size = 8),
        #legend.title = element_text(size = 8),
        legend.position = "none",
        axis.title.y = element_blank()
  ) + 
  guides(color = FALSE) 

figsubreadm <- ggarrange(figgen1, figage1, figrace1, figasd1, figdhs1,
                         nrow = 2, ncol = 3
)

annotate_figure(figsubreadm,
                top = "Figure 4. Days to Readmission for Select Subpopulations")

# Service Utilization Boxplots: After####

#service use in the time after discharge by percent
basetidy %>%
  filter(BA_Category %in% c("After") & Measure_Name %in% c("After Service")) %>%
  select(Client_Master_BK, LOC, Day_Count, SurvivalDays) %>%
  group_by(Client_Master_BK, LOC, SurvivalDays) %>%
  summarize(Days = sum(Day_Count)) %>%#need this step bc some folks have 2 lines of data for same LOC. Not sure why stored proc code would do that.
  mutate(RateDays30 = Days/SurvivalDays * 30) %>% 
  group_by (LOC) %>%
  summarize(N_UsingService = n(),
            Perc_UsingService = n()/length(unique(basetidy$Client_Master_BK)) * 100, 
            AvgRateDays30 = mean(RateDays30, na.rm = TRUE)) %>%
  ggplot(aes(x = reorder(LOC, -Perc_UsingService), y = Perc_UsingService)) +
  geom_bar(stat = "identity", fill = "blue", alpha = .5) +
  geom_text(aes(label = round(Perc_UsingService,1)), vjust = 1) +
  labs(title = "Percent of Members Using Services After Discharge", 
       subtitle = paste0("Index Episodes between ", IndexTimeframe)) +
  xlab("Level of Care") +
  ylab("Percent of Members Using Service")

#service use in the time after discharge by average days per month used
basetidy %>%
  filter(BA_Category %in% c("After") & Measure_Name %in% c("After Service")) %>%
  group_by(Client_Master_BK, BA_Category, Measure_Name, Measure_Period, LOC, SurvivalDays) %>%
  select(Client_Master_BK, BA_Category, Measure_Name, Measure_Period, LOC, Day_Count, SurvivalDays) %>%
  summarize(Days = sum(Day_Count)) %>%#need this step bc some folks have 2 lines of data for same LOC. Not sure why stored proc code would do that.
  mutate(RateDays30 = Days/SurvivalDays * 30) %>% 
  filter(LOC != "NA") %>%
  group_by (BA_Category, Measure_Name, Measure_Period, LOC) %>%
  summarize(N_UsingService = n(),
            Perc_UsingService = n()/nrow(basetidy_indiv) * 100, 
            AvgRateDays30 = mean(RateDays30, na.rm = TRUE)) %>%
  ggplot(aes(x = reorder(LOC, -AvgRateDays30), y = AvgRateDays30)) +
  geom_bar(stat = "identity", fill = "purple", alpha = .5) +
  geom_text(aes(label = round(AvgRateDays30,1)), vjust = 1) +
  labs(title = "Average N of Days Used By Members For Every Month in the Community", subtitle = paste0("Index Episodes between ", IndexTimeframe)) +
  xlab("Level of Care") +
  ylab("Average Days of Service per 30 Days in the Community")

### distribution of Service use in time after DC by year


#base figure  
RateDaysLOC <- basetidy %>%
  filter(BA_Category %in% c("After") & Measure_Name %in% c("After Service")) %>%
  group_by(Client_Master_BK, BA_Category, Measure_Name, Measure_Period, LOC, SurvivalDays) %>%
  select(Client_Master_BK, BA_Category, Measure_Name, Measure_Period, LOC, 
         Day_Count, SurvivalDays, Flag_Readmit, Flag_Active_DHS_Status, ServiceEnd) %>%
  mutate(yearDC = year(ymd(ServiceEnd))) %>%
  group_by(Client_Master_BK, LOC, SurvivalDays, yearDC, Flag_Readmit, Flag_Active_DHS_Status) %>%
  summarize(Days = sum(Day_Count)) %>%  #need this step bc some folks have 2 lines of data for same LOC. Not sure why stored proc code would do that.
  mutate(RateDays30 = Days/SurvivalDays * 30) %>%
  filter(is.na(LOC) == FALSE) %>%
  ggplot(aes(x = reorder(LOC, -RateDays30), y = RateDays30, color = LOC, fill = LOC)) +
  geom_boxplot(alpha = .2) +
  labs(title = paste0("Distribution of Days of Service After Discharge from, " IndexLOC)) +
  xlab("Level of Care") +
  ylab("Days of utilization per 30 days in the community") +
  theme(legend.position = "none") 

RateDaysLOC

# base figure by year
RateDaysLOC + 
  facet_wrap(~yearDC) + 
  labs(title = paste0("Distribution of Days of Service After Discharge from ", IndexLOC),
       subtitle = "Broken Down by Year of Discharge") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#base figure by readmission status
RateDaysLOC + 
  facet_wrap(~Flag_Readmit) + 
  labs(subtitle = "Broken Down by Readmission Status") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) #doublechecked it with this data and elsewhere, Everyone with rehab days did not readmit (n = 38)

RateDaysLOC + 
  facet_wrap(~ Flag_Active_DHS_Status) + 
  labs(title = "By DHS Status") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Service Utilization Boxplots: 1 Yr Prior####

#level check
levels(basetidy$Measure_Name)
levels(basetidy$Measure_Period)







#service use in the year leading up to admission, by percent
basetidy %>%
  filter(BA_Category %in% c("Before") & Measure_Name %in% c("Prior Service") & Measure_Period %in% c("Year 1")) %>%
  select(Client_Master_BK, LOC, Day_Count, SurvivalDays) %>%
  group_by(Client_Master_BK, LOC, SurvivalDays) %>%
  summarize(Days = sum(Day_Count)) %>%#need this step bc some folks have 2 lines of data for same LOC. Not sure why stored proc code would do that.
  mutate(RateDays30 = Days/365 * 30) %>% 
  group_by (LOC) %>%
  summarize(N_UsingService = n(),
            Perc_UsingService = n()/length(unique(basetidy$Client_Master_BK)) * 100, 
            AvgRateDays30 = mean(RateDays30, na.rm = TRUE)) %>%
  ggplot(aes(x = reorder(LOC, -Perc_UsingService), y = Perc_UsingService)) +
  geom_bar(stat = "identity", fill = "blue", alpha = .5) +
  geom_text(aes(label = paste0(round(Perc_UsingService,1),'%')),vjust = 1) +
  labs(title = "Percent of Members Using Services In The Year Prior to Admission", 
       subtitle = paste0("Index Episodes between ", IndexTimeframe)) +
  xlab("Level of Care") +
  ylab("Percent of Members Using Service")

#service use in the year leading up to admission
basetidy %>%
  filter(BA_Category %in% c("Before") & Measure_Name %in% c("Prior Service") & Measure_Period %in% c("Year 1")) %>%
  group_by(Client_Master_BK, LOC, SurvivalDays) %>%
  select(Client_Master_BK, LOC, Day_Count, SurvivalDays) %>%
  summarize(Days = sum(Day_Count)) %>%#need this step bc some folks have 2 lines of data for same LOC. Not sure why stored proc code would do that.
  mutate(RateDays30 = Days/365 * 30) %>% 
  group_by (LOC) %>%
  summarize(N_UsingService = n(),
            Perc_UsingService = n()/nrow(basetidy_indiv) * 100, 
            AvgDays = mean(Days, na.rm = TRUE),
            AvgRateDays30 = mean(RateDays30, na.rm = TRUE)) %>%
  ggplot(aes(x = reorder(LOC, -AvgDays), y = AvgDays)) +
  geom_bar(stat = "identity", fill = "purple", alpha = .5) +
  geom_text(aes(label = paste0(round(AvgDays,1), '\nN = ', N_UsingService)),vjust = 1) +
  labs(title = paste0("Average N of Days Used By Members In The Year Before Index ", IndexLOC), 
       subtitle = paste0("Index Episodes between ", IndexTimeframe)) +
  xlab("Level of Care") +
  ylab("Average Days of Service in Prior Year")


RatePriorDays <- basetidy %>%
  filter(BA_Category %in% c("Before") & Measure_Name %in% c("Prior Service") & Measure_Period %in% c("Year 1")) %>%
  group_by(Client_Master_BK, BA_Category, Measure_Name, Measure_Period, LOC, SurvivalDays) %>%
  select(Client_Master_BK, BA_Category, Measure_Name, Measure_Period, LOC, 
         Day_Count, SurvivalDays, Flag_Readmit, ServiceEnd, Flag_Active_DHS_Status) %>%
  mutate(yearDC = year(ymd(ServiceEnd))) %>%
  group_by(Client_Master_BK, LOC, SurvivalDays, yearDC, Flag_Readmit, Flag_Active_DHS_Status) %>%
  summarize(Days = sum(Day_Count)) %>%  #need this step bc some folks have 2 lines of data for same LOC. Not sure why stored proc code would do that.
  # mutate(RateDays30 = Days/365 * 30) %>%
  filter(is.na(LOC) == FALSE) %>%
  ggplot(aes(x = reorder(LOC, -Days), y = Days, color = LOC, fill = LOC)) +
  geom_boxplot(alpha = .2) +
  geom_jitter(width = .3, alpha = .1) + #can remove this, but gives a sense of volume of members represented in each boxplot
  labs(title = paste0("Distribution of Days of Service Utilization in the Year Prior to ", IndexLOC)) +
  xlab("Level of Care") +
  ylab("Days of utilization") +
  theme(legend.position = "none") 

RatePriorDays


ggplot_build(RatePriorDays) #for boxplot summary stats


# base figure by year
RatePriorDays + 
  facet_wrap(~yearDC) + 
  labs(subtitle = "Broken Down by Year of Discharge") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#base figure by readmission status
RatePriorDays + 
  facet_wrap(~Flag_Readmit) + 
  labs(subtitle = "Broken Down by Readmission Status") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) #doublechecked it with this data and elsewhere, Everyone with rehab days did not readmit (n = 38)

#Do DHS involved kids use dif services in year prior to AIP
RatePriorDays + 
  facet_wrap(~ Flag_Active_DHS_Status) + 
  labs(subtitle = paste0("Broken Down by DHS Status During ", IndexLOC)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) #doublechecked it with this data and elsewhere, Everyone with rehab days did not readmit (n = 38)



#DHS involved, prior 3 years####


#service use in the year leading up to admission, by percent
basetidy %>%
  filter(BA_Category %in% c("Before") & Measure_Name %in% c("DHS")) %>%
  select(Client_Master_BK, Measure_Period, Day_Count, SurvivalDays) %>%
  group_by(Client_Master_BK, Measure_Period, SurvivalDays) %>%
  summarize(Days = sum(Day_Count)) %>%#need this step bc some folks have 2 lines of data for same LOC. Not sure why stored proc code would do that.
  mutate(RateDays30 = Days/365 * 30) %>% 
  group_by (Measure_Period) %>%
  summarize(N_UsingService = n(),
            Perc_UsingService = n()/length(unique(basetidy$Client_Master_BK)) * 100, 
            AvgRateDays30 = mean(RateDays30, na.rm = TRUE)) %>%
  ggplot(aes(x = Measure_Period, y = Perc_UsingService)) +
  geom_bar(stat = "identity", fill = "blue", alpha = .5) +
  geom_text(aes(label = round(Perc_UsingService, 1)), vjust = 1) +
  labs(title = paste0("Percent of Members with DHS involvement in the Years Prior to ", IndexLOC), 
       subtitle = paste0("Index Episodes between ", IndexTimeframe)) +
  xlab("Years Prior to Index Admission") +
  ylab("Percent of Members Using Service")

#service use in the year leading up to admission
basetidy %>%
  filter(BA_Category %in% c("Before") & Measure_Name %in% c("DHS")) %>%
  select(Client_Master_BK, Measure_Period, Day_Count, SurvivalDays) %>%
  group_by(Client_Master_BK, Measure_Period, SurvivalDays) %>%
  summarize(Days = sum(Day_Count)) %>%#need this step bc some folks have 2 lines of data for same LOC. Not sure why stored proc code would do that.
  mutate(RateDays30 = Days/365 * 30) %>% 
  group_by (Measure_Period) %>%
  summarize(N_UsingService = n(),
            Perc_UsingService = n()/nrow(basetidy_indiv) * 100,
            AvgDays = mean(Days, na.rm = TRUE),
            AvgRateDays30 = mean(RateDays30, na.rm = TRUE)) %>%
  ggplot(aes(x = Measure_Period, y = AvgDays)) +
  geom_bar(stat = "identity", fill = "purple", alpha = .5) +
  labs(title = paste0("Average N of Days of DHS involvement in the Years Before ", IndexLOC)), 
       subtitle = paste0("Index Episodes between ", IndexTimeframe)) +
  xlab("Years Prior to Index Admission") +
  ylab("Average Days of Service")




#distribution of days in DHS
RatePriorDaysDHS <- basetidy %>%
  filter(BA_Category %in% c("Before") & Measure_Name %in% c("DHS")) %>%
  group_by(Client_Master_BK, BA_Category, Measure_Name, Measure_Period, SurvivalDays) %>%
  select(Client_Master_BK, BA_Category, Measure_Name, Measure_Period,  
         Day_Count, SurvivalDays, Flag_Readmit, ServiceEnd, Flag_Active_DHS_Status) %>%
  mutate(yearDC = year(ymd(ServiceEnd))) %>%
  group_by(Client_Master_BK, Measure_Period, SurvivalDays, yearDC, Flag_Readmit, Flag_Active_DHS_Status) %>%
  summarize(Days = sum(Day_Count)) %>%  #need this step bc some folks have 2 lines of data for same LOC. Not sure why stored proc code would do that.
  #mutate(RateDays30 = Days/365 * 30) %>%
  ggplot(aes(x = Measure_Period, y = Days, color = Measure_Period)) +
  geom_boxplot(alpha = .2) +
  geom_jitter(width = .3, alpha = .2) +
  labs(title = paste0("Distribution of Days in DHS in the Year Prior to ", IndexLOC)) +
  xlab("Years Prior to Index Admission") +
  ylab("Days of utilization") +
  theme(legend.position = "none") 


# base figure by year
RatePriorDaysDHS + 
  facet_wrap(~yearDC) + 
  labs(subtitle = "Broken Down by Year of Discharge") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#base figure by readmission status
RatePriorDaysDHS + 
  facet_wrap(~Flag_Readmit) + 
  labs(subtitle = "Broken Down by Readmission Status") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) #doublechecked it with this data and elsewhere, Everyone with rehab days did not readmit (n = 38)




#Days Eligible yrs prior####



#distribution of days in DHS
RatePriorDaysElig <- basetidy %>%
  filter(BA_Category %in% c("Before") & Measure_Name %in% c("Eligibility")) %>%
  group_by(Client_Master_BK, BA_Category, Measure_Name, Measure_Period, SurvivalDays) %>%
  select(Client_Master_BK, BA_Category, Measure_Name, Measure_Period,  
         Day_Count, SurvivalDays, Flag_Readmit, ServiceEnd, Flag_Active_DHS_Status) %>%
  mutate(yearDC = year(ymd(ServiceEnd))) %>%
  group_by(Client_Master_BK, Measure_Period, SurvivalDays, yearDC, Flag_Readmit, Flag_Active_DHS_Status) %>%
  summarize(Days = sum(Day_Count)) %>%  #need this step bc some folks have 2 lines of data for same LOC. Not sure why stored proc code would do that.
  #mutate(RateDays30 = Days/365 * 30) %>%
  ggplot(aes(x = Measure_Period, y = Days, color = Measure_Period)) +
  geom_boxplot() +
  labs(title = paste0("Distribution of Days in DHS in the Year Prior to ", IndexLOC)) +
  xlab("Years Prior to Index Admission") +
  ylab("Days of utilization per 30 day period") +
  theme(legend.position = "none") 


# base figure by year
RatePriorDaysElig + 
  facet_wrap(~yearDC) + 
  labs(subtitle = "Broken Down by Year of Discharge") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#base figure by readmission status
RatePriorDaysElig + 
  facet_wrap(~Flag_Readmit) + 
  labs(subtitle = "Broken Down by Readmission Status") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) #doublechecked it with this data and elsewhere, Everyone with rehab days did not readmit (n = 38)

#Percent with >= 300 days of eligibility
basetidy %>%
  filter(BA_Category %in% c("Before") & Measure_Name %in% c("Eligibility")) %>%
  select(Client_Master_BK, Measure_Period, Day_Count, SurvivalDays) %>%
  group_by(Client_Master_BK, Measure_Period, SurvivalDays) %>%
  summarize(Days = sum(Day_Count)) %>%#need this step bc some folks have 2 lines of data for same LOC. Not sure why stored proc code would do that.
  filter(Days >= 300) %>%
  mutate(RateDays30 = Days/365 * 30) %>% 
  group_by (Measure_Period) %>%
  summarize(N_UsingService = n(),
            Perc_UsingService = n()/length(unique(basetidy$Client_Master_BK)) * 100, 
            AvgRateDays30 = mean(RateDays30, na.rm = TRUE)) %>%
  ggplot(aes(x = Measure_Period, y = Perc_UsingService)) +
  geom_bar(stat = "identity", fill = "blue", alpha = .5) +
  geom_text(aes(label = round(Perc_UsingService,1)), vjust = 1) +
  labs(title = "Percent of Members with 300+ Days Eligibility in Prior Years", 
       subtitle = paste0("Index Episodes between ", IndexTimeframe)) +
  xlab("Eligibility") +
  ylab("Percent of Members with 300+ Days")


#Inpatient yrs prior####


levels(basetidy$Measure_Period)

# % using AIP
basetidy %>%
  filter(BA_Category %in% c("Before") & Measure_Name %in% c("IP Discharges") & is.na(Measure_Period) == FALSE) %>%
  group_by(Client_Master_BK, BA_Category, Measure_Name, Measure_Period, SurvivalDays) %>%
  select(Client_Master_BK, BA_Category, Measure_Name, Measure_Period,  
         Day_Count, SurvivalDays, Flag_Readmit, ServiceEnd, Flag_Active_DHS_Status) %>%
  mutate(yearDC = year(ymd(ServiceEnd))) %>%
  group_by(Client_Master_BK, Measure_Period, SurvivalDays, yearDC, Flag_Readmit, Flag_Active_DHS_Status) %>%
  summarize(NAIPDc = sum(Day_Count)) %>%  #need this step bc some folks have 2 lines of data for same LOC. Not sure why stored proc code would do that.
  group_by(Measure_Period) %>%
  summarize(N_UsingService = n(),
            Perc_UsingService = n()/length(unique(basetidy$Client_Master_BK)) * 100, 
            AvgNDCs = mean(NAIPDc, na.rm = TRUE)) %>%
  ggplot(aes(x = Measure_Period, y = Perc_UsingService)) +
  geom_bar(stat = "identity", fill = "blue", alpha = .5) +
  geom_text(aes(label = paste0(round(Perc_UsingService,1),'%')), vjust = 1) +
  labs(title = "Percent of Members with AIP Discharges Before Index Episode") +
  xlab("Years Prior to Index Admission") +
  ylab("Percent of Members with AIP Discharges") +
  theme(legend.position = "none") 

#Avg N DCs per member
basetidy %>%
  filter(BA_Category %in% c("Before") & Measure_Name %in% c("IP Discharges") & is.na(Measure_Period) == FALSE) %>%
  group_by(Client_Master_BK, BA_Category, Measure_Name, Measure_Period, SurvivalDays) %>%
  select(Client_Master_BK, BA_Category, Measure_Name, Measure_Period,  
         Day_Count, SurvivalDays, Flag_Readmit, ServiceEnd, Flag_Active_DHS_Status) %>%
  mutate(yearDC = year(ymd(ServiceEnd))) %>%
  group_by(Client_Master_BK, Measure_Period, SurvivalDays, yearDC, Flag_Readmit, Flag_Active_DHS_Status) %>%
  summarize(NAIPDc = sum(Day_Count)) %>%  #need this step bc some folks have 2 lines of data for same LOC. Not sure why stored proc code would do that.
  group_by(Measure_Period) %>%
  summarize(N_UsingService = n(),
            Perc_UsingService = n()/length(unique(basetidy$Client_Master_BK)) * 100, 
            AvgNDCs = mean(NAIPDc, na.rm = TRUE)) %>%
  ggplot(aes(x = Measure_Period, y = AvgNDCs)) +
  geom_bar(stat = "identity", fill = "blue", alpha = .5) +
  geom_text(aes(label = paste0("N = ", N_UsingService)), vjust = 1) +
  labs(title = "Average Number of AIP Discharges Per Member in the Years Before Index Episode") +
  xlab("Years Prior to Index Admission") +
  ylab("Avg N AIP Discharges \namong those with at least 1 DC") +
  theme(legend.position = "none") 

#boxplots on N DCs per member, most folks only have 1-2, max yr 1 & 2 = 7, max yr 3 = 6

basetidy %>%
  filter(BA_Category %in% c("Before") & Measure_Name %in% c("IP Discharges")  & is.na(Measure_Period) == FALSE) %>%
  group_by(Client_Master_BK, BA_Category, Measure_Name, Measure_Period, SurvivalDays) %>%
  select(Client_Master_BK, BA_Category, Measure_Name, Measure_Period,  
         Day_Count, SurvivalDays, Flag_Readmit, ServiceEnd, Flag_Active_DHS_Status) %>%
  mutate(yearDC = year(ymd(ServiceEnd))) %>%
  group_by(Client_Master_BK, Measure_Period, SurvivalDays, yearDC, Flag_Readmit, Flag_Active_DHS_Status) %>%
  summarize(NAIPDc = sum(Day_Count)) %>%  #need this step bc some folks have 2 lines of data for same LOC. Not sure why stored proc code would do that.
  ggplot(aes(x = Measure_Period, y = NAIPDc)) +
  geom_boxplot(fill = "blue", alpha = .5) +
  labs(title = "Average Number of AIP Discharges Per Member in the Years Before Index Episode") +
  xlab("Years Prior to Index Admission") +
  ylab("Avg N AIP Discharges \namong those with at least 1 DC") +
  theme(legend.position = "none") 


#Quick look at additional services####


#addl service use in the year leading up to admission
basetidy %>%
  filter(BA_Category %in% c("Before") & Measure_Name %in% c("Additional Service") & Measure_Period %in% c("Year 1")) %>%
  select(Client_Master_BK, LOC, Day_Count, SurvivalDays) %>%
  group_by(Client_Master_BK, LOC, SurvivalDays) %>%
  summarize(Days = sum(Day_Count)) %>%#need this step bc some folks have 2 lines of data for same LOC. Not sure why stored proc code would do that.
  mutate(RateDays30 = Days/365 * 30) %>% 
  group_by (LOC) %>%
  summarize(N_UsingService = n(),
            Perc_UsingService = n()/length(unique(basetidy$Client_Master_BK)) * 100, 
            AvgDays = mean(Days, na.rm = TRUE)) %>%
  kable() %>%
  kable_styling(full_width = FALSE)
