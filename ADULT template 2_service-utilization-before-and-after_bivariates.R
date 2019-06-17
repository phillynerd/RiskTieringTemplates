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
       subtitle = paste0("First ", IndexLOC, " Discharge between ", IndexTimeframe),
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
         Demos = AgeCat) %>%
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
  bind_rows (Age, Race, MHSA, ASDID) %>%
  select(Demos, n, percent) %>%
  mutate(percent = round(percent, 1)) %>%
  kable(caption = paste0("Member-Level Demographics during Index  ", IndexLOC, " Episode")) %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE) %>%
  group_rows("Gender", 1,2) %>%
  group_rows("Age at Admission", 3,7) %>%
  group_rows("Race", 8,11) %>%
  group_rows("Does member have SA hx", 12,13) %>%
  group_rows("Does member have ASD and/or ID diagnosis", 14, 15) 




#rbinding for service hx####
Firstbase %>%
  bind_rows(Readmit) %>%
  select(ServUtil, n, percent) %>%
  mutate(percent = round(percent, 1)) %>%
  kable(caption = paste0("Select Service Use Related to Index  ", IndexLOC, " Episode")) %>%
  kable_styling(bootstrap_options = "striped", full_width = TRUE) %>%
  group_rows(paste0("Is this the member's first ", IndexLOC, " episode"), 1, 2) %>%
  group_rows(paste0("Does member readmit to ", ReadmitLOC, " within ", IndexDaystoReadmit ," days"), 3, 4)


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
       subtitle = paste0("Index Episodes between ", IndexTimeframe, "; Zoomed In to LOS <= 30"),
       caption = paste0("N members: ", nrow(basetidy_indiv))) +
  ylab("LOS (Days)") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "none") +
  coord_cartesian(ylim=(c(0, 30)))

#los by readmission status
basetidy_indiv %>%
  ggplot(aes(y = LOS, color = Flag_Readmit, fill = Flag_Readmit)) +
  geom_boxplot(alpha = .2, notch = TRUE) +
  labs(title = paste0("Distribution of LOS in ", IndexLOC, " by Readmission Status"), 
       subtitle = paste0("Index Episodes between ", IndexTimeframe, "; Zoomed In to LOS <= 30"), 
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
  guides(color = FALSE) + #removes redundant legend for the color
  coord_cartesian(ylim=(c(0, 30))) #this will change depending on analysis/LOS distribution

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
  theme(axis.text.x = element_text(angle = 45, vjust = .5),
        axis.title.x = element_blank(),
        #legend.text = element_text(size = 8),
        #legend.title = element_text(size = 8),
        legend.position = "none"
  ) + #removes x axis labels
  guides(color = FALSE)  +
  coord_cartesian(ylim = c(0, 30))

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
  theme(axis.text.x = element_text(angle = 45, vjust = .5),
        axis.title.x = element_blank(),
        #legend.text = element_text(size = 8),
        #legend.title = element_text(size = 8),
        legend.position = "none"
  ) + #removes x axis labels
  guides(color = FALSE) +
  coord_cartesian(ylim = c(0, 30))

##LOS by age
figage <- basetidy_indiv %>%
  ggplot(aes(x = AgeCat, y = LOS, color = AgeCat, fill = AgeCat)) +
  geom_boxplot(alpha = .2, notch = T) +
  labs(title = "Age at Admit", 
       #subtitle = paste0("Index Episodes between ", IndexTimeframe), 
       #caption = paste0("N members that readmit: ", nrow(basetidy_indiv[basetidy_indiv$Flag_Readmit == "Yes",]), 
       #               "; \nN members that don't readmit: ", nrow(basetidy_indiv[basetidy_indiv$Flag_Readmit == "No",])),
       fill = "Age at Admission") +
  ylab("LOS (Days)") +
  xlab("Age Category at Admission") +
  theme(axis.text.x = element_text(angle = 45, vjust = .5),
        axis.title.x = element_blank(),
        #legend.text = element_text(size = 8),
        #legend.title = element_text(size = 8),
        legend.position = "none"
  ) + #removes x axis labels
  guides(color = FALSE) +
  coord_cartesian(ylim = c(0, 30))






figsubLOS <- ggarrange(figgen, figage, figrace,
                       nrow = 1, ncol = 3
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
  theme(axis.text.x = element_text(angle = 45, vjust = .5),
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
  theme(axis.text.x = element_text(angle = 45, vjust = .5),
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
  theme(axis.text.x = element_text(angle = 45, vjust = .5),
        axis.title.x = element_blank(),
        #legend.text = element_text(size = 8),
        #legend.title = element_text(size = 8),
        legend.position = "none",
        axis.title.y = element_blank()
  ) + #removes x axis labels
  guides(color = FALSE) 



figsubreadm <- ggarrange(figgen1, figage1, figrace1,
                         nrow = 1, ncol = 3
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
  ylab("Percent of Members Using Service") +
  theme(axis.text.x = element_text(angle = 45, vjust = .5))

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
  ylab("Average Days of Service per 30 Days in the Community") +
  theme(axis.text.x = element_text(angle = 45, vjust = .5))

### distribution of Service use in time after DC by year


#base figure  
RateDaysLOC <- basetidy %>%
  filter(BA_Category %in% c("After") & Measure_Name %in% c("After Service")) %>%
  group_by(Client_Master_BK, BA_Category, Measure_Name, Measure_Period, LOC, SurvivalDays) %>%
  select(Client_Master_BK, BA_Category, Measure_Name, Measure_Period, LOC, 
         Day_Count, SurvivalDays, Flag_Readmit, ServiceEnd) %>%
  mutate(yearDC = year(ymd(ServiceEnd))) %>%
  group_by(Client_Master_BK, LOC, SurvivalDays, yearDC, Flag_Readmit) %>%
  summarize(Days = sum(Day_Count)) %>%  #need this step bc some folks have 2 lines of data for same LOC. Not sure why stored proc code would do that.
  mutate(RateDays30 = Days/SurvivalDays * 30) %>%
  filter(is.na(LOC) == FALSE) %>%
  ggplot(aes(x = reorder(LOC, -RateDays30), y = RateDays30)) +
  geom_jitter(width = .3, alpha = .1, aes(fill = LOC, color = LOC)) +
  geom_boxplot(alpha = .2) +
  labs(title = paste0("Distribution of Days of Service After Discharge from ", IndexLOC)) +
  xlab("Level of Care") +
  ylab("Days of utilization per 30 days in the community") +
  theme(legend.position = "none", 
        axis.text.x = element_text(angle = 45, vjust = .5)) 

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
  ylab("Percent of Members Using Service") +
  theme(axis.text.x = element_text(vjust = .5, angle = 45))

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
  geom_text(aes(label = paste0(round(AvgDays,1), '\nN = ', N_UsingService)), size = 3) +
  labs(title = paste0("Average N of Days Used By Members In The Year Before Index ", IndexLOC), 
       subtitle = paste0("Index Episodes between ", IndexTimeframe)) +
  xlab("Level of Care") +
  ylab("Average Days of Service in Prior Year") +
  theme(axis.text.x = element_text(vjust = .5, angle = 45))


RatePriorDays <- basetidy %>%
  filter(BA_Category %in% c("Before") & Measure_Name %in% c("Prior Service") & Measure_Period %in% c("Year 1")) %>%
  group_by(Client_Master_BK, BA_Category, Measure_Name, Measure_Period, LOC, SurvivalDays) %>%
  select(Client_Master_BK, BA_Category, Measure_Name, Measure_Period, LOC, 
         Day_Count, SurvivalDays, Flag_Readmit, ServiceEnd) %>%
  mutate(yearDC = year(ymd(ServiceEnd))) %>%
  group_by(Client_Master_BK, LOC, SurvivalDays, yearDC, Flag_Readmit) %>%
  summarize(Days = sum(Day_Count)) %>%  #need this step bc some folks have 2 lines of data for same LOC. Not sure why stored proc code would do that.
  # mutate(RateDays30 = Days/365 * 30) %>%
  filter(is.na(LOC) == FALSE) %>%
  ggplot(aes(x = reorder(LOC, -Days), y = Days)) +
  geom_jitter(width = .3, alpha = .1, aes(color = LOC, fill = LOC)) + #can remove this, but gives a sense of volume of members represented in each boxplot
  geom_boxplot(alpha = .3) +
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
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 


#Days Eligible yrs prior for those with at least 1 day ELIG####

#distribution of eligible days
RatePriorDaysElig <- basetidy %>%
  filter(BA_Category %in% c("Before") & Measure_Name %in% c("Eligibility")) %>%
  group_by(Client_Master_BK, BA_Category, Measure_Name, Measure_Period, SurvivalDays) %>%
  select(Client_Master_BK, BA_Category, Measure_Name, Measure_Period,  
         Day_Count, SurvivalDays, Flag_Readmit, ServiceEnd) %>%
  mutate(yearDC = year(ymd(ServiceEnd))) %>%
  group_by(Client_Master_BK, Measure_Period, SurvivalDays, yearDC, Flag_Readmit) %>%
  summarize(Days = sum(Day_Count)) %>%  #need this step bc some folks have 2 lines of data for same LOC. Not sure why stored proc code would do that.
  #mutate(RateDays30 = Days/365 * 30) %>%
  ggplot(aes(x = Measure_Period, y = Days, color = Measure_Period)) +
  #geom_jitter(width = .3, alpha = .2, aes(color = Measure_Period)) +
  geom_boxplot(alpha = .2) +
  labs(title = paste0("Distribution of Eligibility in the Year Prior to ", IndexLOC)) +
  xlab("Years Prior to Index Admission") +
  ylab("Days of utilization per 30 day period") +
  theme(legend.position = "none") 

RatePriorDaysElig

# base figure by year
RatePriorDaysElig + 
  facet_wrap(~yearDC) + 
  labs(subtitle = "Broken Down by Year of Discharge") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#base figure by readmission status
RatePriorDaysElig + 
  facet_wrap(~Flag_Readmit) + 
  labs(subtitle = "Broken Down by Readmission Status") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

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

#Elig Cat for those w at least 1 day eligibility
basetidy %>%
  filter(BA_Category %in% c("Before") & Measure_Name %in% c("Eligibility")) %>% 
  select(Client_Master_BK, Measure_Period, Day_Count, SurvivalDays) %>% View()
  group_by(Client_Master_BK, Measure_Period, SurvivalDays) %>%
  summarize(Days = sum(Day_Count)) %>%
  mutate(EligCat = ifelse(is.na(Days), "No Elig",
                          ifelse(Days >= 300, "300+ days",
                          ifelse(Days >= 200, "200-299 Days",
                                 ifelse(Days >= 100, "100-199 Days",
                                        ifelse(Days >= 1, "1-99 Days", "ERROR"))))))  %>% 
  group_by(Measure_Period, EligCat) %>%
  summarize(n = n()) %>% 
  ggplot(aes(x = Measure_Period, y = n, fill = EligCat)) +
  geom_bar(stat = "identity", position = position_fill()) +
  labs(title = "Eligibility Categories In Years Prior",
       x = "Years Prior",
       y = "Percent")
  
  basedata %>%  
    filter(BA_Category %in% c("Before") & Measure_Name %in% c("Eligibility")) %>% 
     View()
  
#Elig Cat for ALL members in dataset. 
Elig1 <- baseindiv_spread %>% 
    mutate(Yr1Elig_Cat = ifelse(is.na(Yr1Prior_Elig), "No Elig",
                                ifelse(Yr1Prior_Elig <= 182, "LT 50%", "50% +"))) %>% 
    group_by(Yr1Elig_Cat) %>% 
    tally() %>% 
    ggplot(aes(x = 1, y = n, fill = Yr1Elig_Cat)) +
    geom_bar(stat = "identity", position = "fill") +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank()) +
    xlab ( "Year1")
  
Elig2 <- baseindiv_spread %>% 
  mutate(Yr2Elig_Cat = ifelse(is.na(Yr2Prior_Elig), "No Elig",
                       ifelse(Yr2Prior_Elig <= 182, "LT 50%", "50% +")))%>% 
  group_by(Yr2Elig_Cat) %>% 
    tally() %>% 
    ggplot(aes(x = 1, y = n, fill = Yr2Elig_Cat)) +
    geom_bar(stat = "identity", position = "fill") +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank()) +
    xlab ( "Year2")
  
Elig3 <- baseindiv_spread %>% 
    mutate(Yr3Elig_Cat = ifelse(is.na(Yr3Prior_Elig), "No Elig",
                                ifelse(Yr3Prior_Elig <= 182, "LT 50%", "50% +"))) %>% 

  group_by(Yr3Elig_Cat) %>% 
    tally() %>% 
    ggplot(aes(x = 1, y = n, fill = Yr3Elig_Cat)) +
    geom_bar(stat = "identity", position = "fill") +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank()) +
    xlab ( "Year3")


ggpubr::ggarrange(Elig1, Elig2, Elig3,
          nrow = 1, ncol = 3,
          common.legend = TRUE)

#Inpatient yrs prior####


levels(basetidy$Measure_Period)

# % using AIP
basetidy %>%
  filter(BA_Category %in% c("Before") & Measure_Name %in% c("IP Discharges") & is.na(Measure_Period) == FALSE) %>%
  group_by(Client_Master_BK, BA_Category, Measure_Name, Measure_Period, SurvivalDays) %>%
  select(Client_Master_BK, BA_Category, Measure_Name, Measure_Period,  
         Day_Count, SurvivalDays, Flag_Readmit, ServiceEnd) %>%
  mutate(yearDC = year(ymd(ServiceEnd))) %>%
  group_by(Client_Master_BK, Measure_Period, SurvivalDays, yearDC, Flag_Readmit) %>%
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
         Day_Count, SurvivalDays, Flag_Readmit, ServiceEnd) %>%
  mutate(yearDC = year(ymd(ServiceEnd))) %>%
  group_by(Client_Master_BK, Measure_Period, SurvivalDays, yearDC, Flag_Readmit) %>%
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

#boxplots on N DCs per member, most folks only have 1-2, max yr 1 = 13,  2 = 9, max yr 3 = 11

basetidy %>%
  filter(BA_Category %in% c("Before") & Measure_Name %in% c("IP Discharges")  & is.na(Measure_Period) == FALSE) %>%
  group_by(Client_Master_BK, BA_Category, Measure_Name, Measure_Period, SurvivalDays) %>%
  select(Client_Master_BK, BA_Category, Measure_Name, Measure_Period,  
         Day_Count, SurvivalDays, Flag_Readmit, ServiceEnd) %>%
  mutate(yearDC = year(ymd(ServiceEnd))) %>%
  group_by(Client_Master_BK, Measure_Period, SurvivalDays, yearDC, Flag_Readmit) %>%
  summarize(NAIPDc = sum(Day_Count)) %>%  #need this step bc some folks have 2 lines of data for same LOC. Not sure why stored proc code would do that.
  ggplot(aes(x = Measure_Period, y = NAIPDc)) +
  geom_boxplot(fill = "blue", alpha = .5) +
  labs(title = "Average Number of AIP Discharges Per Member in the Years Before Index Episode") +
  xlab("Years Prior to Index Admission") +
  ylab("Avg N AIP Discharges \namong those with at least 1 DC") +
  scale_y_continuous(breaks = seq(1,14,2)) +
  theme(legend.position = "none") 


#Quick look at additional services####


#addl service use in the years leading up to admission
basetidy %>%
  filter(BA_Category %in% c("Before") & Measure_Name %in% c("Additional Service")) %>%
  select(Client_Master_BK, LOC, Day_Count, SurvivalDays, Measure_Period) %>%
  group_by(Client_Master_BK, LOC, SurvivalDays, Measure_Period) %>%
  summarize(Days = sum(Day_Count)) %>%#need this step bc some folks have 2 lines of data for same LOC. Not sure why stored proc code would do that.
  mutate(RateDays30 = Days/365 * 30) %>% 
  group_by (Measure_Period, LOC) %>%
  summarize(N_UsingService = n(),
            Perc_UsingService = n()/length(unique(basetidy$Client_Master_BK)) * 100, 
            AvgDays = mean(Days, na.rm = TRUE)) %>%
  kable() %>%
  kable_styling(full_width = FALSE)
