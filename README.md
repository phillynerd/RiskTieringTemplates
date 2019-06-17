# Risk Tiering Templates

This set of documents provides a first pass at a streamlined way to analyse data from the uspTieringAnalysisTemplate. As usual, there are no data here, so the code is just for reference. 

For the stored procedure, I created the specifications that were used by data analysts to create the stored procedure.  The general idea for specifications are as follows.
1. Identify level(s) of care (LOC) that you're interested in (1 auth = 1 episode) as your index episode.
  a. The code will pull in demographics associated with that LOC (age, race/ethnicity, gender, MH/SA dx, is it first ever use of this service, LOS in service, plus some age specific things like ASD & DHS involvement)
2. Identify age (this will pick what LOC grouping codes are used for service utilization before and after index discharge)
3. Identify a readmission LOC/set of LOCs
4. Identify a max timeframe to readmission
  a. The code will pull whether or not a readmission occurred in that time frame
  b. The code will define N days to readmission (NA if no readmission)
  c. The code will pull in all service utilization (n days for pre-defined clinical groupings filtered by age grouping) from index DC until   either readmission or end of timeframe for readmission
  d. The code will pull service utilization by year for years 1, 2, and 3 prior to index admission.
  e. The code will pull in eligibility information for years 1, 2, and 3 prior to index admission.
  
- Script 1 cleans this data and gets it in the format I need for survival analysis, as well as a few basic tables for me to inspect.
- Script 2 provides a series of dataviz that are most typically of interest for the LOCs that are likely to go through this process. This script sometimes gets modified for different LOCs depending on the analysis and questions at hand, but the template is the starting point.
- Script 3 creates categories/binary variables and then creates the Cox model.  Script 3 also has the code to create a prettier dataviz than the ggforest option.
- .rmd file creates a short 2 pager based on the files created in script 1.  rmd typically needs some updating depending on mods made in script 1, 2, and 3 based on the LOC's I'm looking at, but the bulk of these updates are done in scripts 1, 2, and 3 and then just transferred here.  When using this file I also need to check the summary stats and provide things that are most relevant. 

This entire process streamlined the entire survival modeling pipeline from a few weeks to less than a day for a first pass at a new LOC grouping (ie a first report on detox), and a few minutes for subsequent updates to an existing LOC grouping (eg updates to detox reports)

## Goals for this process down the line
1. streamline the renaming process so it's less manual and not a string of horrendously nested manual renames
2. reduce the number of points i need to interact with this code each time
3. turn this into a package for the org?
-  
