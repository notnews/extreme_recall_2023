# Title:   qualtrics.R
# Date:    2023-12-15
# Author:  Daniel Weitzel
# Email:   daniel.weitzel@colostate.edu
# Purpose: Collect data from Qualtrics and prepare it for analysis 

# Working directory
setwd(githubdir)
setwd("extreme_recall2")

# Libraries
library(tidyverse)
library(qualtRics)     # Only required for call to Qualtrics API

## Fetch the Qualtrics keys, only required for call to API
source("~/Dropbox/Research/R/scripts/qualtrics_csu.R")


## Call Qualtrics data
df_er <- fetch_survey(surveyID = "SV_eRpRcmF8vPjlaWG", 
                      verbose = TRUE,
                      force_request = TRUE)

# Save Qualtrics data
write_csv(df_er, "extreme_recall2/data/lucid/er2_lucid_jan2023.csv")
