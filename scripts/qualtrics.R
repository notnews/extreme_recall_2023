# Title:qualtrics.R
# Date: 2023-12-15
# Author: Daniel Weitzel
# Email: daniel.weitzel@colostate.edu
# Purpose: Collect data from Qualtrics and prepare it for analysis 

# Working directory
setwd(githubdir)

# Libraries
library(tidyverse)
library(qualtRics)
library(readxl)

## Fetch the Qualtrics keys
source("~/Dropbox/Research/R/scripts/qualtrics_csu.R")

## Load the politicians data 
df_politicians <- read_excel("extreme_recall2/data/politicians/politicians_attributes.xlsx") |> 
  drop_na(bonica.rid) |> 
  dplyr::select(bonica.rid, race_ethnicity, recency, most_recent_office)

## Load and process DIME data
df_dime_merge <-
  read_csv("extreme_recall2/data/cf_scores/dime_recipients_1979_2020.csv.gz") |> 
  mutate(party = ifelse(party.orig == "100", "Democrat",
                        ifelse(party.orig == "200", "Republican", NA))) |> 
  dplyr::select(bonica.rid, election, cycle, fecyear, name, lname, fname, 
                cand.gender:irt.cfscore, party, seat) |> 
  mutate(name_f = as.factor(name)) |> 
  filter(bonica.rid %in% c(
    # Republicans
    "cand625nominee", "cand3331", "cand3334nominee", "cand105110", "cand584", 
    "cand144919", "cand992", "cand110286", "cand1403", "cand418", "cand108459", 
    "cand837", "cand100270nominee", "cand1573",
    # Democrats
    "cand221nominee", "cand3333nominee", "cand1572nominee", "cand760", 
    "cand3335nominee", "cand158985", "cand159005", "cand1235", "cand140679",
    "cand1616", "cand744", "cand146299", "cand1600", "cand41775")) |> 
  group_by(bonica.rid) |> 
  filter(cycle == max(cycle)) |> 
  mutate(drop = ifelse(bonica.rid == "cand1235" & seat == "federal:senate", "drop",
                       ifelse(bonica.rid == "cand41775" & seat == "federal:house", "drop", "keep")),
         executive = str_detect(seat, "president|governor")) |> # this classifies the last race
  filter(drop == "keep") |> 
  dplyr::select(-drop) |>  
  left_join(df_politicians) |> 
  rename(party_name = party)

rm(df_politicians)

## Call Qualtrics data
df_er <- fetch_survey(surveyID = "SV_eRpRcmF8vPjlaWG", 
                      verbose = TRUE,
                      force_request = TRUE)

## Clean data 
## Process the survey data so it's ready for analysis (remove text, make vars numeric etc)
df_er_clean <-
  df_er |> 
  filter(Q1.1 != "I do not agree") |> 
  mutate(passed_ac = ifelse(Q4.1_1 == "Extremely interested" &
                              Q4.1_2 == "Very interested", TRUE, FALSE),
         partisan = ifelse(Q2.1 == "Republican", "Republican",
                           ifelse(Q2.1 == "Democrat", "Democrat",
                                  ifelse(str_detect(Q2.4, "Republican"), "Republican",
                                         ifelse(str_detect(Q2.4, "Democrat"), "Democrat", NA)))),
         pol_interest = as.numeric(str_remove_all(Q2.5_1,"Passionately interested |Not at all interested ")))

## Reduce to only the vars with data about the participants we need
df_er_participants <-
  df_er_clean |> 
  dplyr::select(-c(Q5.1_1:Q5.1_3, Q6.1_1:Q6.1_3, 
                   Q5.2_71:Q5.2_85, Q6.2_37:Q6.2_52, 
                   contains("_DO_")))

## Process the open text responses. 
df_er_open <- 
  df_er_clean |> 
  dplyr::select(ResponseId, Q5.1_1:Q5.1_3, Q6.1_1:Q6.1_3) |> 
  pivot_longer(cols = -ResponseId,
               names_to = "question_open",
               values_to = "politicians_open") |> 
  separate(question_open, into = c("question_stem", "number"), sep = "_", remove = FALSE) |> 
  mutate(party = ifelse(str_detect(question_open, "5"), "dem", "rep")) |> 
  group_by(ResponseId, question_stem) |> 
  mutate(number = as.numeric(number),
         politicians_open_count = sum(!is.na(politicians_open))) |> 
  ungroup() |> 
  #unite("party_open", c(party_open, number), sep = "_", remove = FALSE) |> 
  dplyr::select(-question_stem) |> 
  left_join(df_er_participants)

## Process the closed responses and merge the other data with that 
df_er_closed <-
  df_er_clean |> 
  dplyr::select(ResponseId, Q5.2_71:Q5.2_85, Q6.2_37:Q6.2_52) |> 
  pivot_longer(cols = -ResponseId,
               names_to = "question_closed",
               values_to = "politicians_closed") |> 
  mutate(bonica.rid = case_when(# Democrats
                                politicians_closed == "Joseph Biden" ~ "cand221nominee", 
                                politicians_closed == "Bill Clinton" ~ "cand3333nominee", 
                                politicians_closed == "Hillary Clinton" ~ "cand1572nominee", 
                                politicians_closed == "John F. Kennedy" ~ "", #empty 
                                politicians_closed == "Nancy Pelosi" ~ "cand760",
                                politicians_closed == "Barack Obama" ~ "cand3335nominee", 
                                politicians_closed == "Kamala Harris" ~ "cand158985", 
                                politicians_closed == "Elizabeth Warren" ~ "cand159005", 
                                politicians_closed == "Bernie Sanders" ~ "cand1235", 
                                politicians_closed == "Alexandria Ocasio-Cortez" ~ "cand140679",
                                politicians_closed == "Dianne Feinstein" ~ "cand1616", 
                                politicians_closed == "John Lewis" ~ "cand744",  
                                politicians_closed == "Ilhan Omar" ~ "cand146299",
                                politicians_closed == "Joe Manchin" ~ "cand1600", 
                                politicians_closed == "Tulsi Gabbard" ~ "cand41775",
                                # Republicans
                                politicians_closed == "John McCain" ~ "cand625nominee", # alternative: cand625
                                politicians_closed == "Ronald Reagan" ~ "cand3331", 
                                politicians_closed == "George W. Bush" ~ "cand3334nominee", 
                                politicians_closed == "Ted Cruz" ~ "cand105110", 
                                politicians_closed == "Mitch McConnell" ~ "cand584", 
                                politicians_closed == "Mitt Romney" ~ "cand144919", # alternative: cand100440
                                politicians_closed == "Michele Bachmann" ~ "cand992", 
                                politicians_closed == "Greg Abbott" ~ "cand110286", 
                                politicians_closed == "Lindsey Graham" ~ "cand1403", 
                                politicians_closed == "Newt Gingrich" ~ "cand418", 
                                politicians_closed == "Nikki Haley" ~ "cand108459", 
                                politicians_closed == "Mike Pence" ~ "cand837", 
                                politicians_closed == "Donald Trump" ~ "cand100270nominee", 
                                politicians_closed == "Richard Nixon" ~ "", # empty
                                politicians_closed == "Lisa Murkowski" ~ "cand1573")) |> 
  separate(question_closed, into = c("question_stem", "number"), sep = "_", remove = FALSE) |> 
  mutate(party = ifelse(str_detect(question_closed, "5"), "dem", "rep"),
         number = as.numeric(number),
         number = ifelse(party == "dem", number-70, number-36)) |> 
  group_by(ResponseId, question_stem) |> 
  mutate(politicians_closed_count = sum(!is.na(politicians_closed))) |> 
  ungroup() |> 
  #unite("party_closed", c(party_closed, number), sep = "_", remove = FALSE) |> 
  dplyr::select(-question_stem) |>
  #drop_na(politicians_closed) |> 
  left_join(df_dime_merge) |> 
  left_join(df_er_participants)

rm(df_dime_merge)

