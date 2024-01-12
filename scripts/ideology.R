# Set the working directory
setwd(githubdir)
setwd("extreme_recall2/")

# Load libraries 
library(tidyverse)
library(stringr)

# Load data 
## DIME Data 
## data is reduced to governors, presidents, and senate members
df_dime <- read.csv("data/cf_scores/dime_recipients_1979_2020.csv.gz") |> 
   drop_na(cand.gender) |> 
   filter(cand.gender %in% c("M", "F")) |>  
   filter(seat %in% c("state:governor",  "federal:president", "federal:senate", "federal:house"))
  |> 
  # mutate(party = ifelse(party.orig == "100", "Democrat",
  #                       ifelse(party.orig == "200", "Republican", NA))) |> 
  # dplyr::select(bonica.rid, election, cycle, fecyear, name, lname, fname, 
  #               cand.gender:irt.cfscore, party, seat) |> 
  mutate(name_f = as.factor(name)) |> 
  mutate(party = ifelse(party.orig == "100", "Democrat",
                        ifelse(party.orig == "200", "Republican", NA))) |> 
  dplyr::select(bonica.rid, election, cycle, fecyear, name, lname, fname, 
                cand.gender:irt.cfscore, party, seat)

|> 
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
                       ifelse(bonica.rid == "cand41775" & seat == "federal:house", "drop", "keep"))) |> 
  filter(drop == "keep") |> 
  dplyr::select(-drop)
  
## Generate subset data sets that only look at individual parties
reps <- 
  df_dime  |> 
  #filter(cycle > 2000) |> 
  filter(!is.na(party)) |> 
  group_by(bonica.rid) |> 
  #filter(cycle == max(cycle)) |> 
  #filter(fecyear == max(fecyear)) |> 
  drop_na(recipient.cfscore) |> 
  filter(party == "Republican") |> 
  #filter(str_detect(seat, "federal")) |> 
  dplyr::select(bonica.rid, election, cycle, fecyear, party, name, cand.gender, recipient.cfscore.dyn, seat)


dems <- 
  df_dime  |> 
  #filter(cycle > 2000) |> 
  filter(!is.na(party)) |> 
  group_by(bonica.rid) |> 
  #filter(cycle == max(cycle)) |> 
  #filter(fecyear == max(fecyear)) |> 
  drop_na(recipient.cfscore) |> 
  filter(party == "Democrat") |> 
  #filter(str_detect(seat, "federal")) |> 
  dplyr::select(bonica.rid, election, cycle, fecyear, party, name, cand.gender, recipient.cfscore.dyn, seat)



# Plots
## Plotting the Democrats
df_dime  |> 
  #filter(cycle > 2010) |> 
  filter(!is.na(party)) |> 
  group_by(bonica.rid) |> 
  #filter(cycle == max(cycle)) |> 
  #filter(fecyear == max(fecyear)) |> 
  drop_na(recipient.cfscore) |> 
  filter(party == "Democrat") |> 
  #filter(str_detect(seat, "federal")) |> 
  #filter(party == "Republican") |> 
  ggplot(aes(x=reorder(name_f, recipient.cfscore), y=recipient.cfscore.dyn, label=recipient.cfscore)) + 
  geom_point(stat='identity', aes(col=party, shape = seat), size=6)  +
  scale_color_manual(name="Party", 
                     labels = c("Democrat", "Republican"), 
                     values = c("Democrat"="blue", "Republican"="#f8766d")) + 
  geom_text(color="white", size=2) +
  labs(title="CF Scores for politicians",
       x = "Politicians",
       y = "CF-Score") + 
  ylim(-2.5, 2.5) +
  coord_flip() + theme_bw() + theme(legend.position = "bottom")


## Plotting the Republicans
df_dime  |> 
  #filter(cycle > 2010) |> 
  filter(!is.na(party)) |> 
  group_by(bonica.rid) |> 
  #filter(cycle == max(cycle)) |> 
  #filter(fecyear == max(fecyear)) |> 
  drop_na(recipient.cfscore.dyn) |> 
  #filter(str_detect(seat, "federal")) |> 
  #filter(party == "Democrat") |> 
  filter(party == "Republican") |> 
  ggplot(aes(x=reorder(name_f, recipient.cfscore), y=recipient.cfscore.dyn, label=recipient.cfscore)) + 
  geom_point(stat='identity', aes(col=party, shape = seat), size=6)  +
  scale_color_manual(name="Party", 
                     labels = c("Democrat", "Republican"), 
                     values = c("Democrat"="blue", "Republican"="#f8766d")) + 
  geom_text(color="white", size=2) +
  labs(title="CF Scores for politicians",
       x = "Politicians",
       y = "CF-Score") + 
  ylim(-2.5, 2.5) +
  coord_flip() + theme_bw() + theme(legend.position = "bottom")

## Visualizing politicians on our list
## Loading the list and merging in the DIME data 
df_pols <- read_csv("data/politicians/politicians.csv") |> 
  dplyr::select(-c(lname, mname, fname, party)) |> 
  left_join(df_dime) 

## Reducing the CF Score data set
df_pols_cf <-
  df_pols |> 
  group_by(bonica.rid) |> 
  filter(cycle == max(cycle)) |> 
  filter(fecyear == max(fecyear)) |> 
  drop_na(recipient.cfscore.dyn) 

## This will make sure that sorting works
df_pols_cf <- df_pols_cf[order(df_pols_cf$recipient.cfscore), ]  # sort
df_pols_cf$name_f <- factor(df_pols_cf$name, levels = df_pols_cf$name)  # convert to factor to retain sorted order in plot.

## Reducing the DW Nominee data 
df_pols_dw1 <-
  df_pols |> 
  group_by(bonica.rid) |> 
  filter(cycle == max(cycle)) |> 
  filter(fecyear == max(fecyear)) |> 
  drop_na(dwnom1) 

## This will make sure that sorting works
df_pols_dw1 <- df_pols_dw1[order(df_pols_dw1$dwnom1), ]  # sort
df_pols_dw1$name_f <- factor(df_pols_dw1$name, levels = df_pols_dw1$name)  # convert to factor to retain sorted order in plot.



# Plotting the CF score data
df_pols_cf  |> 
  ggplot(aes(x=name_f, y=recipient.cfscore, label=recipient.cfscore)) + 
  geom_point(stat='identity', aes(col=party), size=6)  +
  scale_color_manual(name="Party", 
                     labels = c("Democrat", "Republican"), 
                     values = c("Democrat"="blue", "Republican"="#f8766d")) + 
  geom_text(color="white", size=2) +
  labs(title="CF Scores for politicians",
       x = "Politicians",
       y = "CF-Score") + 
  ylim(-2.5, 2.5) +
  coord_flip() + theme_bw() + theme(legend.position = "bottom")

# Plotting the DWN data
df_pols_dw1  |> 
  ggplot(aes(x=name_f, y=dwnom1, label=dwnom1)) + 
  geom_point(stat='identity', aes(col=party), size=6)  +
  scale_color_manual(name="Party", 
                     labels = c("Democrat", "Republican"), 
                     values = c("Democrat"="blue", "Republican"="#f8766d")) + 
  geom_text(color="white", size=2) +
  labs(title="DW Nominate Scores for politicians",
       x = "Politicians",
       y = "DW-Nominate") + 
  coord_flip() + theme_bw()  + theme(legend.position = "bottom")



