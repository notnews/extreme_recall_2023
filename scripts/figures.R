# Working directory
setwd(githubdir)
setwd("extreme_recall2")

# Libraries
library(tidyverse)
library(readxl)

# Source the data file
source("scripts/survey_df_clean.R")


## People named in the data set 
df_er_open |> 
  ungroup() |> 
  dplyr::select(politican_clean, party_name) |> 
  drop_na(politican_clean) |> 
  add_count(politican_clean) |> 
  ggplot(aes(x = reorder(politican_clean, n),fill = party_name)) +
  geom_bar() +
  coord_flip() +
  scale_fill_manual(name="Party", 
                    labels = c("Democrat", "Republican"), 
                    values = c("Democrat"="blue", "Republican"="#f8766d")) +
  labs(title = "Frequency of politicians named in open ended question",
       x = "Politician",
       y = "Count",
       caption = "Question: When you think about the Democratic/Republican Party, which political leader(s) first come to mind? Name up to three ...") +
  theme_bw()


## Frequency of recalled politicians 
df_er_open |> 
  dplyr::select(ResponseId, party_open_q, politican_clean, 
                recipient.cfscore.dyn, dwnom1, dwnom2, party_name, partisan) |> 
  #filter(politican_clean != "Spam") |> 
  mutate(politican_clean = ifelse(politican_clean == "Spam", NA, politican_clean)) |> 
  group_by(ResponseId, party_open_q) |> 
  mutate(num_politicians = length(na.omit(politican_clean)),
         all_missing = if_else(all(is.na(politican_clean)), TRUE, FALSE),
         num_politicians = ifelse(all_missing == TRUE, 0, num_politicians)) |> 
  ungroup() |> 
  dplyr::select(ResponseId, num_politicians, party_open_q) |> 
  distinct() |> 
  ggplot(aes(x = num_politicians)) +
  geom_bar()+ 
  facet_grid(~party_open_q) +
  theme_bw() +
  labs(title = "Number of politicians remembered",
       x = "Number of politicians",
       y = "Count")

## Frequency of recalled politicians in vs outpartisan
df_er_open |> 
  dplyr::select(ResponseId, party_open_q, politican_clean, 
                recipient.cfscore.dyn, dwnom1, dwnom2, party_name, partisan) |> 
  mutate(party_open_q = ifelse(str_detect(party_open_q,"Demo"), "Democrat",
                                          ifelse(str_detect(party_open_q, "Repub"), "Republican", NA)),
         partisan_type = ifelse(partisan == party_open_q, "Copartisan", "Outpartisan"),
         partisan_type = ifelse(is.na(partisan_type), "Independent/Other", partisan_type),
         politican_clean = ifelse(politican_clean == "Spam", NA, politican_clean)) |> 
  group_by(ResponseId, party_open_q) |> 
  mutate(num_politicians = length(na.omit(politican_clean)),
         all_missing = if_else(all(is.na(politican_clean)), TRUE, FALSE),
         num_politicians = ifelse(all_missing == TRUE, 0, num_politicians)) |> 
  ungroup() |> 
  dplyr::select(ResponseId, num_politicians, partisan_type) |> 
  distinct() |> 
  ggplot(aes(x = num_politicians)) +
  geom_bar()+ 
  facet_grid(~partisan_type) +
  theme_bw() +
  labs(title = "Number of politicians remembered by partisanship",
       x = "Number of politicians",
       y = "Count")

## Ideology of recalled politicians
df_er_open |> 
  dplyr::select(ResponseId, party_open_q, politican_clean, 
                recipient.cfscore.dyn, dwnom1, dwnom2, party_name, partisan) |> 
  #filter(politican_clean != "Spam") |> 
  mutate(politican_clean = ifelse(politican_clean == "Spam", NA, politican_clean)) |> 
  dplyr::select(ResponseId, recipient.cfscore.dyn, party_open_q) |> 
  distinct() |> 
  ggplot(aes(x = recipient.cfscore.dyn)) +
  geom_histogram()+ 
  facet_grid(~party_open_q) +
  theme_bw() +
  labs(title = "CF Score of politicians remembered",
       x = "CF Score",
       y = "Count")

df_er_open |> 
  dplyr::select(ResponseId, party_open_q, politican_clean, 
                recipient.cfscore.dyn, dwnom1, dwnom2, party_name, partisan) |> 
  #filter(politican_clean != "Spam") |> 
  mutate(politican_clean = ifelse(politican_clean == "Spam", NA, politican_clean)) |> 
  dplyr::select(ResponseId, dwnom1, party_open_q) |> 
  distinct() |> 
  ggplot(aes(x = dwnom1)) +
  geom_histogram()+ 
  facet_grid(~party_open_q) +
  theme_bw() +
  labs(title = "DW1 Score of politicians remembered",
       x = "DW1 Score",
       y = "Count")

df_er_open |> 
  dplyr::select(ResponseId, party_open_q, politican_clean, 
                recipient.cfscore.dyn, dwnom1, dwnom2, party_name, partisan) |> 
  #filter(politican_clean != "Spam") |> 
  mutate(politican_clean = ifelse(politican_clean == "Spam", NA, politican_clean)) |> 
  dplyr::select(ResponseId, dwnom2, party_open_q) |> 
  distinct() |> 
  ggplot(aes(x = dwnom2)) +
  geom_histogram()+ 
  facet_grid(~party_open_q) +
  theme_bw() +
  labs(title = "DW2 Score of politicians remembered",
       x = "DW2 Score",
       y = "Count")
## Ideology of recalled politicians in vs outpartisans 