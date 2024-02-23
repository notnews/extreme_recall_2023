# Working directory
setwd(githubdir)
setwd("extreme_recall2")

# Libraries
library(tidyverse)
library(readxl)
library(ggpubr)

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


df_er_open |> 
  ungroup() |> 
  dplyr::select(politican_clean, party_name) |> 
  drop_na(politican_clean) |> 
  add_count(politican_clean) |> 
  distinct() |> 
  mutate(proportion = n/sum(n)) |> 
  #ggplot(aes(x = reorder(politican_clean, n),fill = party_name)) +
  ggplot(aes(x = reorder(politican_clean, proportion),y = proportion, color = party_name)) +
  #geom_bar(aes(y = stat(count / sum(count)))) +
  geom_point() +
  coord_flip() +
  labs(title = "Frequency of politicians named in open ended question",
       x = "Politician",
       y = "Proportion",
       #caption = "Question: When you think about the Democratic/Republican Party, which political leader(s) first come to mind? Name up to three ..."
       ) + scale_color_manual(name="Party", 
                             labels = c("Democrat", "Republican"), 
                             values = c("Democrat"="blue", "Republican"="#f8766d")) +
  theme_bw() + 
  theme(legend.position = "none")


## Inline numbers for the manuscript
df_er_open |> 
  ungroup() |> 
  dplyr::select(politican_clean, party_name) |> 
  filter(party_name %in%  c("Democrat")) |> 
  drop_na(politican_clean) |> 
  add_count(politican_clean) |> 
  distinct() |> 
  mutate(proportion = n/sum(n)) |> 
  arrange(-proportion) |> 
  print(n = 100)

#1 Joseph Biden                 Democrat     592     0.357 
#2 Barack Obama                 Democrat     260     0.157 
#3 Kamala Harris                Democrat     229     0.138 
0.357 + 0.157 + 0.138
# 0.652


df_er_open |> 
  ungroup() |> 
  dplyr::select(politican_clean, party_name) |> 
  filter(party_name %in%  c("Republican")) |> 
  drop_na(politican_clean) |> 
  add_count(politican_clean) |> 
  distinct() |> 
  mutate(proportion = n/sum(n)) |> 
  arrange(-proportion)|> 
  print(n = 100)

#1 Donald Trump             Republican   611     0.376 
#2 Ron DeSantis             Republican   185     0.114 
#3 Nikki Haley              Republican   104     0.064 
0.376 + 0.114 + 0.064
# 0.554
## Figures for the manuscript
figure_1_dem <-
  df_er_open |> 
  ungroup() |> 
  dplyr::select(politican_clean, party_name) |> 
  filter(party_name %in%  c("Democrat")) |> 
  drop_na(politican_clean) |> 
  add_count(politican_clean) |> 
  distinct() |> 
  mutate(proportion = n/sum(n),
         politican_clean = ifelse(politican_clean == "Hillary Clinton, ambiguous", "Clinton",
                                  ifelse(politican_clean == "John F. Kennedy ambiguous", "Kennedy", 
                                         ifelse(politican_clean == "Alexandria Ocasio Cortez",  "A. Ocasio Cortez", politican_clean))),
         politican_clean = str_remove_all(politican_clean, ", unambiguous")) |> 
  #top_n(15) |> 
  #ggplot(aes(x = reorder(politican_clean, n),fill = party_name)) +
  ggplot(aes(x = reorder(politican_clean, proportion),y = proportion, color = party_name)) +
  #geom_bar(aes(y = stat(count / sum(count)))) +
  geom_point() +
  coord_flip() +
  labs(title = "Democratic Party",
       x = "",
       y = "Proportion",
       #caption = "Question: When you think about the Democratic/Republican Party, which political leader(s) first come to mind? Name up to three ..."
  ) + scale_color_manual(name="Party", 
                         labels = c("Democrat", "Republican"), 
                         values = c("Democrat"="blue", "Republican"="#f8766d")) +
  theme_bw() + 
  theme(legend.position = "none") +
  ylim(0,0.4)

figure_1_rep <-
  df_er_open |> 
  ungroup() |> 
  dplyr::select(politican_clean, party_name) |> 
  filter(party_name %in%  c("Republican")) |> 
  drop_na(politican_clean) |> 
  add_count(politican_clean) |> 
  distinct() |> 
  mutate(proportion = n/sum(n),
         politican_clean = ifelse(politican_clean == "George W Bush, ambiguous", "Bush", politican_clean),
         politican_clean = str_remove_all(politican_clean, ", unambiguous")) |> 
  #top_n(15) |> 
  #ggplot(aes(x = reorder(politican_clean, n),fill = party_name)) +
  ggplot(aes(x = reorder(politican_clean, proportion),y = proportion, color = party_name)) +
  #geom_bar(aes(y = stat(count / sum(count)))) +
  geom_point() +
  coord_flip() +
  labs(title = "Republican Party",
       x = "",
       y = "Proportion",
       #caption = "Question: When you think about the Democratic/Republican Party, which political leader(s) first come to mind? Name up to three ..."
  ) + scale_color_manual(name="Party", 
                         labels = c("Democrat", "Republican"), 
                         values = c("Democrat"="blue", "Republican"="#f8766d")) +
  theme_bw() + 
  theme(legend.position = "none") +
  ylim(0,0.4)

ggarrange(figure_1_dem, figure_1_rep, labels = c("A", "B"), ncol = 2, nrow = 1, widths = c(0.5, 0.5))

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

df_er_open |> 
  dplyr::select(ResponseId, party_open_q, politican_clean, 
                recipient.cfscore.dyn, dwnom1, dwnom2, party_name, partisan) |> 
  mutate(party_open_q = ifelse(str_detect(party_open_q,"Demo"), "Democrat",
                               ifelse(str_detect(party_open_q, "Repub"), "Republican", NA)),
         partisan_type = ifelse(partisan == party_open_q, "Copartisan", "Outpartisan"),
         partisan_type = ifelse(is.na(partisan_type), "Independent/Other", partisan_type),
         politican_clean = ifelse(politican_clean == "Spam", NA, politican_clean)) |> 
  dplyr::select(ResponseId, recipient.cfscore.dyn, partisan_type) |> 
  distinct() |> 
  ggplot(aes(x = recipient.cfscore.dyn)) +
  geom_histogram()+ 
  facet_grid(~partisan_type) +
  theme_bw() +
  labs(title = "Ideology of recalled politicians by partisanship",
       x = "CF Score",
       y = "Count")

df_er_open |> 
  dplyr::select(ResponseId, party_open_q, politican_clean, 
                recipient.cfscore.dyn, dwnom1, dwnom2, party_name, partisan) |> 
  mutate(party_open_q = ifelse(str_detect(party_open_q,"Demo"), "Democrat",
                               ifelse(str_detect(party_open_q, "Repub"), "Republican", NA)),
         partisan_type = ifelse(partisan == party_open_q, "Copartisan", "Outpartisan"),
         partisan_type = ifelse(is.na(partisan_type), "Independent/Other", partisan_type),
         politican_clean = ifelse(politican_clean == "Spam", NA, politican_clean)) |> 
  dplyr::select(ResponseId, recipient.cfscore.dyn, partisan) |> 
  distinct() |> 
  ggplot(aes(x = recipient.cfscore.dyn)) +
  geom_histogram()+ 
  facet_grid(~partisan) +
  theme_bw() +
  labs(title = "Ideology of recalled politicians by partisanship",
       x = "CF Score",
       y = "Count")

