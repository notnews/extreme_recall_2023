## In text numbers 

# Working directory
setwd(githubdir)
setwd("extreme_recall2")

# Libraries
library(tidyverse)
library(readxl)

# Source the data file
source("scripts/survey_df_clean.R")


df_er_open |> 
  ungroup() |> 
  dplyr::select(bonica.rid, politican_clean) |> 
  drop_na(bonica.rid) |> 
  add_count(bonica.rid, name = "n_polit") |> 
  add_count() |> 
  distinct() |> 
  arrange(-n_polit) |> 
  print(n = 20)

# XX\% of the recollections are of three politicians
(611+592+260)/3119


df_er_open |> 
  ungroup() |> 
  filter(party_open_q == "Democrat") |>  
  filter(party_name != "Republican") |> 
  dplyr::select(bonica.rid, politican_clean) |> 
  drop_na(bonica.rid) |> 
  add_count(bonica.rid, name = "n_polit") |> 
  add_count() |> 
  distinct() |> 
  arrange(-n_polit) |> 
  print(n = 20)


df_er_open |> 
  mutate(seat = ifelse(politican_clean == "Jim Danner", "state:lower",
                       ifelse(politican_clean == "Pete Aguilar", "federal:house",
                              ifelse(politican_clean == "Andrew Johnson", "federal:president",
                                     ifelse(politican_clean == "Ross Perot", "federal:president",
                                            ifelse(politican_clean == "Marianne Williamson", "federal:president", seat)))))) |> 
  ungroup() |> 
  dplyr::select(bonica.rid, politican_clean, most_recent_office, seat) |> 
  drop_na(bonica.rid) |> 
  add_count(bonica.rid, name = "n_polit") |> 
  add_count() |> 
  distinct() |> 
  arrange(-n_polit)


df_er_open |> 
  ungroup() |> 
  drop_na(bonica.rid) |> 
  mutate(seat = ifelse(politican_clean == "Jim Danner", "state:lower",
                       ifelse(politican_clean == "Pete Aguilar", "federal:house",
                              ifelse(politican_clean == "Andrew Johnson", "federal:president",
                                     ifelse(politican_clean == "Ross Perot", "federal:president",
                                            ifelse(politican_clean == "Marianne Williamson", "federal:president", seat)))))) |> 
  add_count(seat, name = "number")  |> 
  distinct() |> 
  ggplot(aes(x = reorder(seat, number))) + 
  geom_bar(aes(y = after_stat(count / sum(count)))) + 
  theme_bw() +
  labs(x = "Office",
       y = "Proportion") +
  coord_flip()
  

df_er_open |> 
  ungroup() |> 
  drop_na(bonica.rid) |> 
  mutate(seat = str_squish(seat),
         seat = ifelse(politican_clean == "Jim Danner", "state:lower",
                       ifelse(politican_clean == "Pete Aguilar", "federal:house",
                              ifelse(politican_clean == "Andrew Johnson", "federal:president",
                                     ifelse(politican_clean == "Ross Perot", "federal:president",
                                            ifelse(politican_clean == "Marianne Williamson", "federal:president", seat)))))) |> 
  add_count(seat, name = "number")  |> 
  dplyr::select(seat, number) |> 
  distinct() |> 
  arrange(-number)
# 374/3119
104+185
