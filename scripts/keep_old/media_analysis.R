# Media Analysis 

# Libraries
library("tidyverse")
options(scipen = 999)
# Data 
load("data/kim_et_al/dataverse_files/individual_shows.RData")
load("data/kim_et_al/dataverse_files/screen_panel.RData")
load("data/kim_et_al/dataverse_files/show_screen_panel.RData")
load("data/politicians/showdatawithpols.RData")
load("data/kim_et_al/dime_contrib_matches_with_info.RData")




show.all |> 
  dplyr::select(name) |> 
  distinct() |> 
  pull(name)
