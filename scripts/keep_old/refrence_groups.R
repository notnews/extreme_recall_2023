# Purpose: Prepare the reference group data sets 
# Date: 2023-12-15
# Author: Daniel Weitzel
# Email: daniel.weitzel@colostate.edu
# Purpose: Analyze the data

# Working directory
setwd(githubdir)
setwd("extreme_recall2")

## We will use the following reference groups:
## - current congress
## - current congress + governors and all presidents 


## Generate a data set of current members of congress
df_dime_congress <-
  read_csv("data/cf_scores/dime_recipients_1979_2020.csv.gz") |> 
  mutate(party = ifelse(party.orig == "100", "Democrat",
                        ifelse(party.orig == "200", "Republican", NA))) |> 
  filter(seat %in% c("federal:senate", "federal:house")) |>
  filter(gwinner == "W") |> 
  group_by(bonica.rid) |> 
  filter(fecyear > 2019)|> 
  dplyr::select(bonica.rid, election, cycle, fecyear, name, lname, fname, 
                cand.gender:irt.cfscore, party, seat) |> 
  mutate(data = "congress")

## Generate a data set of current governors
df_dime_governor <-
  read_csv("data/cf_scores/dime_recipients_1979_2020.csv.gz") |> 
  mutate(party = ifelse(party.orig == "100", "Democrat",
                        ifelse(party.orig == "200", "Republican", NA))) |> 
  filter(seat %in% c("state:governor")) |>
  #filter(gwinner == "W") |> 
  group_by(state) |> 
  #filter(fecyear == max(fecyear)) |> 
  dplyr::select(bonica.rid,gwinner, state,election, cycle, fecyear, name, lname, fname, 
                cand.gender:irt.cfscore, party, seat) |> 
  mutate(data = "governor")

## Generate a data set of all presidents
df_dime_president <-
  read_csv("data/cf_scores/dime_recipients_1979_2020.csv.gz") |> 
  mutate(party = ifelse(party.orig == "100", "Democrat",
                        ifelse(party.orig == "200", "Republican", NA))) |> 
  filter(seat %in% c("federal:president")) |>
  #filter(gwinner == "W") |> 
  group_by(state) |> 
  #filter(fecyear == max(fecyear)) |> 
  dplyr::select(bonica.rid,gwinner, state,election, cycle, fecyear, name, lname, fname, 
                cand.gender:irt.cfscore, party, seat) |> 
  mutate(data = "president") 
  filter(fecyear == max(fecyear))



# |> 
#   dplyr::select(bonica.rid, election, cycle, fecyear, name, lname, fname, 
#                 cand.gender:irt.cfscore, party, seat) |> 
#   mutate(name_f = as.factor(name)) |> 
#   filter(bonica.rid %in% list_bonica_closed) |> 
#   group_by(bonica.rid) |> 
#   filter(cycle == max(cycle)) |> 
#   mutate(drop = ifelse(bonica.rid == "cand1235" & seat == "federal:senate", "drop",
#                        ifelse(bonica.rid == "cand41775" & seat == "federal:house", "drop", "keep")),
#          executive = str_detect(seat, "president|governor")) |> # this classifies the last race
#   filter(drop == "keep") |> 
#   dplyr::select(-drop) |>  
#   left_join(df_politicians) |> 
#   rename(party_name = party)




