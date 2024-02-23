# state clean


state_reduced <-
  df_er_state_open |> 
  dplyr::select(politicians_state_open) |> 
  drop_na() |> 
  mutate(politicians_state_open = str_squish(politicians_state_open),
         politicians_state_open = str_to_lower(politicians_state_open)) |> 
  distinct() |> 
  arrange()

write_csv(state_reduced, "data/politicians/state_politicians.csv")


check <-
  df_er_open |> 
  ungroup() |> 
  dplyr::select(politican_clean, party_open_q, party_name) |> 
  drop_na(politican_clean) |> 
  filter(is.na(party_name)) |> 
  group_by(politican_clean, party_open_q) |> 
  add_count() |> 
  ungroup() |> 
  distinct() |> 
  mutate(party_name2 = ifelse(n > 10, party_open_q, NA)) |> 
  dplyr::select(-party_name)


write_csv(check, "data/politicians/partisanship.csv")
