# Lucid Examination 

df_lucid_check <-
  df_er_clean |> 
  dplyr::select(ResponseId, `Duration (in seconds)`, Q7.3:Q3.1_3, age, gender, hhi, 
                ethnicity, hispanic, political_party, region, zip, education) |> 
  mutate(gender = ifelse(gender_label == 1, "Male", "Female"),
         hhi_label = case_when(hhi == 1 ~ "Less than $14,999",
                         hhi == 2 ~ "$15,000 to $19,999",
                         hhi == 3 ~ "$20,000 to $24,999",
                         hhi == 4 ~ "$25,000 to $29,999",
                         hhi == 5 ~ "$30,000 to $34,999",
                         hhi == 6 ~ "$35,000 to $39,999",
                         hhi == 7 ~ "$40,000 to $44,999",
                         hhi == 8 ~ "$45,000 to $49,999",
                         hhi == 9 ~ "$50,000 to $54,999",
                         hhi == 10 ~ "$55,000 to $59,999",
                         hhi == 11 ~ "$60,000 to $64,999",
                         hhi == 12 ~ "$65,000 to $69,999",
                         hhi == 13 ~ "$70,000 to $74,999",
                         hhi == 14 ~ "$75,000 to $79,999",
                         hhi == 15 ~ "$80,000 to $84,999",
                         hhi == 16 ~ "$85,000 to $89,999",
                         hhi == 17 ~ "$90,000 to $94,999",
                         hhi == 18 ~ "$95,000 to $99,999",
                         hhi == 19 ~ "$100,000 to $124,999",
                         hhi == 20 ~ "$125,000 to $149,999",
                         hhi == 21 ~ "$150,000 to $174,999",
                         hhi == 22 ~ "$175,000 to $199,999",
                         hhi == 23 ~ "$200,000 to $249,999",
                         hhi == 24 ~ "$250,000 and above",
                         hhi == -3105 ~ "Prefer not to answer"),
         hhi_label = factor(hhi_label, levels=c("Less than $14,999", "$15,000 to $19,999", "$20,000 to $24,999", "$25,000 to $29,999", 
                                    "$30,000 to $34,999", "$35,000 to $39,999", "$40,000 to $44,999", "$45,000 to $49,999", 
                                    "$50,000 to $54,999", "$55,000 to $59,999", "$60,000 to $64,999", "$65,000 to $69,999", 
                                    "$70,000 to $74,999", "$75,000 to $79,999", "$80,000 to $84,999", "$85,000 to $89,999", 
                                    "$90,000 to $94,999", "$95,000 to $99,999", "$100,000 to $124,999", 
                                    "$125,000 to $149,999", "$150,000 to $174,999", "$175,000 to $199,999", 
                                    "$200,000 to $249,999", "$250,000 and above")),
         hhi_question_label = factor(Q7.11, levels=c("Less than $14,999", "$15,000 to $19,999", "$20,000 to $24,999", "$25,000 to $29,999", 
                                        "$30,000 to $34,999", "$35,000 to $39,999", "$40,000 to $44,999", "$45,000 to $49,999", 
                                        "$50,000 to $54,999", "$55,000 to $59,999", "$60,000 to $64,999", "$65,000 to $69,999", 
                                        "$70,000 to $74,999", "$75,000 to $79,999", "$80,000 to $84,999", "$85,000 to $89,999", 
                                        "$90,000 to $94,999", "$95,000 to $99,999", "$100,000 to $124,999", 
                                        "$125,000 to $149,999", "$150,000 to $174,999", "$175,000 to $199,999", 
                                        "$200,000 to $249,999", "$250,000 and above")),
         ethnicity_label = case_when(ethnicity == 1 ~	"White",
                               ethnicity == 2 ~ "Black, or African American",
                               ethnicity == 3 ~ "American Indian or Alaska Native", 
                               ethnicity == 4 ~ "Asian", 
                               ethnicity == 5 ~ "Asian", 
                               ethnicity == 6 ~ "Asian", 
                               ethnicity == 7 ~ "Asian", 
                               ethnicity == 8 ~ "Asian", 
                               ethnicity == 9 ~ "Asian", 
                               ethnicity == 10 ~ "Asian", 
                               ethnicity == 11 ~ "Pacific Islander",
                               ethnicity == 12 ~ "Pacific Islander",
                               ethnicity == 13 ~ "Pacific Islander",
                               ethnicity == 14 ~ "Pacific Islander",
                               ethnicity == 15 ~ "Some other race",
                               ethnicity == 16 ~ "Prefer not to answer"),
         hispanic_label = case_when(hispanic == 1 ~ "No , not of Hispanic, Latino, or Spanish origin",
                              hispanic == 2 ~ "Yes, Mexican, Mexican American, Chicano",
                              hispanic == 3 ~ "Yes, Cuban",
                              hispanic == 4 ~ "Yes, another Hispanic, Latino, or Spanish origin",
                              hispanic == 5 ~ "Yes, another Hispanic, Latino, or Spanish origin", 
                              hispanic == 6 ~ "Yes, another Hispanic, Latino, or Spanish origin", 
                              hispanic == 7 ~ "Yes, another Hispanic, Latino, or Spanish origin", 
                              hispanic == 8 ~ "Yes, another Hispanic, Latino, or Spanish origin", 
                              hispanic == 9 ~ "Yes, another Hispanic, Latino, or Spanish origin", 
                              hispanic == 10 ~ "Yes, another Hispanic, Latino, or Spanish origin", 
                              hispanic == 11 ~ "Yes, another Hispanic, Latino, or Spanish origin", 
                              hispanic == 12 ~ "Yes, another Hispanic, Latino, or Spanish origin", 
                              hispanic == 13 ~ "Yes, another Hispanic, Latino, or Spanish origin", 
                              hispanic == 14 ~ "Yes, another Hispanic, Latino, or Spanish origin",
                              hispanic == 15 ~ "Prefer not to answer"),
         ethnicity_label_detail = case_when(ethnicity == 1 ~	"White",
                               ethnicity == 2 ~ "Black, or African American",
                               ethnicity == 3 ~ "American Indian or Alaska Native",
                               ethnicity == 4 ~ "Asian *** Asian Indian",
                               ethnicity == 5 ~ "Asian *** Chinese",
                               ethnicity == 6 ~ "Asian *** Filipino",
                               ethnicity == 7 ~ "Asian *** Japanese",
                               ethnicity == 8 ~ "Asian *** Korean",
                               ethnicity == 9 ~ "Asian *** Vietnamese",
                               ethnicity == 10 ~ "Asian *** Other",
                               ethnicity == 11 ~ "Pacific Islander *** Native Hawaiian",
                               ethnicity == 12 ~ "Pacific Islander *** Guamanian",
                               ethnicity == 13 ~ "Pacific Islander *** Samoan",
                               ethnicity == 14 ~ "Pacific Islander *** Other Pacific Islander",
                               ethnicity == 15 ~ "Some other race",
                               ethnicity == 16 ~ "Prefer not to answer"),
         hispanic_label_detail = case_when(hispanic == 1 ~ "No , not of Hispanic, Latino, or Spanish origin",
                              hispanic == 2 ~ "Yes, Mexican, Mexican American, Chicano",
                              hispanic == 3 ~ "Yes, Cuban",
                              hispanic == 4 ~ "Yes, another Hispanic, Latino, or Spanish origin *** Argentina",
                              hispanic == 5 ~ "Yes, another Hispanic, Latino, or Spanish origin *** Colombia",
                              hispanic == 6 ~ "Yes, another Hispanic, Latino, or Spanish origin *** Ecuador",
                              hispanic == 7 ~ "Yes, another Hispanic, Latino, or Spanish origin *** El Salvadore",
                              hispanic == 8 ~ "Yes, another Hispanic, Latino, or Spanish origin *** Guatemala",
                              hispanic == 9 ~ "Yes, another Hispanic, Latino, or Spanish origin *** Nicaragua",
                              hispanic == 10 ~ "Yes, another Hispanic, Latino, or Spanish origin *** Panama",
                              hispanic == 11 ~ "Yes, another Hispanic, Latino, or Spanish origin *** Peru",
                              hispanic == 12 ~ "Yes, another Hispanic, Latino, or Spanish origin *** Spain",
                              hispanic == 13 ~ "Yes, another Hispanic, Latino, or Spanish origin *** Venezuela",
                              hispanic == 14 ~ "Yes, another Hispanic, Latino, or Spanish origin *** Other Country",
                              hispanic == 15 ~ "Prefer not to answer"),
         political_party_label = case_when(political_party == 1 ~ "Strong Democrat",
                                     political_party == 2 ~ "Not very strong Democrat",
                                     political_party == 3 ~ "Independent Democrat",
                                     political_party == 4 ~ "Independent - neither",
                                     political_party == 5 ~ "Independent Republican",
                                     political_party == 6 ~ "Other - leaning Democrat",
                                     political_party == 7 ~ "Other - neither",
                                     political_party == 8 ~ "Other - leaning Republican",
                                     political_party == 9 ~ "Not very strong Republican",
                                     political_party == 10 ~ "Strong Republican"), 
         education = case_when(education == 1 ~ "Some high school or less",
                               education == 2 ~ "High school graduate",
                               education == 3 ~ "Other post high school vocational training",
                               education == 4 ~ "Completed some college, but no degree",
                               education == 5 ~ "Associate's degree",
                               education == 6 ~ "Bachelor's degree",
                               education == 7 ~ "Master's or professional degree",
                               education == 8 ~ "Doctorate degree",
                               education == -3105 ~ "None of the above"),
         education = factor(education, levels=c("Some high school or less", "High school graduate","Other post high school vocational training", 
                                                "Completed some college, but no degree", "Associate's degree", "Bachelor's degree", 
                                                "Master's or professional degree", "Doctorate degree", "None of the above")),
         Q7.5 = factor(Q7.5, levels=c("Some high school or less", "High school graduate","Other post high school vocational training", 
                                      "Completed some college, but no degree", "Associate's degree", "Bachelor's degree", 
                                      "Master's or professional degree", "Doctorate degree", "None of the above")))


table(df_er_clean2$Q7.6, df_er_clean2$ethnicity)
df_er_clean2 |> 
  mutate(Q7.6= coalesce(Q7.6_1, Q7.6_2, Q7.6_3,Q7.6_4, Q7.6_5, Q7.6_6,Q7.6_7)) |> 
  dplyr::select(Q7.6, ethnicity) |> 
  drop_na() |> 
  group_by(Q7.6, ethnicity) |> 
  add_count() |> 
  ggplot(aes(x = Q7.6, y = ethnicity, fill = n)) +
  geom_tile()

df_er_clean2 |> 
  dplyr::select(Q7.7, hispanic) |> 
  drop_na() |> 
  group_by(Q7.7, hispanic) |> 
  add_count() |> 
  ggplot(aes(x = Q7.7, y = hispanic, fill = n)) +
  geom_tile()

table(df_er_clean2$Q7.4, df_er_clean2$gender)
df_er_clean2 |> 
  dplyr::select(Q7.4, gender) |> 
  drop_na() |> 
  group_by(Q7.4, gender) |> 
  add_count() |> 
  ggplot(aes(x = Q7.4, y = gender, fill = n)) +
  geom_tile()


table(df_er_clean2$Q7.11, df_er_clean2$hhi)
df_er_clean2 |> 
  dplyr::select(Q7.11, hhi) |> 
  drop_na() |> 
  group_by(Q7.11, hhi) |> 
  add_count() |> 
  ggplot(aes(x = Q7.11, y = hhi, fill = n)) +
  geom_tile()


table(df_er_clean2$Q7.5, df_er_clean2$education)

df_er_clean2 |> 
  dplyr::select(Q7.5, education) |> 
  drop_na() |> 
  group_by(Q7.5, education) |> 
  add_count() |> 
  ggplot(aes(x = Q7.5, y = education, fill = n)) +
  geom_tile()

