# Title: analysis.R
# Date: 2023-12-15
# Author: Daniel Weitzel
# Email: daniel.weitzel@colostate.edu
# Purpose: Analyze the data

# Working directory
setwd(githubdir)

# Libraries
source(file = "extreme_recall2/scripts/qualtrics.R")

## Randomization check
table(df_er_clean$experiment)
df_er_clean2 <-
  df_er_clean |> 
  mutate(gender = ifelse(gender == 1, "Male", "Female"),
         hhi = case_when(hhi == 1 ~ "Less than $14,999",
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
         hhi = factor(hhi, levels=c("Less than $14,999", "$15,000 to $19,999", "$20,000 to $24,999", "$25,000 to $29,999", 
                                    "$30,000 to $34,999", "$35,000 to $39,999", "$40,000 to $44,999", "$45,000 to $49,999", 
                                    "$50,000 to $54,999", "$55,000 to $59,999", "$60,000 to $64,999", "$65,000 to $69,999", 
                                    "$70,000 to $74,999", "$75,000 to $79,999", "$80,000 to $84,999", "$85,000 to $89,999", 
                                    "$90,000 to $94,999", "$95,000 to $99,999", "$100,000 to $124,999", 
                                    "$125,000 to $149,999", "$150,000 to $174,999", "$175,000 to $199,999", 
                                    "$200,000 to $249,999", "$250,000 and above")),
         Q7.11 = factor(Q7.11, levels=c("Less than $14,999", "$15,000 to $19,999", "$20,000 to $24,999", "$25,000 to $29,999", 
                                    "$30,000 to $34,999", "$35,000 to $39,999", "$40,000 to $44,999", "$45,000 to $49,999", 
                                    "$50,000 to $54,999", "$55,000 to $59,999", "$60,000 to $64,999", "$65,000 to $69,999", 
                                    "$70,000 to $74,999", "$75,000 to $79,999", "$80,000 to $84,999", "$85,000 to $89,999", 
                                    "$90,000 to $94,999", "$95,000 to $99,999", "$100,000 to $124,999", 
                                    "$125,000 to $149,999", "$150,000 to $174,999", "$175,000 to $199,999", 
                                    "$200,000 to $249,999", "$250,000 and above")),
         ethnicity = case_when(ethnicity == 1 ~	"White",
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
         hispanic = case_when(hispanic == 1 ~ "No , not of Hispanic, Latino, or Spanish origin",
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
         # ethnicity = case_when(ethnicity == 1 ~	"White",
         #                       ethnicity == 2 ~ "Black, or African American",
         #                       ethnicity == 3 ~ "American Indian or Alaska Native", 
         #                       ethnicity == 4 ~ "Asian *** Asian Indian", 
         #                       ethnicity == 5 ~ "Asian *** Chinese", 
         #                       ethnicity == 6 ~ "Asian *** Filipino", 
         #                       ethnicity == 7 ~ "Asian *** Japanese", 
         #                       ethnicity == 8 ~ "Asian *** Korean", 
         #                       ethnicity == 9 ~ "Asian *** Vietnamese", 
         #                       ethnicity == 10 ~ "Asian *** Other", 
         #                       ethnicity == 11 ~ "Pacific Islander *** Native Hawaiian",
         #                       ethnicity == 12 ~ "Pacific Islander *** Guamanian",
         #                       ethnicity == 13 ~ "Pacific Islander *** Samoan",
         #                       ethnicity == 14 ~ "Pacific Islander *** Other Pacific Islander",
         #                       ethnicity == 15 ~ "Some other race",
         #                       ethnicity == 16 ~ "Prefer not to answer"),
         # hispanic = case_when(hispanic == 1 ~ "No , not of Hispanic, Latino, or Spanish origin",
         #                      hispanic == 2 ~ "Yes, Mexican, Mexican American, Chicano",
         #                      hispanic == 3 ~ "Yes, Cuban",
         #                      hispanic == 4 ~ "Yes, another Hispanic, Latino, or Spanish origin *** Argentina",
         #                      hispanic == 5 ~ "Yes, another Hispanic, Latino, or Spanish origin *** Colombia", 
         #                      hispanic == 6 ~ "Yes, another Hispanic, Latino, or Spanish origin *** Ecuador", 
         #                      hispanic == 7 ~ "Yes, another Hispanic, Latino, or Spanish origin *** El Salvadore", 
         #                      hispanic == 8 ~ "Yes, another Hispanic, Latino, or Spanish origin *** Guatemala", 
         #                      hispanic == 9 ~ "Yes, another Hispanic, Latino, or Spanish origin *** Nicaragua", 
         #                      hispanic == 10 ~ "Yes, another Hispanic, Latino, or Spanish origin *** Panama", 
         #                      hispanic == 11 ~ "Yes, another Hispanic, Latino, or Spanish origin *** Peru", 
         #                      hispanic == 12 ~ "Yes, another Hispanic, Latino, or Spanish origin *** Spain", 
         #                      hispanic == 13 ~ "Yes, another Hispanic, Latino, or Spanish origin *** Venezuela", 
         #                      hispanic == 14 ~ "Yes, another Hispanic, Latino, or Spanish origin *** Other Country",
         #                      hispanic == 15 ~ "Prefer not to answer"),
         political_party = case_when(political_party == 1 ~ "Strong Democrat",
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

## Visualize number of politicians in OPEN
df_er_open |> 
  dplyr::select(ResponseId, politicians_open_count, party) |> 
  distinct() |> 
  mutate(party = ifelse(party == "dem", "Democrats", "Republican")) |> 
  ggplot(aes(x = politicians_open_count)) +
  geom_bar() +
  facet_wrap(~party) +
  theme_bw() +
  labs(title = "Number of politicians named in open question",
       y = "Count",
       x = "Number of politicians")

## Visualize number of politicians in CLOSED
df_er_closed |> 
  dplyr::select(ResponseId, politicians_closed_count, party) |> 
  distinct() |> 
  mutate(party = ifelse(party == "dem", "Democrats", "Republican")) |> 
  ggplot(aes(x = politicians_closed_count)) +
  geom_bar() +
  facet_wrap(~party) +
  theme_bw() +
  labs(title = "Number of politicians named in closed question",
       y = "Count",
       x = "Number of politicians")

## Visualize extremity score in open
## recipient.cfscore.dyn, dwdime, dwnom2
df_er_open |> 
  ggplot(aes(x = recipient.cfscore.dyn, 
             #x = dwdime,
             #x = dwnom2,
             color = party_name)) +
  geom_density() +
  scale_color_manual(values = c ("blue","red")) +
  theme_bw()

## Visualize extremity score in closed
## recipient.cfscore.dyn, dwdime, dwnom2
df_er_closed |> 
  dplyr::select(ResponseId, bonica.rid, party,recipient.cfscore.dyn) |>
  distinct() |> 
  mutate(Party = ifelse(party == "dem", "Democrats", "Republican")) |> 
  ggplot(aes(x = recipient.cfscore.dyn, 
             #x = dwdime,
             #x = dwnom2,
             color = Party)) +
  geom_density() +
  scale_color_manual(values = c ("blue","red")) +
  theme_bw() +
  facet_wrap(~Party) +
  labs(title = "Extremity score of recalled politicians in closed questions",
       y = "Density",
       x = "CF Score, dynamic") +
  theme(legend.position = "none")

## Visualizing recency bias 
df_er_closed |> 
  drop_na(recency) |> 
  dplyr::select(ResponseId, bonica.rid, party,recency) |>
  distinct() |> 
  mutate(Party = ifelse(party == "dem", "Democrats", "Republican")) |> 
  ggplot(aes(x = recency,
             fill = Party)) +
  geom_bar() +
  theme_bw() +
  facet_wrap(~Party) +
  scale_fill_manual(values = c ("blue","red")) +
  theme(legend.position = "none")

## Visualizing gender bias 
df_er_closed |> 
  drop_na(cand.gender) |> 
  mutate(Party = ifelse(party == "dem", "Democrats", "Republican")) |> 
  ggplot(aes(x = cand.gender,
             fill = Party)) +
  geom_bar() +
  theme_bw() +
  facet_wrap(~Party) +
  scale_fill_manual(values = c ("blue","red")) +
  theme(legend.position = "none")

## Visualizing executive bias 
df_er_closed |> 
  drop_na(most_recent_office) |> 
  mutate(Party = ifelse(party == "dem", "Democrats", "Republican")) |> 
  ggplot(aes(x = most_recent_office,
             fill = Party)) +
  geom_bar() +
  theme_bw() +
  facet_wrap(~Party) +
  scale_fill_manual(values = c ("blue","red")) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle=45, vjust=1, hjust=1))

# Models
