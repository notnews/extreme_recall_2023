# Study 2

# Libraries
library("tidyverse")
library("readxl")
library("texreg")
library("fixest")
library("sjPlot")
library("scales")

# Source the data file
source("scripts/00_survey_df_clean.R")

## Data
list_bonica <- c(
  # Republicans
  "cand625nominee", "cand3331", "cand3334nominee", "cand105110", "cand584", "cand144919", 
  "cand992", "cand110286", "cand1403", "cand418", "cand108459", "cand837", 
  "cand100270nominee", "cand1573",
  # Democrats
  "cand221nominee", "cand3333nominee", "cand1572nominee", "cand760", 
  "cand3335nominee", "cand158985", "cand159005", "cand1235", "cand140679",
  "cand1616", "cand744", "cand146299", "cand1600", "cand41775",
  # open
  "cand3335nominee", "cand221nominee", "cand100270nominee", "cand166080", 
  "cand545", "cand132239", "cand31555" ,"cand137524", "cand584", "cand158985", 
  "cand968", "cand41326", "cand1572nominee", "cand1607", "cand3333nominee",
  "cand3334nominee", "cand3331", "cand105110", "cand108459", "cand837", 
  "cand82171","cand625nominee", "cand1235", "cand760", "cand606", "cand1316",
  "cand402", "cand159005", "cand1600", "cand335", "cand68896224", "cand3330", 
  "cand1023", "cand23468", "cand153422","cand1583", "cand151850", "cand130816", 
  "cand116996", "cand141779", "cand1161", "cand110286","cand583", "cand159281", 
  "cand1002", "cand149269", "cand140679", "cand137716", "cand1403", "cand159285",
  "cand141152", "cand50384", "cand144919", "cand1608", "cand61168", "cand142635",
  "cand180820", "cand1603", "cand418", "cand69014415", "cand607", "cand909", 
  "cand188224", "cand69030231", "cand50421", "cand47166", "cand1119", "cand147768", 
  "cand190976", "cand142740", "cand197013", "cand43832", "cand140557", "cand100450",
  "cand10", "cand160447", "cand616", "cand143041", "cand1312", "cand145542",
  "cand104638", "cand1635", "cand1588", "cand1592", "cand303", "cand143308", 
  "cand28657", "cand152762", "cand161330", "cand141770", "cand153043", "cand1182", 
  "cand68893433", "cand68896317", "cand190769", "cand189537", "cand137082",
  "cand142904","cand75384", "cand75700", "cand131167", "cand3332nominee")

df_er_open_w2 <-
  df_er_open_w2 |> 
  ungroup() |> 
  mutate(recipient.cfscore.dyn = ifelse(is.na(politican_clean_count), NA, recipient.cfscore.dyn),
         recipient.cfscore.dyn = ifelse(party_open_q != party_name, NA, recipient.cfscore.dyn),
         recipient.cfscore.dyn_scaled1 = ifelse(party_open_q == "Democrat", recipient.cfscore.dyn, NA),
         recipient.cfscore.dyn_scaled2 = ifelse(party_open_q == "Republican", recipient.cfscore.dyn, NA),
         recipient.cfscore.dyn_scaled_comb = coalesce(abs(recipient.cfscore.dyn_scaled1),recipient.cfscore.dyn_scaled2),
         recipient.cfscore.dyn_scaled_comb = rescale(recipient.cfscore.dyn_scaled_comb),
         recipient.cfscore.dyn_scaled1 = rescale(abs(recipient.cfscore.dyn_scaled1)),
         recipient.cfscore.dyn_scaled2 = rescale(recipient.cfscore.dyn_scaled2),
         recipient.cfscore.dyn_scaled = coalesce(recipient.cfscore.dyn_scaled1,recipient.cfscore.dyn_scaled2),
         recipient.cfscore.dyn_scaled_prop_1 = ifelse(party_open_q == "Democrat", recipient.cfscore.dyn_prop, NA),
         recipient.cfscore.dyn_scaled_prop_2 = ifelse(party_open_q == "Republican", recipient.cfscore.dyn_prop, NA),
         recipient.cfscore.dyn_scaled_moderate_1 = ifelse(party_open_q == "Democrat", recipient.cfscore.dyn_extreme, NA),
         recipient.cfscore.dyn_scaled_moderate_2 = ifelse(party_open_q == "Republican", recipient.cfscore.dyn_extreme, NA),
         recipient.cfscore.dyn_scaled_extreme_1 = ifelse(party_open_q == "Democrat", recipient.cfscore.dyn_moderate, NA),
         recipient.cfscore.dyn_scaled_extreme_2 = ifelse(party_open_q == "Republican", recipient.cfscore.dyn_moderate, NA),
         recipient.cfscore.dyn_scaled_comb = coalesce(abs(recipient.cfscore.dyn_scaled1),recipient.cfscore.dyn_scaled2),
         recipient.cfscore.dyn_scaled_comb = rescale(recipient.cfscore.dyn_scaled_comb),
         recipient.cfscore.dyn_scaled_prop_comb = coalesce(abs(recipient.cfscore.dyn_scaled_prop_1),recipient.cfscore.dyn_scaled_prop_2),
         recipient.cfscore.dyn_scaled_prop_comb = rescale(recipient.cfscore.dyn_scaled_prop_comb),
         recipient.cfscore.dyn_scaled_extreme_comb = coalesce(abs(recipient.cfscore.dyn_scaled_extreme_1),recipient.cfscore.dyn_scaled_extreme_2),
         recipient.cfscore.dyn_scaled_extreme_comb = rescale(recipient.cfscore.dyn_scaled_extreme_comb),
         recipient.cfscore.dyn_scaled_moderate_comb = coalesce(abs(recipient.cfscore.dyn_scaled_moderate_1),recipient.cfscore.dyn_scaled_moderate_2),
         recipient.cfscore.dyn_scaled_moderate_comb = rescale(recipient.cfscore.dyn_scaled_moderate_comb),
         number_inverse = ((number -4)*-1)/3,)|> 
  dplyr::select(-c(recipient.cfscore.dyn_scaled1,recipient.cfscore.dyn_scaled2)) |> 
  ungroup()


df_polinfo <-   read_csv("data/cf_scores/dime_recipients_1979_2020.csv.gz") |> 
  dplyr::select(cycle, fecyear, bonica.rid, state, seat, cand.gender, 
                recipient.cfscore.dyn, dwdime, dwnom1, dwnom2, ICPSR, ICPSR2, 
                Cand.ID, FEC.ID, NID, party.orig) |> 
  group_by(bonica.rid) |> 
  filter(cycle == max(cycle)) |> 
  mutate(drop = ifelse(bonica.rid == "cand1235" & seat == "federal:senate", "drop",
                       ifelse(bonica.rid == "cand41775" & seat == "federal:house", "drop", "keep")),
         executive = str_detect(seat, "president|governor"),
         party_name = ifelse(party.orig == "100", "Democrat",
                             ifelse(party.orig == "200", "Republican", NA))) |> # this classifies the last race
  filter(drop == "keep") |> 
  dplyr::select(-drop) |> 
  filter(bonica.rid %in% list_bonica) 

df_attributes <- read_excel("data/politicians/politicians_attributes.xlsx") |> 
  drop_na(bonica.rid) |> 
  dplyr::select(-c(party, name, executive, state, gender))

df_selected <- 
  df_er_closed |> 
  ungroup() |> 
  dplyr::select(bonica.rid, ResponseId) |> 
  drop_na(bonica.rid) |> 
  mutate(selected = 1)


df_id <- 
  df_er_closed |> 
  ungroup() |> 
  dplyr::select(ResponseId) |> 
  drop_na() |> 
  distinct()

df_long <-
  df_er_closed |> 
  mutate(bonica.rid = ifelse(bonica.rid == "", NA, bonica.rid)) |> 
  dplyr::select(politicians_closed, bonica.rid) |> 
  drop_na() |> 
  distinct() 

df_closed_long_w2 <- 
  crossing(df_long, df_id) |> 
  left_join(df_selected) |> 
  mutate(selected = replace_na(selected, 0)) |> 
  left_join(df_polinfo) |> 
  mutate(executive = ifelse(seat %in% c("state:governor", "federal:president"), 1, 0),
         party_name = replace_na(party_name, "Republican"),
         recipient.cfscore.dyn_scaled1 = ifelse(party_name == "Democrat", recipient.cfscore.dyn, NA),
         recipient.cfscore.dyn_scaled2 = ifelse(party_name == "Republican", recipient.cfscore.dyn, NA),
         recipient.cfscore.dyn_scaled_comb = coalesce(abs(recipient.cfscore.dyn_scaled1),recipient.cfscore.dyn_scaled2),
         recipient.cfscore.dyn_scaled_comb = rescale(recipient.cfscore.dyn_scaled_comb),
         recipient.cfscore.dyn_scaled1 = rescale(abs(recipient.cfscore.dyn_scaled1)),
         recipient.cfscore.dyn_scaled2 = rescale(recipient.cfscore.dyn_scaled2),
         recipient.cfscore.dyn_scaled = coalesce(recipient.cfscore.dyn_scaled1,recipient.cfscore.dyn_scaled2),
         cand.gender = ifelse(politicians_closed == "Greg Abbott", "M",cand.gender ))|> 
  dplyr::select(-c(recipient.cfscore.dyn_scaled1,recipient.cfscore.dyn_scaled2)) |> 
  ungroup() |> 
  left_join(df_attributes) |> 
  left_join(df_er_participants) |> 
  mutate(most_recent_office_f = as.factor(most_recent_office),
         most_recent_office_f = relevel(most_recent_office_f, ref = "president"))

rm(df_long, df_id, df_selected, list_bonica, df_polinfo, df_attributes)


## Inline Numbers
## Inline numbers for the manuscript
df_er_open_w2 |> 
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


df_er_open_w2 |> 
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


# Figure 4
figure_1_dem <-
  df_er_open_w2 |> 
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
  df_er_open_w2 |> 
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

## Figure 5
ggplot(df_er_open_w2, aes(x = recipient.cfscore.dyn_scaled_prop_comb)) +
  #geom_histogram(aes(x = recipient.cfscore.dyn_scaled_comb, fill = party_open_q, alpha = 0.8)) + 
  geom_histogram(data=subset(df_er_open_w2,party_open_q == 'Democrat'),fill = "blue", alpha = 0.5) +
  geom_histogram(data=subset(df_er_open_w2,party_open_q == 'Republican'),fill = "red", alpha = 0.5) +
  # geom_histogram(aes(x = dwnom2), fill = "blue") + 
  theme_bw()+
  labs(#title = "CF Score Distribution",
    #subtitle = "Political parties are scaled on shared min and max",
    x = "Extremity", y = "Count", fill = "Party")  +
  theme(legend.position = "none") +
  geom_vline(xintercept = 0.53, linetype = "dashed", color = "blue") +
  geom_vline(xintercept = 0.64, linetype = "dashed", color = "red") 
## export as w2_scaled_extremity_distribution 1996x1246

## Table 3
lm_tab3_1 <- feols(recipient.cfscore.dyn_scaled_prop_comb ~ number_inverse, data = df_er_open_w2, se = "cluster", cluster = "ResponseId")
lm_tab3_2 <- feols(recipient.cfscore.dyn_scaled_prop_comb ~ number_inverse*party_open_q, data = df_er_open_w2, se = "cluster", cluster = "ResponseId")

screenreg(list(lm_tab3_1, lm_tab3_2),
          custom.coef.names = c("(Intercept)", "Recall Order", "Republican", "Recall Order*Republican"),
          custom.model.names = c("Ideological Extremity", "Ideological Extremity"))

texreg(list(lm_tab3_1, lm_tab3_2),
       custom.coef.names = c("(Intercept)", "Recall Order", "Republican", "Recall Order*Republican"),
       custom.model.names = c("Ideological Extremity", "Ideological Extremity"))

## Figure 6
lm_fig_6 <- feols(recipient.cfscore.dyn_scaled_prop_comb ~ party_open_q*partisan, data = df_er_open_w2, se = "cluster", cluster = "ResponseId")

plot_model(lm_fig_6, type = "int") + 
  theme_bw() +
  labs(title = "",
       x = "Political Party in survey question",
       y = "Extremity",
       color = 'Partisanship of respondent') +
  scale_color_manual(values = c("blue", "red")) +
  theme(legend.position = "bottom")
# export as interaction_party_partisanship_comb with 825x825

## Table 4
lm_tab4_1 <- lmer(selected ~ recipient.cfscore.dyn_scaled_comb + (1|ResponseId), data=df_closed_long_w2)
lm_tab4_2 <- lmer(selected ~ recipient.cfscore.dyn_scaled_comb + race_ethnicity + recency + most_recent_office_f + (1|ResponseId), data=df_closed_long_w2)

screenreg(list(lm_tab4_1, lm_tab4_2),
          custom.coef.names = c("(Intercept)", "Extremity", "Race: White", "Recent Politican",
                                "Governor", "House", "Sec. of State", "Senate",
                                "Vice President"))

texreg(list(lm_tab4_1, lm_tab4_2),
          custom.coef.names = c("(Intercept)", "Extremity", "Race: White", "Recent Politican",
                                "Governor", "House", "Sec. of State", "Senate",
                                "Vice President"))

