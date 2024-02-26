# Study 1 files 

# Libraries
library("tidyverse")
library("rio")
library("ggpubr")
library("scales")
library("fixest")
library("sjPlot")
library("texreg")

# Loading the data
w1_state_politicians <- c("chris christie","jeb bush", "jerry brown", "jon huntsman", 
                          "michael bloomberg", "michael steele", "mitch daniels", "jack dorgan", 
                          "sarah palin", "nikki haley","rick perry", "rick scott", "ron cuccinelli",
                          "scott walker","terry mcauliffe","terry wilson", "wendy davis")

bill    <- c("bill clinton", "pres. clinton", "president clinton")
hillary <- c("hilary clinton", "hillary clinton", "hillary", "hilliary clinton", "hillary cliton", "hiliary clinton", "hillary rodham clinton", "hillary cllinton")
clinton <- c("clinton", "klinton", "clintony")
jeb     <- c("jeb bush","bush's brother")
bush <- c("president bush", "george bush", "bush")
gwb  <- c("w. bush", "president george bush (jr)", "president george bush (2nd one)", 
          "george wbush", "george w. bush", "george w bush", "george bush jr.", 
          "george bush jr", "george bush ii", "bush junior", "bush jr.", 
          "bush jr", "bush 2")
ghwb <- c("george hw bush", "george h.w. bush", "george h. w. bush", 
          "george h. bush", "george h w bush", "george bush, sr.", 
          "george bush, senior", "george bush sr.", "g hw bush", "bush sr", 
          "bush senior")


df_clean_names  <- import("data/turk/clean_names.dta") |> 
  dplyr::select(uniqid, dem_ex_mc, rep_ex_mc) |> 
  mutate(dem_ex_mc = ifelse(dem_ex_mc == "", NA, dem_ex_mc), 
         rep_ex_mc = ifelse(rep_ex_mc == "", NA, rep_ex_mc)) |> 
  filter(!is.na(dem_ex_mc) & !is.na(rep_ex_mc))


df_names_scores <- import("data/turk/names_scores.dta")
df_missing_names <- read_csv("data/politicians/wave1_politicians - wave1_names.csv") |> 
  drop_na(politician_as_entered) |> 
  dplyr::select(-recall_party) |> 
  distinct()

df_missing_cf <- read_csv("data/politicians/w1_missing_cf_scores.csv") |> 
  rename(cfscore_new = cfscore,
         recipient.cfscore.dyn_new = recipient.cfscore.dyn,
         dwnom1_new = dwnom1,
         dwnom2_new = dwnom2,
         seat_new = seat)

df_w1       <- import("data/turk/cleansurvey.dta") |> 
  rename(recipient.cfscore.dyn = cfscoresdyn, 
         ResponseId = uniqid) |> 
  mutate(party_name = ifelse(recall_party == "dem", "Democrat", 
                             ifelse(recall_party == "rep", "Republican", NA)))


df_w1_politician <- 
  df_w1 |> 
  dplyr::select(ResponseId, recall_party, recall_order, name, dem_ex_1:rep_ex_3, 
                recall_rating:recipient.cfscore.dyn, exec:party_name)

df_w1_survey <- 
  df_w1 |> 
  dplyr::select(-c(recall_party, recall_order, name, dem_ex_1:rep_ex_3, 
                   recall_rating:recipient.cfscore.dyn, exec:party_name)) |> 
  distinct()


df_matched_names <-
  df_w1 |> 
  dplyr::select(dem_ex_1:rep_ex_3) |> 
  distinct() |> 
  pivot_longer(cols = everything(),
               #cols = -c(ResponseId),
               names_to = "question",
               values_to = "politician_as_entered") |> 
  separate(question, into = c("party", "drop", "number")) |> 
  mutate(number = as.numeric(number),
         politician_as_entered = ifelse(politician_as_entered == "", NA, politician_as_entered),
         politician_as_entered = str_squish(politician_as_entered)) |>
  drop_na(politician_as_entered) |> 
  dplyr::select(politician_as_entered) |> 
  distinct() |> 
  left_join(df_missing_names) 



df_w1_complete <- 
  df_w1 |> 
  dplyr::select(ResponseId, dem_ex_1:rep_ex_3) |> 
  distinct() |> 
  pivot_longer(cols = -c(ResponseId),
               names_to = "question",
               values_to = "politician_as_entered") |> 
  separate(question, into = c("party", "drop", "number"))  |> 
  mutate(number = as.numeric(number),
         politician_as_entered = ifelse(politician_as_entered == "", NA, politician_as_entered),
         politician_as_entered = str_squish(politician_as_entered)) |> 
  left_join(df_matched_names) |> 
  rename(name = name_cleaned,
         recall_order = number,
         recall_party = party) |> 
  left_join(df_w1_politician) |> 
  dplyr::select(-c(dem_ex_1:rep_ex_3)) |> 
  group_by(recall_party, name) |> 
  fill(cfscore,dwnom1,dwnom2,recipient.cfscore.dyn, exec, gov, leg, legleader,
       .direction = "updown")  |> 
  group_by(name) |> 
  add_count(recall_party, name = "subtotal") |> 
  distinct() |> 
  add_count(name = "total") |> 
  mutate(name = ifelse(total > 1 & subtotal != max(subtotal), NA, name),
         name = ifelse(name == "harry truman" & recall_party == "rep", NA, name)) |> 
  dplyr::select(-c(subtotal, total)) |> 
  left_join(df_missing_cf) |> 
  ungroup() |> 
  mutate(party_name_new = ifelse(is.na(party_name) & recall_party == "dem", "Democrat",
                                 ifelse(is.na(party_name) & recall_party == "rep", "Republican", NA)),
         party_name = coalesce(party_name, party_name_new),
         cfscore = coalesce(cfscore, cfscore_new),
         dwnom1 = as.numeric(dwnom1),
         dwnom2 = as.numeric(dwnom2),
         dwnom1 = coalesce(dwnom1, dwnom1_new),
         dwnom2 = coalesce(dwnom2, dwnom2_new),
         recipient.cfscore.dyn = coalesce(recipient.cfscore.dyn, recipient.cfscore.dyn_new),
         number_inverse = ((recall_order -4)*-1)/3) |> 
  dplyr::select(-c(ends_with("_new"), drop)) |> 
  left_join(df_w1_survey) |> 
  group_by(ResponseId, recall_party) |> 
  mutate(politicians_open_count = sum(!is.na(name)),
         state_politician = name %in% w1_state_politicians) |> 
  mutate(name_extreme = case_when(
    politician_as_entered %in% clinton ~ "hillary clinton",
    politician_as_entered %in% bill ~ "bill clinton",
    politician_as_entered %in% hillary ~ "hillary clinton",
    .default = name),
    name_moderate = case_when(
      politician_as_entered %in% clinton ~ "bill clinton",
      politician_as_entered %in% bill ~ "bill clinton",
      politician_as_entered %in% hillary ~ "hillary clinton",
      .default = name),
    name_prop = case_when(
      politician_as_entered %in% clinton ~ "clinton ambiguous",
      politician_as_entered %in% bill ~ "bill clinton",
      politician_as_entered %in% hillary ~ "hillary clinton",
    ),
    clinton_assign = ifelse(name_prop == "clinton ambiguous", sample(c("bill", "hillary"), prob = c(0.485, 0.515)), NA),
    name_prop = case_when(
      clinton_assign == "bill" ~ "bill clinton",
      clinton_assign == "hillary" ~ "hillary clinton",
      .default = name_prop),
    name = ifelse(name %in% jeb, "jeb bush", name),
    cfscore = ifelse(name %in% jeb, 0.919, cfscore),
    recipient.cfscore.dyn = ifelse(name %in% jeb, 0.948, recipient.cfscore.dyn),
    name_extreme = case_when(
      politician_as_entered %in% bush ~ "george h. w. bush",
      politician_as_entered %in% ghwb ~ "george h. w. bush",
      politician_as_entered %in% gwb ~ "george w bush",
      .default = name),
    name_moderate = case_when(
      politician_as_entered %in% bush ~ "george w bush",
      politician_as_entered %in% ghwb ~ "george h. w. bush",
      politician_as_entered %in% gwb ~ "george w bush",
      .default = name),
    name_prop = case_when(
      politician_as_entered %in% bush ~ "bush ambiguous",
      politician_as_entered %in% ghwb ~ "george h. w. bush",
      politician_as_entered %in% gwb ~ "george w bush",
      .default = name),
    bush_assign = ifelse(name_prop == "bush ambiguous", sample(c("w", "hw"), prob = c(0.758, 0.242)), NA),
    name_prop = case_when(
      bush_assign == "w" ~ "george w bush",
      bush_assign == "hw" ~ "george h. w. bush",
      .default = name_prop),
    cfscore_prob = case_when(
      name_prop == "bill clinton" ~ -0.8249882,
      name_prop == "hillary clinton" ~ -1.1018933,
      name_prop == "george w bush" ~ 0.9920000,
      name_prop == "george h. w. bush" ~ 0.9224605,
      .default = cfscore),
    cfscore_moderate = case_when(
      name_prop == "bill clinton" ~ -0.8249882,
      name_prop == "hillary clinton" ~ -1.1018933,
      name_prop == "george w bush" ~ 0.9920000,
      name_prop == "george h. w. bush" ~ 0.9224605,
      .default = cfscore),
    cfscore_extreme = case_when(
      name_prop == "bill clinton" ~ -0.8249882,
      name_prop == "hillary clinton" ~ -1.1018933,
      name_prop == "george w bush" ~ 0.9920000,
      name_prop == "george h. w. bush" ~ 0.9224605,
      .default = cfscore),
    recipient.cfscore.dyn_prob = case_when(
      name_prop == "bill clinton" ~ -0.6689523,
      name_prop == "hillary clinton" ~ -0.9435650,
      name_prop == "george w bush" ~ 1.3020000,
      name_prop == "george h. w. bush" ~ 0.8344688,
      .default = recipient.cfscore.dyn),
    recipient.cfscore.dyn_moderate = case_when(
      name_prop == "bill clinton" ~ -0.6689523,
      name_prop == "hillary clinton" ~ -0.9435650,
      name_prop == "george w bush" ~ 1.3020000,
      name_prop == "george h. w. bush" ~ 0.8344688,
      .default = recipient.cfscore.dyn),
    recipient.cfscore.dyn_extreme = case_when(
      name_prop == "bill clinton" ~ -0.6689523,
      name_prop == "hillary clinton" ~ -0.9435650,
      name_prop == "george w bush" ~ 1.3020000,
      name_prop == "george h. w. bush" ~ 0.8344688,
      .default = recipient.cfscore.dyn)) |> 
  ungroup() |> 
  mutate(recipient.cfscore.dyn_scaled1 = ifelse(party_name == "Democrat", recipient.cfscore.dyn, NA),
         recipient.cfscore.dyn_scaled2 = ifelse(party_name == "Republican", recipient.cfscore.dyn, NA),
         recipient.cfscore.dyn_scaled_comb = coalesce(abs(recipient.cfscore.dyn_scaled1),recipient.cfscore.dyn_scaled2),
         recipient.cfscore.dyn_scaled_comb = rescale(recipient.cfscore.dyn_scaled_comb),
         recipient.cfscore.dyn_scaled1 = rescale(abs(recipient.cfscore.dyn_scaled1)),
         recipient.cfscore.dyn_scaled2 = rescale(recipient.cfscore.dyn_scaled2),
         recipient.cfscore.dyn_scaled = coalesce(recipient.cfscore.dyn_scaled1,recipient.cfscore.dyn_scaled2),
         recipient.cfscore.dyn_scaled_prop_1 = ifelse(party_name == "Democrat", recipient.cfscore.dyn_prob, NA),
         recipient.cfscore.dyn_scaled_prop_2 = ifelse(party_name == "Republican", recipient.cfscore.dyn_prob, NA),
         recipient.cfscore.dyn_scaled_moderate_1 = ifelse(party_name == "Democrat", recipient.cfscore.dyn_extreme, NA),
         recipient.cfscore.dyn_scaled_moderate_2 = ifelse(party_name == "Republican", recipient.cfscore.dyn_extreme, NA),
         recipient.cfscore.dyn_scaled_extreme_1 = ifelse(party_name == "Democrat", recipient.cfscore.dyn_moderate, NA),
         recipient.cfscore.dyn_scaled_extreme_2 = ifelse(party_name == "Republican", recipient.cfscore.dyn_moderate, NA),
         recipient.cfscore.dyn_scaled_prop_comb = coalesce(abs(recipient.cfscore.dyn_scaled_prop_1),recipient.cfscore.dyn_scaled_prop_2),
         recipient.cfscore.dyn_scaled_prop_comb = rescale(recipient.cfscore.dyn_scaled_prop_comb),
         recipient.cfscore.dyn_scaled_extreme_comb = coalesce(abs(recipient.cfscore.dyn_scaled_extreme_1),recipient.cfscore.dyn_scaled_extreme_2),
         recipient.cfscore.dyn_scaled_extreme_comb = rescale(recipient.cfscore.dyn_scaled_extreme_comb),
         recipient.cfscore.dyn_scaled_moderate_comb = coalesce(abs(recipient.cfscore.dyn_scaled_moderate_1),recipient.cfscore.dyn_scaled_moderate_2),
         recipient.cfscore.dyn_scaled_moderate_comb = rescale(recipient.cfscore.dyn_scaled_moderate_comb)) |> 
  dplyr::select(-c(recipient.cfscore.dyn_scaled1,recipient.cfscore.dyn_scaled2, 
                   recipient.cfscore.dyn_scaled_prop_1,recipient.cfscore.dyn_scaled_prop_2,
                   recipient.cfscore.dyn_scaled_moderate_1, recipient.cfscore.dyn_scaled_moderate_2,
                   recipient.cfscore.dyn_scaled_extreme_1, recipient.cfscore.dyn_scaled_extreme_2,
                   bush_assign, clinton_assign))

# Clean up
rm(df_w1, df_w1_politician, df_w1_survey,df_missing_cf,
   df_matched_names, df_missing_names, df_names_scores, df_clean_names, w1_state_politicians,
   bill, hillary, clinton, jeb, bush, ghwb, gwb)


## Number of politicians that came to mind
df_w1_complete |> 
  ungroup() |> 
  dplyr::select(name_prop, recall_party) |> 
  group_by(recall_party) |>
  add_count() |> 
  group_by(name_prop,recall_party) |> 
  add_count(name = "recalled_frequency") |> 
  mutate(proportion = recalled_frequency/n) |> 
  arrange(-recalled_frequency) |> 
  distinct() |> 
  add_tally(recalled_frequency) 

# top 10 democrats
df_w1_complete |> 
  ungroup() |> 
  filter(recall_party == "dem") |> 
  dplyr::select(name_prop, recall_party) |> 
  group_by(recall_party) |>
  add_count() |> 
  group_by(name_prop,recall_party) |> 
  add_count(name = "recalled_frequency") |> 
  mutate(proportion = recalled_frequency/n) |> 
  arrange(-recalled_frequency) |> 
  distinct() |> 
  add_tally(recalled_frequency) 

# top 10 republicans
df_w1_complete |> 
  ungroup() |> 
  filter(recall_party == "rep") |>
  dplyr::select(name_prop, recall_party) |> 
  group_by(recall_party) |>
  add_count() |> 
  group_by(name_prop,recall_party) |> 
  add_count(name = "recalled_frequency") |> 
  mutate(proportion = recalled_frequency/n) |> 
  arrange(-recalled_frequency) |> 
  distinct() |> 
  add_tally(recalled_frequency) 


df_w1_complete |> 
  dplyr::select(ResponseId, recall_party, recall_order, name) |> 
  group_by(ResponseId, recall_party) |> 
  add_count(name = "recalled_number") |> 
  dplyr::select(ResponseId, recall_party,recalled_number) |> 
  distinct() |> 
  arrange(ResponseId) |> 
  ungroup() |> 
  add_tally(recalled_number, name = "recalled_total") |> 
  group_by(recalled_number) |> 
  add_count(name = "recalled_categories") |> 
  dplyr::select(recalled_categories, recalled_total) |> 
  distinct() |> 
  ungroup() |> 
  mutate(recalled_proportion = recalled_categories/sum(recalled_categories)) 

df_w1_complete |> 
  ungroup() |> 
  dplyr::select(ResponseId, party_name, politicians_open_count) |> 
  distinct() |> 
  dplyr::select(party_name, politicians_open_count) |> 
  group_by(party_name, politicians_open_count) |> 
  add_count() |> 
  distinct()





## Figure 1 in text
fig_1a_wave1 <-
  df_w1_complete |> 
  ungroup() |> 
  dplyr::select(name_prop, recall_party) |> 
  drop_na(name_prop) |> 
  add_count(name_prop) |> 
  distinct() |> 
  filter(recall_party == "dem") |> 
  mutate(proportion = n/sum(n),
         name_prop = str_to_title(name_prop),
         recall_party = ifelse(recall_party == "dem", "Democrat", 
                               ifelse(recall_party == "rep", "Republican", NA))) |> 
  arrange(-n) |> 
  slice_head(n = 20) |> 
  #ggplot(aes(x = reorder(politican_clean, n),fill = party_name)) +
  ggplot(aes(x = reorder(name_prop, proportion),y = proportion, color = recall_party)) +
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

fig_1b_wave1 <-
  df_w1_complete |> 
  ungroup() |> 
  dplyr::select(name_prop, recall_party) |> 
  drop_na(name_prop) |> 
  add_count(name_prop) |>
  distinct() |> 
  filter(recall_party == "rep") |> 
  mutate(proportion = n/sum(n),
         name_prop = str_to_title(name_prop),
         recall_party = ifelse(recall_party == "dem", "Democrat", 
                               ifelse(recall_party == "rep", "Republican", NA))) |> 
  arrange(-n) |> 
  slice_head(n = 20) |> 
  #ggplot(aes(x = reorder(politican_clean, n),fill = party_name)) +
  ggplot(aes(x = reorder(name_prop, proportion),y = proportion, color = recall_party)) +
  #geom_bar(aes(y = stat(count / sum(count)))) +
  geom_point() +
  coord_flip() +
  labs(title = "Republican Party",
       x = "",
       y = "Proportion") + 
  scale_color_manual(name="Party", 
                     labels = c("Democrat", "Republican"), 
                     values = c("Democrat"="blue", "Republican"="#f8766d")) +
  theme_bw() + 
  theme(legend.position = "none") +
  ylim(0,0.4)

ggarrange(fig_1a_wave1, fig_1b_wave1, labels = c("A", "B"), ncol = 2, nrow = 1, widths = c(0.5, 0.5))
# saved as wave1_politicians_dotplot_byparty 1815 × 921 pixels

## Figure 2
## Calculating the ideology mean of the survey
mean(df_w1_complete$recipient.cfscore.dyn_scaled_prop_comb[df_w1_complete$party_name == 'Democrat'], na.rm = TRUE)
mean(df_w1_complete$recipient.cfscore.dyn_scaled_prop_comb[df_w1_complete$party_name == 'Republican'], na.rm = TRUE)


ggplot(df_w1_complete, aes(x = recipient.cfscore.dyn_scaled_prop_comb)) +
  #geom_histogram(aes(x = recipient.cfscore.dyn_scaled_comb, fill = party_open_q, alpha = 0.8)) + 
  geom_histogram(data=subset(df_w1_complete, party_name == 'Democrat'),fill = "blue", alpha = 0.5) +
  geom_histogram(data=subset(df_w1_complete, party_name == 'Republican'),fill = "red", alpha = 0.5) +
  geom_vline(xintercept = 0.49, linetype = "dashed", color = "blue") +
  geom_vline(xintercept = 0.53, linetype = "dashed", color = "red") +
  # geom_histogram(aes(x = dwnom2), fill = "blue") + 
  theme_bw() +
  labs(#title = "CF Score Distribution",
    # subtitle = "Political parties are scaled on party-specific min and max",
    x = "Extremity", y = "Count", fill = "Party")  +
  theme(legend.position = "none")
# w1_scaled_extremity_distribution 2994 × 1869 pixels

## Figure 2
## Calculating the ideology mean of Congress for 2013
mean(df_w1_complete$recipient.cfscore.dyn_scaled_prop_comb[df_w1_complete$party_name == 'Democrat'], na.rm = TRUE)
mean(df_w1_complete$recipient.cfscore.dyn_scaled_prop_comb[df_w1_complete$party_name == 'Republican'], na.rm = TRUE)

check <- 
  df_w1_complete |> 
  dplyr::select(name, name_prop,party_name, recipient.cfscore.dyn, recipient.cfscore.dyn_scaled_comb)

ggplot(df_w1_complete, aes(x = recipient.cfscore.dyn_scaled_prop_comb)) +
  #geom_histogram(aes(x = recipient.cfscore.dyn_scaled_comb, fill = party_open_q, alpha = 0.8)) + 
  geom_histogram(data=subset(df_w1_complete, party_name == 'Democrat'),fill = "blue", alpha = 0.5) +
  geom_histogram(data=subset(df_w1_complete, party_name == 'Republican'),fill = "red", alpha = 0.5) +
  geom_vline(xintercept = 0.47, linetype = "dashed", color = "blue") +
  geom_vline(xintercept = 0.67, linetype = "solid", color = "gray50") +
  geom_vline(xintercept = 0.67, linetype = "solid", color = "gray50") +
  geom_vline(xintercept = 0.7, linetype = "dashed", color = "red") +
  # geom_histogram(aes(x = dwnom2), fill = "blue") + 
  theme_bw() +
  labs(#title = "CF Score Distribution",
    # subtitle = "Political parties are scaled on party-specific min and max",
    x = "Extremity", y = "Count", fill = "Party")  +
  theme(legend.position = "none")


## Table 1
lm_1_1 <- feols(recipient.cfscore.dyn_scaled_prop_comb ~ number_inverse, data = df_w1_complete, se = "cluster", cluster = "ResponseId")
lm_1_2 <- feols(recipient.cfscore.dyn_scaled_prop_comb ~ number_inverse + party_name*number_inverse, data = df_w1_complete, se = "cluster", cluster = "ResponseId")


screenreg(list(lm_1_1, lm_1_2))
texreg(list(lm_1_1, lm_1_2))

plot_model(lm_1_2, type = "int") + 
  theme_bw() +
  labs(title = "Ideological recall by partisans",
       x = "Political Party in survey question",
       y = "CF Score, rescaled",
       color = 'Partisanship of respondent') +
  scale_color_manual(values = c("blue", "red")) +
  theme(legend.position = "bottom")


## Closed ended analysis

df_closed_w1  <- import("data/turk/clean_names.dta") |> 
  dplyr::select(uniqid, dem_ex_mc, rep_ex_mc) |> 
  mutate(dem_ex_mc = ifelse(dem_ex_mc == "", NA, dem_ex_mc), 
         rep_ex_mc = ifelse(rep_ex_mc == "", NA, rep_ex_mc)) |> 
  filter(!is.na(dem_ex_mc) & !is.na(rep_ex_mc)) |> 
  rename(ResponseId = uniqid) |> 
  pivot_longer(cols= -ResponseId,
               names_to = "party",
               values_to = "politician") |> 
  mutate(selected = 1)


df_w1_ids <- 
  df_closed_w1 |> 
  dplyr::select(ResponseId) |> 
  distinct()

df_closed_merge <- read_csv("data/turk/politicians_closed_ended.csv") |> 
  rename(politician = name_of_politician)

df_closed_w1_pols <-
  enframe(c("John F. Kennedy", "George W. Bush", "Barack Obama", 
            "John McCain", "Harry Reid", "Ronald Reagan", 
            "Bill Clinton", "Sarah Palin", "Hillary Clinton", 
            "Nancy Pelosi", "Mitt Romney", "Ted Kennedy", 
            "Ted Cruz", "Rand Paul", "Chris Christie", 
            "Franklin D. Roosevelt", "Jeb Bush", "John Kerry", 
            "Mitch McConnell", "Paul Ryan", "Joe Biden", 
            "Michele Bachmann", "Marco Rubio", "Eric Cantor", 
            "John Boehner", "Barney Frank", "Al Gore")) |> 
  dplyr::select(-name) |> 
  rename(politician = value)


df_closed_longformat_w1 <- 
  crossing(df_w1_ids, df_closed_w1_pols) |> 
  left_join(df_closed_w1) |> 
  group_by(politician) |> 
  fill(party, .direction = "updown") |> 
  mutate(party = ifelse(is.na(party), "dem_ex_mc", party),
         party = str_remove_all(party, "_ex_mc"),
         selected = ifelse(is.na(selected), 0, selected)) |> 
  left_join(df_closed_merge) |> 
  mutate(most_recent_office_f = as.factor(position),
         most_recent_office_f = relevel(most_recent_office_f, ref = "President"))


names(df_closed_longformat_w1)

## Longform analysis 
library(lme4)
lm_1 <- lm(selected ~ 1, data=df_closed_longformat_w1)
screenreg(lm_1)

lm_1 <- lmer(selected ~ recipient.cfscore.dyn_scaled_comb + (1|ResponseId), data=df_closed_longformat_w1)
lm_2 <- lmer(selected ~ recipient.cfscore.dyn_scaled_comb + race + gender + most_recent_office_f + (1|ResponseId), data=df_closed_longformat_w1)
lm_3 <- lmer(selected ~ recent + race + gender + most_recent_office_f + (1|ResponseId), data=df_closed_longformat_w1)


screenreg(list(lm_1, lm_2, lm_3),
          custom.coef.names = c("(Intercept)", "Extremity", "Race: White", "Gender: Male",
                                "Vice President", "House", "Governor", "Senate", "Contemporaneous"))


df_int <-
  df_w1_complete |> 
  dplyr::select(ResponseId, recipient.cfscore.dyn_scaled_prop_comb, recall_party, pship) |> 
  mutate(recall_party = ifelse(recall_party == "dem", "Democratic", "Republican"),
         recall_party_f = as.factor(recall_party),
         pship = ifelse(pship %in% c("Democrat", "Republican"), pship, NA),
         pship_f = as.factor(pship))

lm_2_6 <- feols(recipient.cfscore.dyn_scaled_prop_comb ~ recall_party_f*pship_f, data = df_int, se = "cluster", cluster = "ResponseId")


plot_model(lm_2_6, type = "int") + 
  theme_bw() +
  labs(title = "",
       x = "Political party of the recalled politician",
       y = "Extremity",
       color = 'Partisanship of respondent') +
  scale_color_manual(values = c("blue", "red")) +
  theme(legend.position = "bottom")
# interaction_party_partisanship_comb_w1 825x825 


## Inline numbers
df_w1_complete |> 
  ungroup() |> 
  dplyr::select(ResponseId, party_name, politicians_open_count) |> 
  distinct() |> 
  dplyr::select(politicians_open_count) |> 
  group_by(politicians_open_count) |> 
  add_count() |> 
  distinct() |> 
  ungroup() |> 
  mutate(poportion = n/sum(n))

df_w1_complete |> 
  ungroup() |> 
  filter(recall_party == "dem") |> 
  dplyr::select(name) |> 
  add_count() |> 
  group_by(name) |> 
  add_count(name = "recalled_frequency") |> 
  mutate(proportion = recalled_frequency/n) |> 
  arrange(-recalled_frequency) |> 
  distinct() |> 
  add_tally(recalled_frequency) |> 
  ungroup() |> 
  add_tally(recalled_frequency) |> 
  print(n = 15)



# Number of three politicians
df_w1_complete |> 
  dplyr::select(ResponseId, politicians_open_count) |> 
  distinct() |> 
  add_count(politicians_open_count) |> 
  dplyr::select(politicians_open_count, n) |> 
  distinct()

328/(328+55+9+2)
# 0.8324873