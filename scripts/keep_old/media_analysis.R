# Media Analysis 

# Libraries
library("tidyverse")
library("scales")
library("ggrepel")
library("patchwork")
library("estimatr")
library("texreg")
library("sjPlot")

# Option
options(scipen = 999)

# Data 
## Show data 
load("data/kim_et_al/showdatawithpols.RData")

show_people <-
  show.all %>% 
  dplyr::select(year, bonica.rid, screentime) %>% 
  group_by(year, bonica.rid) %>% 
  summarize(sum_screentime = sum(screentime),
            log_sum_screentime = log(sum_screentime)) %>% 
  distinct()

## DIME data 
# party: Party of candidate/recipient (100 = Dem, 200 = Rep, 328 = Ind)
df_dime <- 
  read_csv("data/cf_scores/dime_recipients_1979_2020.csv.gz") |> 
  dplyr::select(cycle, bonica.rid, seat, cand.gender, party,
                recipient.cfscore, recipient.cfscore.dyn) |> 
  #dplyr::select(cycle, bonica.rid, seat, ico.status, cand.gender, party) |> 
  rename(year = cycle) |> 
  filter(year >= 2010 ) %>% 
  filter(!str_detect(seat, "local")) %>% 
  arrange(year) %>% 
  group_by(bonica.rid) %>% 
  mutate(party = ifelse(party == 100, "Democratic",
                        ifelse(party == 200, "Republican",
                               ifelse(party == 328, "Independent", "Other"))),
         cand.gender = ifelse(cand.gender == "U", NA, cand.gender),
         party = ifelse(bonica.rid == "cand194302" & year == 2010,  "Democratic", party),
         party = ifelse(bonica.rid == "cand112923", "Republican", party),
         party = ifelse(bonica.rid == "cand57743", "Republican", party),
         party = ifelse(bonica.rid == "cand169776", "Republican", party),
         party = ifelse(bonica.rid == "cand121718", "Independent", party),
         party = ifelse(bonica.rid == "cand136065", "Independent", party),
         party = ifelse(bonica.rid == "cand99460" & year %in% c(2012, 2014), "Independent", party),
         party = ifelse(bonica.rid == "cand168198" & year > 2009,  "Democratic", party),
         cand.gender = ifelse(bonica.rid == "cand193805", "F", cand.gender),
         cand.gender = ifelse(bonica.rid == "cand94845", "M", cand.gender)) %>% 
  fill(cand.gender, .direction = "downup") %>% 
  fill(party, .direction = "downup") %>% 
  distinct() %>% 
  group_by(year, bonica.rid) %>% 
  mutate(id = row_number()) %>% 
  ungroup() %>% 
  pivot_wider(names_from = id,
              values_from = c(seat, recipient.cfscore.dyn),
              names_glue = "{.value}_{id}") %>% 
  group_by(year, bonica.rid) %>% 
  add_count() %>% 
  mutate(party = ifelse(n == 2 & lag(party) %in% c("Republican", "Democratic") &
                          party %in% c("Independent", "Other"), NA, party),
         party = ifelse(n == 2 & lead(party) %in% c("Republican", "Democratic") &
                          party %in% c("Independent", "Other"), NA, party)) %>% 
  dplyr::select(-n) %>% 
  fill(recipient.cfscore.dyn_1, .direction = "downup") %>% 
  fill(recipient.cfscore.dyn_2, .direction = "downup") %>% 
  fill(recipient.cfscore.dyn_3, .direction = "downup") %>% 
  fill(party, .direction = "downup") %>% 
  fill(seat_1, .direction = "downup") %>% 
  fill(seat_2, .direction = "downup") %>% 
  fill(seat_3, .direction = "downup") %>% 
  distinct()

df_dime_small <-
  df_dime |> 
  dplyr::select(bonica.rid, year, recipient.cfscore.dyn_1, party) |> 
  mutate(party = ifelse(party %in% c("Democratic", "Republican"), party, NA),
         party = as.factor(party))

## DIME people
df_dime_people <- 
  read_csv("data/cf_scores/dime_recipients_1979_2020.csv.gz") |> 
  dplyr::select(cycle, bonica.rid, cand.gender, party) |> 
  rename(year = cycle) |> 
  filter(year >= 2010) %>% 
  arrange(year) %>% 
  group_by(bonica.rid) %>% 
  mutate(party = ifelse(party == 100, "Democratic",
                        ifelse(party == 200, "Republican",
                               ifelse(party == 328, "Independent", "Other")))) %>% 
  fill(cand.gender, .direction = "downup") %>% 
  fill(party, .direction = "downup") %>% 
  distinct() 

table(df_dime_people$party)

#load("data/kim_et_al/dataverse_files/individual_shows.RData")
#load("data/kim_et_al/dataverse_files/screen_panel.RData")
#load("data/kim_et_al/dataverse_files/show_screen_panel.RData")
#load("data/politicians/showdatawithpols.RData")
#load("data/kim_et_al/dime_contrib_matches_with_info.RData")


# Combine show data with dime data
length(unique(show.all$bonica.rid))
length(unique(df_dime$bonica.rid))
table(show.all$year)

df_combined <- 
  show.all |> 
  left_join(df_dime, by = join_by(year, bonica.rid)) 






# Name of all politicians
show.all |> 
  dplyr::select(name) |> 
  distinct() |> 
  pull(name)


# Models
# 1. selection model
# 2. time ~ extreme

df_model_inclusion <-
  df_dime_people %>% 
  left_join(show_people) %>% 
  mutate(tv_appearance = ifelse(!is.na(sum_screentime), 1, 0)) %>% 
  left_join(df_dime, by = join_by(year, bonica.rid)) %>% 
  dplyr::select(-ends_with(".y")) %>% 
  rename(cand.gender = cand.gender.x,
         party = party.x) |> 
  mutate(party = as.factor(party))

table(df_model_inclusion$tv_appearance)

df_model_time <-
  show_people %>% 
  left_join(df_dime)  |> 
    mutate(party = as.factor(party))

part1 <- 
  df_model_inclusion %>% 
  mutate(tv_appearance = ifelse(tv_appearance == 1, "Yes", "No")) %>% 
  ggplot(aes(y = recipient.cfscore.dyn_1, x = tv_appearance)) +
  #geom_jitter(aes(color = party, alpha = 1/10)) + 
  geom_violin() +
  geom_boxplot(width=0.1) +
  #geom_boxplot() +
  theme_bw() +
  labs(y = "CF Score, dynamic", x = "")


part2 <-
  df_model_inclusion %>% 
  mutate(tv_appearance = ifelse(tv_appearance == 1, "Yes", "No")) %>% 
  filter(party %in% c("Republican", "Democratic", "Independent")) %>% 
  ggplot(aes(y = recipient.cfscore.dyn_1, x = tv_appearance)) +
  #geom_jitter(aes(color = party, alpha = 1/10)) + 
  geom_violin() +
  #geom_boxplot() +
  geom_boxplot(width=0.1) +
  theme_bw() + facet_wrap(~party) +
  labs(y = "CF Score, dynamic", x = "TV apperance in a year")



fig10 <- part1 + part2 + plot_layout(ncol = 1)
ggsave("figures/media_appearance.pdf", plot = fig10, width = 13, height = 6.75, units = "in")
ggsave("figures/media_appearance.png", plot = fig10, width = 13, height = 6.75, units = "in")

lm1 <- lm(tv_appearance ~ recipient.cfscore.dyn_1 + year , clusters = bonica.rid, data = df_model_inclusion)
lm2 <- lm_robust(log_sum_screentime ~ recipient.cfscore.dyn_1*party + cand.gender + year , clusters = bonica.rid, data = df_model_time)

screenreg(list(lm1, lm2))

fig11 <-
  plot_model(lm2, type = "int",colors = c("blue", "darkgray", "#f8766d")) + theme_bw() +
  labs(x = "CF Score, dynamic", y = "Hours, logged",
    title = "Predicted values of TV time",
      color = "Party") + theme(legend.position = "bottom") 
ggsave("figures/screentime.pdf", plot = fig11, width = 13, height = 6.75, units = "in")
ggsave("figures/screentime.png", plot = fig11, width = 13, height = 6.75, units = "in")
      

# Plot Figure 7
# Density plot of mentions
fig7 <-
  show.all |> 
  dplyr::select(name) |> 
  add_count(name) |> 
  distinct() |> 
  ggplot(aes(x = n)) +
  geom_density(fill = "lightblue", color = "darkblue") + theme_bw() +
  labs(x = "Frequency", y = "Density") 

ggsave("figures/media_pol_density.pdf", plot = fig7, width = 9.29, height = 5.75, units = "in")

# Plot Figure 8
# Frequency Plot of Names
fig8 <-
show.all |> 
  dplyr::select(name) |> 
  add_count(name) |>
  mutate(name = str_to_title(name),
         percent = n/n()*100) |> 
  distinct() |> 
  top_n(n = 50) |> 
  ggplot(aes(x = reorder(name, n), y = percent)) +
  geom_col() + coord_flip() + theme_bw() +
  labs(x = "", y = "Percentage of All Citations")

ggsave("figures/media_top_50.pdf", plot = fig8, width = 9.29, height = 9.29, units = "in")
rm(fig8)


# Time by name plot
fig9_1 <-
  show.all |> 
  dplyr::select(bonica.rid, name, screentime, cfscore, year) |> 
  group_by(bonica.rid) |> 
  mutate(sum_time = sum(screentime)/60,
         log_sum_time = log(sum_time),
         years_mentioned = n_distinct(year),
         name = str_to_title(name),
         name = str_replace_all(name, "Mccrory", "McCrory"),
         name = str_replace_all(name, "Mcconnell", "McConnell"),
         name = str_replace_all(name, "Mccarthy", "McCarthy")) |> 
  dplyr::select(-c(screentime, sum_time)) |> 
  separate(name, into = c("first", "last"), sep = " ", remove = FALSE) |> 
  unite("name_year", c(last, year), sep = " ", remove = FALSE) |> 
  ungroup() %>% 
  distinct() |> 
  left_join(df_dime_small, by = join_by(bonica.rid, year)) |>
  mutate(group = ifelse(recipient.cfscore.dyn_1 < 0, "left", "right")) |> 
  drop_na(party) |> 
  ggplot(aes(x = recipient.cfscore.dyn_1, y = log_sum_time, label = name_year, group = party,color = party)) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_point() + theme_bw() + geom_text_repel( max.overlaps = 12) +
  labs(y = "Hours, logged", x = "CF Score, dynamic")  + scale_color_manual(name="Party", 
labels = c("Democratic", "Republican"), 
values = c("Democratic"="blue", "Republican"="#f8766d")) + theme(legend.position = "none")

# Time percentage for top 20 by name plot
fig9_2 <-
  show.all |> 
  dplyr::select(bonica.rid, name, screentime, cfscore) |> 
  group_by(bonica.rid) |> 
  mutate(sum_time = sum(screentime)/60,
         name = str_to_title(name),
         name = str_replace_all(name, "Mccrory", "McCrory"),
         name = str_replace_all(name, "Mcconnell", "McConnell"),
         name = str_replace_all(name, "Mccain", "McCain"),
         name = str_replace_all(name, "Mccarthy", "McCarthy")) |> 
  ungroup() %>% 
  dplyr::select(-screentime) |> 
  distinct() %>% 
  mutate(percent = round(sum_time/sum(sum_time),3)*100,
         scaled_cfscore = rescale(cfscore, to = c(-1, 1))) %>% 
  dplyr::select(name, sum_time,percent, scaled_cfscore) %>% 
  arrange(-sum_time) %>% 
  top_n(sum_time, n = 20)  %>% 
  ggplot(aes(x = reorder(name, percent), y = percent)) +
  geom_col() + coord_flip() + theme_bw() +
  labs(x = "", y = "Percentage of Hours") +
  theme(legend.position = "none")


#Combined
fig7_combined <- fig7 + fig9_2 + plot_layout(widths = c(2, 1))
fig9 <- fig9_1 + fig9_2 + plot_layout(widths = c(2, 1))

ggsave("figures/media_pol_density_combined.pdf", plot = fig7_combined, width = 13, height = 6.75, units = "in")
ggsave("figures/total_time_single.pdf", plot = fig9_1, width = 14, height = 8, units = "in")
ggsave("figures/total_time_single.png", plot = fig9_1, width = 14, height = 8, units = "in")

ggsave("figures/total_time_combined.pdf", plot = fig9, width = 13, height = 6.75, units = "in")
ggsave("figures/total_time_combined.png", plot = fig9, width = 13, height = 6.75, units = "in")

rm(fig9, fig9_1, fig9_2, fig7, fig7_combined)





#--------------------------------------------------
# OLD CODE, don't yet drop


# Total time by year 
#check <- 
show.all |> 
  dplyr::select(bonica.rid, name, screentime, year, cfscore) |> 
  group_by(year) |> 
  mutate(scaled_cfscore = ifelse(cfscore < 0, abs(cfscore), cfscore),
         scaled_cfscore = scale(scaled_cfscore),
         name = str_to_title(name),
         name = str_replace_all(name, "Mccrory", "McCrory"),
         name = str_replace_all(name, "Mcconnell", "McConnell"),
         name = str_replace_all(name, "Mccarthy", "McCarthy")) |> 
  group_by(bonica.rid, year) |> 
  mutate(sum_time = sum(screentime)/60,
         log_sum_time = log(sum_time)) |> 
  unite("name_year", c(name, year), sep = " ", remove = F) |> 
  dplyr::select(-screentime) |> 
  distinct() |> 
  ggplot(aes(x = cfscore, y = log_sum_time, label = name_year)) +
  geom_point() + theme_bw() + geom_text_repel() +
  labs(y = "Hours", x = "CF Score")


## Less than 3000 hours 
show.all |> 
  dplyr::select(bonica.rid, name, screentime, year, cfscore) |> 
  group_by(year) |> 
  mutate(scaled_cfscore = ifelse(cfscore < 0, abs(cfscore), cfscore),
         scaled_cfscore = scale(scaled_cfscore)) |> 
  group_by(bonica.rid, year) |> 
  mutate(sum_time = sum(screentime)/60,
         name = str_to_title(name),
         name = str_replace_all(name, "Mccrory", "McCrory"),
         name = str_replace_all(name, "Mcconnell", "McConnell"),
         name = str_replace_all(name, "Mccarthy", "McCarthy")) |> 
  filter(sum_time < 3000) |> 
  unite("name_year", c(name, year), sep = " ", remove = F) |> 
  dplyr::select(-screentime) |> 
  distinct() |> 
  ggplot(aes(x = cfscore, y = sum_time, label = name_year)) +
  geom_point() + theme_bw() + geom_text_repel() +
  labs(y = "Hours", x = "CF Score") 

