# SI 1 Number of Recalled Politicians
# Figure SI 1.1: Number of politicians recalled in open-ended questions in Study 1
df_w1_complete |> 
  ungroup() |> 
  dplyr::select(ResponseId, party_name, politicians_open_count) |> 
  distinct() |> 
  ggplot(aes(x = politicians_open_count)) +
  geom_bar(aes(y = after_stat(count / sum(count)))) + theme_bw() +
  labs(title = "Number of politicians recalled in the open-ended questions (Study 1)",
       y = "Proportion",
       x = "Number of politicians recalled")

# Figure SI 1.2: Number of politicians recalled in open-ended questions in Study 2
df_er_open_w2 |> 
  ggplot(aes(x = politicians_open_count)) +
  geom_bar(aes(y = stat(count / sum(count)))) +
  #geom_bar() + 
  theme_bw() +
  labs(title = "Number of recalled politicians in the open-ended question (Study 2)",
       y = "Proportion", x = "Number of politicians recalled")

# Figure SI 1.3: Number of state and local politicians recalled in open-ended questions in Study 2
df_er_state_open |> 
  ggplot(aes(x = politicians_state_open_count)) +
  geom_bar(aes(y = stat(count / sum(count)))) +
  theme_bw() +
  labs(title = "Number of recalled state or local politicians in the open-ended question (Study 2)",
       y = "Proportion", x = "Number of state or local politicians recalled")

# Figure SI 1.4: Number of politicians recalled in close-ended questions in Study 2
df_er_closed_analysis |> 
  ggplot(aes(x = politicians_closed_count)) +
  geom_bar(aes(y = stat(count / sum(count)))) +
  theme_bw() +
  labs(title = "Number of recalled politicians in the close-ended question",
       y = "Proportion", x = "Number of politicians recalled")

# Table SI 1.1: Descriptive statistics of recall in Study 2

## ADD inline code here

# SI 2 Who is Recalled?
#Figure SI 2.1: Frequency of politicians that come to mind when people think about the parties in the open-ended questions in Study 1
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
  ggplot(aes(x = reorder(name_prop, proportion),y = proportion, color = recall_party)) +
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
  ggplot(aes(x = reorder(name_prop, proportion),y = proportion, color = recall_party)) +
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

## Table SI 2.2: Summary statistics of scaled ideology of recalled politicians

## Add the inline numbers here

## Figure SI 2.2: Figure SI 2.2: Frequency of politicians in the open-ended questions in Study 2
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


## Figure SI 2.3: Office of politicians named in open-ended responses in Study 1
df_w1_complete |> 
  ungroup() |> 
  drop_na(name) |> 
  drop_na(seat) |> 
  add_count(seat, name = "number")  |> 
  distinct() |> 
  ggplot(aes(x = reorder(seat, number))) + 
  geom_bar(aes(y = after_stat(count / sum(count)))) + 
  theme_bw() +
  labs(x = "Office",
       y = "Proportion") +
  coord_flip()

## Figure SI 2.4: Office of politicians named in open-ended responses in Study 2
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

# SI 3 Alternate Specifications
# Table SI 3.3: Statistical models
lm_5 <- lmer(number_inverse ~ recipient.cfscore.dyn_scaled_prop_comb + party_open_q + cand.gender + partisan + pol_interest_scaled + ideological_extremity_scaled + gender + (1|ResponseId), data=df_open_longform) 
lm_6 <- lmer(recipient.cfscore.dyn_scaled_prop_comb ~number_inverse+ party_open_q + cand.gender + partisan + pol_interest_scaled + ideological_extremity_scaled + gender + (1|ResponseId), data=df_open_longform) 

texreg(list(lm_5, lm_6),
       custom.model.names = c("Number, inverse, open", "Ideology, scaled"),
       custom.coef.names = c("(Intercept)", "Extremity", "Republican Politician",
                             "Male Politician", "Republican Respondent", "Pol. Interest, scaled",
                             "Extremity Respondent, scaled", "Male Respondent", "Non-binary Respondent",
                             "Recall Order, inverse"))
# SI 4 Perceived Extremity (Study 1)
# Table SI 4.4: Perceived Extremity of Recalled Politicians
