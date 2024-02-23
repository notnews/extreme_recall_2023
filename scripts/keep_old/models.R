# Working directory
setwd(githubdir)
setwd("extreme_recall2")

# Libraries
library(tidyverse)
library(readxl)
library(texreg)
#library(estimatr)
library(fixest)
library(sjPlot)
library(scales)

# Source the data file
source("scripts/survey_df_clean.R")


## What we want to estimate 
## 1. average num of recall 
##    - split by open vs closed (how much was vol., how much came after probe)
##    - skew in responses (mean,median)
##    - how many were not able to vol. anyone
##    - compare to prop of people that were able to vol. state/local politicians in second open q. 
## TODO clean the state politicians: we need to drop presidents etc 

df_er_open <-
  df_er_open |> 
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
  

check <-
  df_er_open |> 
  dplyr::select(politican_clean, party_open_q, party_name, recipient.cfscore.dyn, recipient.cfscore.dyn_scaled) |> 
  drop_na() |> 
  distinct()

df_er_open_analysis <-
  df_er_open |> 
  dplyr::select(ResponseId, politicians_open_count) |> 
  distinct()

df_er_state_open_analysis <-
  df_er_state_open |> 
  dplyr::select(ResponseId, politicians_state_open_count) |> 
  distinct()


df_er_closed_analysis <-
  df_er_closed |> 
  dplyr::select(ResponseId, politicians_closed_count) |> 
  distinct() 

## Figure: Politicians recalled by frequency
df_er_open |> 
  ungroup() |> 
  dplyr::select(politican_clean, party_name) |> 
  drop_na(politican_clean) |> 
  add_count(politican_clean) |> 
  distinct() |> 
  ggplot(aes(x = reorder(politican_clean, n), y = n,color = party_name)) +
  geom_point() +
  coord_flip() +
  labs(title = "Frequency of politicians named in open ended question",
       x = "Politician",
       y = "Count",
       #caption = "Question: When you think about the Democratic/Republican Party, which political leader(s) first come to mind? Name up to three ..."
  ) + scale_color_manual(name="Party", 
                         labels = c("Democrat", "Republican"), 
                         values = c("Democrat"="blue", "Republican"="#f8766d")) +
  theme_bw() + 
  theme(legend.position = "none")

 
df_er_open |> 
  ungroup() |> 
  dplyr::select(politican_clean, party_name) |> 
  drop_na(politican_clean) |> 
  add_count(politican_clean) |> 
  distinct() |> 
  filter(n > 19) |> 
  ggplot(aes(x = reorder(politican_clean, n), y = n,color = party_name)) +
  geom_point() +
  coord_flip() +
  labs(title = "Frequency of politicians named in open ended question",
       subtitle = "Minimum count is 20",
       x = "Politician",
       y = "Count",
       #caption = "Question: When you think about the Democratic/Republican Party, which political leader(s) first come to mind? Name up to three ..."
  ) + scale_color_manual(name="Party", 
                         labels = c("Democrat", "Republican"), 
                         values = c("Democrat"="blue", "Republican"="#f8766d")) +
  theme_bw() + 
  theme(legend.position = "none")


df_er_open_analysis |> 
  ggplot(aes(x = politicians_open_count)) +
  geom_bar(aes(y = stat(count / sum(count)))) +
  #geom_bar() + 
  theme_bw() +
  labs(title = "Number of recalled politicians in the open-ended question",
       y = "Proportion", x = "Number of politicians recalled")

df_er_state_open_analysis |> 
  ggplot(aes(x = politicians_state_open_count)) +
  geom_bar(aes(y = stat(count / sum(count)))) +
  theme_bw() +
  labs(title = "Number of recalled state or local politicians in the open-ended question",
       y = "Proportion", x = "Number of state or local politicians recalled")

df_er_closed_analysis |> 
  ggplot(aes(x = politicians_closed_count)) +
  geom_bar(aes(y = stat(count / sum(count)))) +
  theme_bw() +
  labs(title = "Number of recalled politicians in the close-ended question",
       y = "Proportion", x = "Number of politicians recalled")


lm_1_0 <- lm(politicians_open_count ~ 1, data = df_er_open_analysis)
lm_1_1 <- lm(politicians_closed_count ~ 1, data = df_er_closed_analysis)

screenreg(list(lm_1_0, lm_1_1),
          custom.header = list("Mean Number recalled"= 1:2),
          custom.model.names = c("Open-ended", "Close-ended"),
          omit.coef = c("ResponseId"))


mean(df_er_open_analysis$politicians_open_count)
median(df_er_open_analysis$politicians_open_count)
sd(df_er_open_analysis$politicians_open_count)
length(df_er_open_analysis$politicians_open_count)

mean(df_er_closed_analysis$politicians_closed_count)
median(df_er_closed_analysis$politicians_closed_count)
sd(df_er_closed_analysis$politicians_closed_count)
length(df_er_closed_analysis$politicians_closed_count)

mean(df_er_state_open_analysis$politicians_state_open_count)
median(df_er_state_open_analysis$politicians_state_open_count)
sd(df_er_state_open_analysis$politicians_state_open_count)
length(df_er_state_open_analysis$politicians_state_open_count)

table(df_er_open_analysis$politicians_open_count[df_er_open_analysis$politicians_open_count == 0])
table(df_er_closed_analysis$politicians_closed_count[df_er_closed_analysis$politicians_closed_count == 0])

## 2. ideological dist of recalled 
##    - histogram of ideological results (dem, reps), add number in text in the graph, top XX contribute to YY percent etc 
##    - mean and how does the mean compare to congress, congress + pres, congress + pres + govern
##    - ideological bias in recall order (more extreme recalled earlier?, restricted to open)
##        - scale reps and dems with regard to most moderate dem or rep, zero is most moderate, 1 most extreme
## 3. inparty and outparty bias of recall 
##    - are more extreme pols recalled for outparty politicians 
##    - are in or outparty recalled more frequently 

df_er_open |> 
  ggplot(aes()) +
  geom_histogram(aes(x = recipient.cfscore.dyn, fill = party_open_q)) + 
  theme_bw() +
  scale_fill_manual(values = c("blue", "red")) +
  labs(x = "Ideology Score for all open responses", y = "Count", fill = "Party")  +
  theme(legend.position = "none")


df_er_open |> 
  ggplot(aes()) +
  geom_histogram(aes(x = recipient.cfscore.dyn, fill = party_open_q)) + 
  # geom_histogram(aes(x = dwnom2), fill = "blue") + 
  theme_bw() +
  scale_fill_manual(values = c("blue", "red")) +
  labs(x = "Ideology Score by recall order", y = "Count", fill = "Party")  +
  theme(legend.position = "none") +
  facet_wrap(~number)

df_er_open |> 
  ggplot(aes()) +
  #geom_histogram(aes(x = recipient.cfscore.dyn, fill = as.factor(number))) + 
  geom_density(aes(x = recipient.cfscore.dyn, color = as.factor(number))) + 
  # geom_histogram(aes(x = dwnom2), fill = "blue") + 
  theme_bw() +
  scale_color_manual(values = c("gray22", "gray44", "gray66")) +
  scale_fill_manual(values = c("gray22", "gray44", "gray66")) +
  labs(x = "Ideology Score by recall order", y = "Count", fill = "Recall order")  +
  theme(legend.position = "bottom") 

df_er_open |> 
  ggplot(aes()) +
  geom_histogram(aes(x = recipient.cfscore.dyn)) + 
  geom_histogram(aes(x = recipient.cfscore.dyn_scaled), fill = "gray66") + 
  theme_bw() + facet_wrap(~party_open_q) +
  labs(x = "Ideology Score") 





ggplot(df_er_open, aes(x = recipient.cfscore.dyn_scaled_prop_comb)) +
  #geom_histogram(aes(x = recipient.cfscore.dyn_scaled_comb, fill = party_open_q, alpha = 0.8)) + 
  geom_histogram(data=subset(df_er_open,party_open_q == 'Democrat'),fill = "blue", alpha = 0.5) +
  geom_histogram(data=subset(df_er_open,party_open_q == 'Republican'),fill = "red", alpha = 0.5) +
  # geom_histogram(aes(x = dwnom2), fill = "blue") + 
  theme_bw()+
  labs(#title = "CF Score Distribution",
       #subtitle = "Political parties are scaled on shared min and max",
       x = "Extremity", y = "Count", fill = "Party")  +
  theme(legend.position = "none") +
  geom_vline(xintercept = 0.53, linetype = "dashed", color = "blue") +
  geom_vline(xintercept = 0.64, linetype = "dashed", color = "red") 
  


df_er_open <- 
  df_er_open |> 
  mutate(congress_mean = 0.14,
         senate_mean = 0.6)

lm_2_0  <- feols(recipient.cfscore.dyn ~ 1, data = df_er_open, se = "cluster", cluster = "ResponseId")
lm_2_01 <- feols(recipient.cfscore.dyn ~ -1 + congress_mean, data = df_er_open, se = "cluster", cluster = "ResponseId")
lm_2_02 <- feols(recipient.cfscore.dyn ~ -1 + senate_mean, data = df_er_open, se = "cluster", cluster = "ResponseId")

screenreg(list(lm_2_0, lm_2_01, lm_2_02),
          custom.header = list("Average Ideological Score of recalled Politicians"= 1:3),
          custom.model.names = c("Mean CF Score", "Difference to Congress mean", "Difference to Senate mean"),
          omit.coef = c("ResponseId"))

screenreg(list(lm_2_01),
          custom.header = list("Average Ideological Score of recalled Politicians"= 1),
          custom.model.names = c("Difference to Congress mean"),
          omit.coef = c("ResponseId"))

summary(lm_2_01)                                 
                                 
t.test(df_er_open$recipient.cfscore.dyn, mu = 0.14)

lm_2_0 <- feols(recipient.cfscore.dyn_scaled_prop_comb ~ 1, data = df_er_open, se = "cluster", cluster = "ResponseId")
lm_2_1 <- feols(recipient.cfscore.dyn_scaled_prop_comb ~ number_inverse, data = df_er_open, se = "cluster", cluster = "ResponseId")
lm_2_2 <- feols(recipient.cfscore.dyn_scaled_prop_comb ~ number_inverse*party_open_q, data = df_er_open, se = "cluster", cluster = "ResponseId")

screenreg(list(lm_2_1, lm_2_2),
          custom.coef.names = c("(Intercept)", "Recall Order", "Republican", "Recall Order*Republican"),
          custom.model.names = c("Ideological Extremity", "Ideological Extremity"))

texreg(list(lm_2_1, lm_2_2),
          custom.coef.names = c("(Intercept)", "Recall Order", "Republican", "Recall Order*Republican"),
          custom.model.names = c("Ideological Extremity", "Ideological Extremity"))

plot_model(lm_2_2, type = "int") +
  theme_bw() +
  labs(title = "Predicted values of Extremity",
       x = "Recall Order",
       y = "Extremity",
       color = "Party") +
  theme(legend.position = "bottom") +
  scale_color_manual(values = c("blue", "red"))+
  scale_fill_manual(values = c("blue", "red"))

lm_2_2 <- feols(recipient.cfscore.dyn_scaled ~ 1, data = df_er_open, se = "cluster", cluster = "ResponseId")
lm_2_3 <- feols(recipient.cfscore.dyn_scaled ~ number, data = df_er_open, se = "cluster", cluster = "ResponseId")
lm_2_4 <- feols(recipient.cfscore.dyn_scaled_comb ~ 1, data = df_er_open, se = "cluster", cluster = "ResponseId")
lm_2_5 <- feols(recipient.cfscore.dyn_scaled_comb ~ number, data = df_er_open, se = "cluster", cluster = "ResponseId")



lm_2_6 <- feols(recipient.cfscore.dyn_scaled ~ partisan_type, data = df_er_open, se = "cluster", cluster = "ResponseId")
lm_2_7 <- feols(recipient.cfscore.dyn_scaled ~ -1 + partisan_type, data = df_er_open, se = "cluster", cluster = "ResponseId")

lm_2_6 <- feols(recipient.cfscore.dyn_scaled ~ party_open_q*partisan, data = df_er_open, se = "cluster", cluster = "ResponseId")
lm_2_6 <- feols(recipient.cfscore.dyn_scaled ~ party_open_q*partisan_type, data = df_er_open, se = "cluster", cluster = "ResponseId")
lm_2_6 <- feols(recipient.cfscore.dyn_scaled_prop_comb ~ party_open_q*partisan, data = df_er_open, se = "cluster", cluster = "ResponseId")


plot_model(lm_2_6, type = "int") + 
  theme_bw() +
  labs(title = "",
       x = "Political Party in survey question",
       y = "Extremity",
       color = 'Partisanship of respondent') +
  scale_color_manual(values = c("blue", "red")) +
  theme(legend.position = "bottom")


names(df_er_participants)

screenreg(list(lm_2_0, lm_2_1, lm_2_2, lm_2_3, lm_2_4),
          custom.header = list("Ideological Score of recalled Politicians"= 1:4),
          custom.model.names = c("Mean", "Recall order", "Mean, scaled", "Recall order, scaled","Mean, scaled"),
          omit.coef = c("ResponseId"))

texreg(list(lm_2_2,lm_2_3,lm_2_4,lm_2_5),
          custom.header = list("Ideological Score of recalled Politicians"= 1:4),
          custom.model.names = c("Mean, scaled", "Recall order, scaled","Mean, scaled2", "Recall order, scaled2"),
          omit.coef = c("ResponseId"))

mean(df_er_open$recipient.cfscore.dyn, na.rm = TRUE)
median(df_er_open$recipient.cfscore.dyn, na.rm = TRUE)

recipient.cfscore.dyn_scaled
recipient.cfscore.dyn_scaled_comb

df_er_open_mean <- 
  df_er_open |> 
  ungroup() |> 
  dplyr::select(bonica.rid, recipient.cfscore.dyn_scaled, party_open_q) |> 
  distinct()

round(mean(df_er_open$recipient.cfscore.dyn_scaled, na.rm = TRUE),3)
round(mean(df_er_open_mean$recipient.cfscore.dyn_scaled, na.rm = TRUE),3)

round(median(df_er_open$recipient.cfscore.dyn_scaled, na.rm = TRUE),3)
round(sd(df_er_open$recipient.cfscore.dyn_scaled, na.rm = TRUE),3)

round(mean(df_er_open$recipient.cfscore.dyn_scaled[df_er_open$party_open_q == "Democrat"], na.rm = TRUE),3)
round(mean(df_er_open_mean$recipient.cfscore.dyn_scaled[df_er_open_mean$party_open_q == "Democrat"], na.rm = TRUE),3)

round(median(df_er_open$recipient.cfscore.dyn_scaled[df_er_open$party_open_q == "Democrat"], na.rm = TRUE),3)
round(sd(df_er_open$recipient.cfscore.dyn_scaled[df_er_open$party_open_q == "Democrat"], na.rm = TRUE),3)

round(mean(df_er_open$recipient.cfscore.dyn_scaled[df_er_open$party_open_q == "Republican"], na.rm = TRUE),3)
round(mean(df_er_open_mean$recipient.cfscore.dyn_scaled[df_er_open_mean$party_open_q == "Republican"], na.rm = TRUE),3)


round(median(df_er_open$recipient.cfscore.dyn_scaled[df_er_open$party_open_q == "Republican"], na.rm = TRUE),3)
round(sd(df_er_open$recipient.cfscore.dyn_scaled[df_er_open$party_open_q == "Republican"], na.rm = TRUE),3)

round(mean(df_er_open$recipient.cfscore.dyn_scaled[df_er_open$partisan == "Democrat"], na.rm = TRUE),3)
round(median(df_er_open$recipient.cfscore.dyn_scaled[df_er_open$partisan == "Democrat"], na.rm = TRUE),3)
round(sd(df_er_open$recipient.cfscore.dyn_scaled[df_er_open$partisan == "Democrat"], na.rm = TRUE),3)

round(mean(df_er_open$recipient.cfscore.dyn_scaled[df_er_open$partisan == "Republican"], na.rm = TRUE),3)
round(median(df_er_open$recipient.cfscore.dyn_scaled[df_er_open$partisan == "Republican"], na.rm = TRUE),3)
round(sd(df_er_open$recipient.cfscore.dyn_scaled[df_er_open$partisan == "Republican"], na.rm = TRUE),3)



round(mean(df_er_open$recipient.cfscore.dyn_scaled_comb, na.rm = TRUE),3)
round(median(df_er_open$recipient.cfscore.dyn_scaled_comb, na.rm = TRUE),3)
round(sd(df_er_open$recipient.cfscore.dyn_scaled_comb, na.rm = TRUE),3)

round(mean(df_er_open$recipient.cfscore.dyn_scaled_comb[df_er_open$party_open_q == "Democrat"], na.rm = TRUE),3)
round(median(df_er_open$recipient.cfscore.dyn_scaled_comb[df_er_open$party_open_q == "Democrat"], na.rm = TRUE),3)
round(sd(df_er_open$recipient.cfscore.dyn_scaled_comb[df_er_open$party_open_q == "Democrat"], na.rm = TRUE),3)

round(mean(df_er_open$recipient.cfscore.dyn_scaled_comb[df_er_open$party_open_q == "Republican"], na.rm = TRUE),3)
round(median(df_er_open$recipient.cfscore.dyn_scaled_comb[df_er_open$party_open_q == "Republican"], na.rm = TRUE),3)
round(sd(df_er_open$recipient.cfscore.dyn_scaled_comb[df_er_open$party_open_q == "Republican"], na.rm = TRUE),3)

round(mean(df_er_open$recipient.cfscore.dyn_scaled_comb[df_er_open$partisan == "Democrat"], na.rm = TRUE),3)
round(median(df_er_open$recipient.cfscore.dyn_scaled_comb[df_er_open$partisan == "Democrat"], na.rm = TRUE),3)
round(sd(df_er_open$recipient.cfscore.dyn_scaled_comb[df_er_open$partisan == "Democrat"], na.rm = TRUE),3)

round(mean(df_er_open$recipient.cfscore.dyn_scaled[df_er_open$partisan == "Republican"], na.rm = TRUE),3)
round(median(df_er_open$recipient.cfscore.dyn_scaled[df_er_open$partisan == "Republican"], na.rm = TRUE),3)
round(sd(df_er_open$recipient.cfscore.dyn_scaled[df_er_open$partisan == "Republican"], na.rm = TRUE),3)


table(df_er_open$partisan)
round(sd(df_er_open$recipient.cfscore.dyn_scaled[df_er_open$seat == "federal:house"], na.rm = TRUE),2)

mean(df_er_open$recipient.cfscore.dyn_scaled, na.rm = TRUE)
median(df_er_open$recipient.cfscore.dyn_scaled, na.rm = TRUE)


## additional
## pol interest, own ideological position (rescale based on party, middle to max con, middle to max dem, rescaled, experiment)
## most of pols recalled are... describe who most likely to be recalled indiv. are 
## robustness -top10 politicians, even if we give older people in closed ended, base rate of recall here would be X but we see Y 

# longform data, 1 for each politician picked by person and 0 for non picked person. each politicians is defined by covariates (recency, ideology, gender, race etc )
# replicate this for open as well, also combined with open/closed. logistic for who is picked or not, cluster(ResponseId)












## Number of names 
df_er_open |> 
  ggplot(aes(x = politicians_open_count)) +
  geom_bar() + 
  theme_bw()

lm_1_0 <- feols(politicians_open_count ~ ideology_self, data = df_er_open, se = "cluster", cluster = "ResponseId")
lm_1_1 <- lm_robust(politicians_open_count ~ ideology_self, data = df_er_open, clusters = ResponseId)
lm_1_2 <- lm_robust(politicians_open_count ~ pol_interest, data = df_er_open, clusters = ResponseId)
lm_1_3 <- lm_robust(politicians_open_count ~ political_party, data = df_er_open, clusters = ResponseId)
lm_1_4 <- lm_robust(politicians_open_count ~ partisan_type, data = df_er_open, clusters = ResponseId)

summary(lm_1_0)
summary(lm_1_1)

screenreg(list(lm_1_0, lm_1_1))
screenreg(list(lm_1_1, lm_1_2, lm_1_3, lm_1_4),
          custom.header = list("Number of politicians"= 1:4),
          custom.model.names = c("Ideology, self", "Political Interest", "Political Party", "Partisan Type"),
          omit.coef = c("ResponseId"))

## Ideological extremity 
df_er_open |> 
  ggplot(aes()) +
  geom_histogram(aes(x = recipient.cfscore.dyn), fill = "red") + 
  geom_histogram(aes(x = dwnom2), fill = "blue") + 
  theme_bw() +
  labs(x = "Ideology Score",
       caption = "Red = CF, Blue = DW2")

lm_2_1 <- lm_robust(recipient.cfscore.dyn ~ political_party, data = df_er_open, clusters = ResponseId)
lm_2_2 <- lm_robust(recipient.cfscore.dyn ~ partisan, data = df_er_open, clusters = ResponseId)
lm_2_3 <- lm_robust(recipient.cfscore.dyn ~ political_party, data = df_er_open, clusters = ResponseId)
lm_2_4 <- lm_robust(recipient.cfscore.dyn ~ ideology_self, data = df_er_open, clusters = ResponseId)
lm_2_5 <- lm_robust(recipient.cfscore.dyn ~ partisan_type, data = df_er_open, clusters = ResponseId)

screenreg(list(lm_2_1, lm_2_2, lm_2_3, lm_2_4, lm_2_5),
          custom.header = list("CF Score of recalled Politicians"= 1:5),
          custom.model.names = c("Political Party", "Partisan", "Political Party", "Ideology", "Partisan Type"),
          omit.coef = c("ResponseId"))


lm_2_6 <- lm_robust(recipient.cfscore.dyn ~  ideology_self*partisan_type, data = df_er_open, clusters = ResponseId)
plot_model(lm_2_6, type = "int", mdrt.values = "all") +
  theme_bw()+
  labs(subtitle = "Interaction between Partisan Type and Ideology of Respondent") +
  theme(legend.position = "bottom")


## Recency
df_er_open |> 
  ggplot(aes(x = recency)) +
  geom_bar() + 
  theme_bw()

table(df_er_open$recency)
lm_3_1 <- lm(recency_num ~ 1, data = df_er_open)


screenreg(list(lm_3_1))


## Office 
df_er_open |> 
  ggplot(aes(x = most_recent_office)) +
  geom_bar() + 
  theme_bw()

table(df_er_open$most_recent_office)

lm_4_1 <- lm(pres_gov_dummy ~ 1, data = df_er_open)
screenreg(list(lm_4_1))

## Race
df_er_open |> 
  ggplot(aes(x = race_ethnicity)) +
  geom_bar() + 
  theme_bw()

table(df_er_open$race_ethnicity)

lm_5_1 <- lm(recall_white ~ 1, data = df_er_open)

screenreg(list(lm_5_1))

## Gender 
screenreg(list(lm_3_1, lm_4_1, lm_5_1),
          custom.header = list("Intercept Models"= 1:3),
          custom.model.names = c("Recency", "Executive", "White"),
          omit.coef = c("ResponseId"))
