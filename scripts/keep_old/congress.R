library(tidyverse)
library(scales)


## Study 1
df_house_w1 <- read.csv("data/cf_scores/dime_recipients_1979_2020.csv.gz") |> 
  filter(seat %in% c("federal:house")) |> 
  filter(cycle == 2012) |> 
  filter(gwinner == "W") |> 
  dplyr::select(election, cycle, fecyear, bonica.rid, name, party, state, seat, cand.gender, recipient.cfscore:irt.cfscore)


df_senate_w1 <- read.csv("data/cf_scores/dime_recipients_1979_2020.csv.gz") |> 
  filter(seat %in% c("federal:senate"))|> 
  filter(cycle >= 2007 & cycle < 2014) |> 
  filter(gwinner == "W") |> 
  dplyr::select(election, cycle, fecyear, bonica.rid, name, party, state, seat, cand.gender, recipient.cfscore:irt.cfscore)

df_congress_w1 <-
  df_house_w1 |> 
  bind_rows(df_senate_w1) |> 
  mutate(party = ifelse(party == "100", "Democrat",
                        ifelse(party == "200", "Republican", NA)),
         recipient.cfscore.dyn_scaled1 = ifelse(party == "Democrat", recipient.cfscore.dyn, NA),
         recipient.cfscore.dyn_scaled1 = recipient.cfscore.dyn_scaled1 - 0.332,
         recipient.cfscore.dyn_scaled2 = ifelse(party == "Republican", recipient.cfscore.dyn, NA),
         recipient.cfscore.dyn_scaled_comb = coalesce(abs(recipient.cfscore.dyn_scaled1),recipient.cfscore.dyn_scaled2),
         recipient.cfscore.dyn_scaled_comb = rescale(recipient.cfscore.dyn_scaled_comb),
         recipient.cfscore.dyn_scaled1 = rescale(abs(recipient.cfscore.dyn_scaled1)),
         recipient.cfscore.dyn_scaled2 = rescale(recipient.cfscore.dyn_scaled2),
         recipient.cfscore.dyn_scaled = coalesce(recipient.cfscore.dyn_scaled1,recipient.cfscore.dyn_scaled2)) |> 
  #dplyr::select(-c(recipient.cfscore.dyn_scaled1,recipient.cfscore.dyn_scaled2)) |> 
  ungroup()



range(df_congress_w1$recipient.cfscore.dyn[df_congress_w1$party == "Democrat"], na.rm = TRUE)
range(df_congress_w1$recipient.cfscore.dyn1[df_congress_w1$party == "Democrat"], na.rm = TRUE)

ggplot(df_congress_w1, aes(x = recipient.cfscore.dyn_scaled_comb)) +
  geom_histogram() +
  facet_wrap(~party)

## Mean, Median, SD 
round(mean(df_congress_w1$recipient.cfscore.dyn_scaled_comb, na.rm = TRUE),2)

round(mean(df_congress_w1$recipient.cfscore.dyn_scaled_comb, na.rm = TRUE),2)
round(mean(df_congress_w1$recipient.cfscore.dyn_scaled_comb[df_congress_w1$party == "Democrat"], na.rm = TRUE),2)
round(sd(df_congress_w1$recipient.cfscore.dyn_scaled_comb[df_congress_w1$party == "Democrat"], na.rm = TRUE),2)


round(mean(df_congress_w1$recipient.cfscore.dyn_scaled1[df_congress_w1$party == "Democrat"], na.rm = TRUE),2)
round(sd(df_congress_w1$recipient.cfscore.dyn_scaled1[df_congress_w1$party == "Democrat"], na.rm = TRUE),2)

round(mean(df_congress_w1$recipient.cfscore.dyn_scaled_comb[df_congress_w1$party == "Republican"], na.rm = TRUE),2)
round(sd(df_congress_w1$recipient.cfscore.dyn_scaled_comb[df_congress_w1$party == "Republican"], na.rm = TRUE),2)


round(mean(df_congress_w1$recipient.cfscore.dyn_scaled2[df_congress_w1$party == "Republican"], na.rm = TRUE),2)
round(sd(df_congress_w1$recipient.cfscore.dyn_scaled2[df_congress_w1$party == "Republican"], na.rm = TRUE),2)


round(median(df_congress_w1$recipient.cfscore.dyn_scaled_comb, na.rm = TRUE),2)
round(sd(df_congress_w1$recipient.cfscore.dyn_scaled_comb, na.rm = TRUE),2)


round(mean(df_congress_w1$recipient.cfscore.dyn_scaled_comb[df_congress_w1$seat == "federal:house"], na.rm = TRUE),2)
round(mean(df_congress_w1$recipient.cfscore.dyn_scaled_comb[df_congress_w1$seat == "federal:house" & df_congress_w1$party == "Democrat"], na.rm = TRUE),2)
round(mean(df_congress_w1$recipient.cfscore.dyn_scaled_comb[df_congress_w1$seat == "federal:house" & df_congress_w1$party == "Republican"], na.rm = TRUE),2)

round(median(df_congress_w1$recipient.cfscore.dyn_scaled_comb[df_congress_w1$seat == "federal:house"], na.rm = TRUE),2)
round(sd(df_congress_w1$recipient.cfscore.dyn_scaled_comb[df_congress_w1$seat == "federal:house"], na.rm = TRUE),2)

round(mean(df_congress_w1$recipient.cfscore.dyn_scaled_comb[df_congress_w1$seat == "federal:senate"], na.rm = TRUE),2)
round(mean(df_congress_w1$recipient.cfscore.dyn_scaled_comb[df_congress_w1$seat == "federal:senate" & df_congress_w1$party == "Democrat"], na.rm = TRUE),2)
round(mean(df_congress_w1$recipient.cfscore.dyn_scaled_comb[df_congress_w1$seat == "federal:senate" & df_congress_w1$party == "Republican"], na.rm = TRUE),2)



## Study 2
df_house_w2 <- read.csv("data/cf_scores/dime_recipients_1979_2020.csv.gz") |> 
  filter(seat %in% c("federal:house")) |> 
  filter(cycle == 2020) |> 
  filter(gwinner == "W") |> 
  dplyr::select(election, cycle, fecyear, bonica.rid, name, party, state, seat, cand.gender, recipient.cfscore:irt.cfscore)
  

df_senate_w2 <- read.csv("data/cf_scores/dime_recipients_1979_2020.csv.gz") |> 
  filter(seat %in% c("federal:senate"))|> 
  filter(cycle >= 2015) |> 
  filter(gwinner == "W") |> 
  dplyr::select(election, cycle, fecyear, bonica.rid, name, party, state, seat, cand.gender, recipient.cfscore:irt.cfscore)

df_congress_w2 <-
  df_house_w2 |> 
  bind_rows(df_senate_w2) |> 
  mutate(party = ifelse(party == "100", "Democrat",
                        ifelse(party == "200", "Republican", NA)),
         recipient.cfscore.dyn_scaled1 = ifelse(party == "Democrat", recipient.cfscore.dyn, NA),
         recipient.cfscore.dyn_scaled2 = ifelse(party == "Republican", recipient.cfscore.dyn, NA),
         recipient.cfscore.dyn_scaled_comb = coalesce(abs(recipient.cfscore.dyn_scaled1),recipient.cfscore.dyn_scaled2),
         recipient.cfscore.dyn_scaled_comb = rescale(recipient.cfscore.dyn_scaled_comb),
         recipient.cfscore.dyn_scaled1 = rescale(abs(recipient.cfscore.dyn_scaled1)),
         recipient.cfscore.dyn_scaled2 = rescale(recipient.cfscore.dyn_scaled2),
         recipient.cfscore.dyn_scaled = coalesce(recipient.cfscore.dyn_scaled1,recipient.cfscore.dyn_scaled2))|> 
  dplyr::select(-c(recipient.cfscore.dyn_scaled1,recipient.cfscore.dyn_scaled2)) |> 
  ungroup()

mean(df_congress_w2$recipient.cfscore.dyn_scaled_comb, na.rm = TRUE)

## Visualizing 
ggplot(df_congress_w2, aes(x = recipient.cfscore.dyn_scaled)) +
  #geom_histogram(aes(x = recipient.cfscore.dyn_scaled_comb, fill = party_open_q, alpha = 0.8)) + 
  geom_histogram(data=subset(df_congress_w2,party == 'Democrat'),fill = "blue", alpha = 0.5) +
  geom_histogram(data=subset(df_congress_w2,party == 'Republican'),fill = "red", alpha = 0.5) +
  # geom_histogram(aes(x = dwnom2), fill = "blue") + 
  theme_bw() +
  labs(title = "CF Score Distribution for current Congress",
       subtitle = "Political parties are scaled on party-specific min and max",
       x = "CF Score, rescaled", y = "Count", fill = "Party")  +
  theme(legend.position = "none")


ggplot(df_congress_w2, aes(x = recipient.cfscore.dyn_scaled_comb)) +
  #geom_histogram(aes(x = recipient.cfscore.dyn_scaled_comb, fill = party_open_q, alpha = 0.8)) + 
  geom_histogram(data=subset(df_congress_w2,party == 'Democrat'),fill = "blue", alpha = 0.5) +
  geom_histogram(data=subset(df_congress_w2,party == 'Republican'),fill = "red", alpha = 0.5) +
  # geom_histogram(aes(x = dwnom2), fill = "blue") + 
  theme_bw()+
  labs(title = "CF Score Distribution for current Congress",
       subtitle = "Political parties are scaled on shared min and max",
       x = "CF Score, rescaled", y = "Count", fill = "Party")  +
  theme(legend.position = "none")


## Mean, Median, SD 
round(mean(df_congress_w2$recipient.cfscore.dyn_scaled_comb, na.rm = TRUE),2)

round(mean(df_congress_w2$recipient.cfscore.dyn_scaled_comb, na.rm = TRUE),2)
round(mean(df_congress_w2$recipient.cfscore.dyn_scaled_comb[df_congress_w2$party == "Democrat"], na.rm = TRUE),2)
round(mean(df_congress_w2$recipient.cfscore.dyn_scaled_comb[df_congress_w2$party == "Republican"], na.rm = TRUE),2)

round(sd(df_congress_w2$recipient.cfscore.dyn_scaled_comb[df_congress_w2$party == "Democrat"], na.rm = TRUE),2)
round(sd(df_congress_w2$recipient.cfscore.dyn_scaled_comb[df_congress_w2$party == "Republican"], na.rm = TRUE),2)


round(median(df_congress_w2$recipient.cfscore.dyn_scaled_comb, na.rm = TRUE),2)
round(sd(df_congress_w2$recipient.cfscore.dyn_scaled_comb, na.rm = TRUE),2)


round(mean(df_congress_w2$recipient.cfscore.dyn_scaled[df_congress_w2$seat == "federal:house"], na.rm = TRUE),2)
round(mean(df_congress_w2$recipient.cfscore.dyn_scaled[df_congress_w2$seat == "federal:house" & df_congress_w2$party == "Democrat"], na.rm = TRUE),2)
round(mean(df_congress_w2$recipient.cfscore.dyn_scaled[df_congress_w2$seat == "federal:house" & df_congress_w2$party == "Republican"], na.rm = TRUE),2)

round(median(df_congress_w2$recipient.cfscore.dyn_scaled[df_congress_w2$seat == "federal:house"], na.rm = TRUE),2)
round(sd(df_congress_w2$recipient.cfscore.dyn_scaled[df_congress_w2$seat == "federal:house"], na.rm = TRUE),2)

round(mean(df_congress_w2$recipient.cfscore.dyn_scaled[df_congress_w2$seat == "federal:senate"], na.rm = TRUE),2)
round(mean(df_congress_w2$recipient.cfscore.dyn_scaled[df_congress_w2$seat == "federal:senate" & df_congress_w2$party == "Democrat"], na.rm = TRUE),2)
round(mean(df_congress_w2$recipient.cfscore.dyn_scaled[df_congress_w2$seat == "federal:senate" & df_congress_w2$party == "Republican"], na.rm = TRUE),2)


round(median(df_congress_w2$recipient.cfscore.dyn_scaled[df_congress_w2$seat == "federal:senate"], na.rm = TRUE),2)
round(sd(df_congress_w2$recipient.cfscore.dyn_scaled[df_congress_w2$seat == "federal:senate"], na.rm = TRUE),2)


round(mean(df_congress_w2$recipient.cfscore.dyn_scaled_comb, na.rm = TRUE),2)
round(median(df_congress_w2$recipient.cfscore.dyn_scaled_comb, na.rm = TRUE),2)
round(sd(df_congress_w2$recipient.cfscore.dyn_scaled_comb, na.rm = TRUE),2)

round(mean(df_congress_w2$recipient.cfscore.dyn_scaled_comb[df_congress_w2$seat == "federal:house"], na.rm = TRUE),2)
round(median(df_congress_w2$recipient.cfscore.dyn_scaled_comb[df_congress_w2$seat == "federal:house"], na.rm = TRUE),2)
round(sd(df_congress_w2$recipient.cfscore.dyn_scaled_comb[df_congress_w2$seat == "federal:house"], na.rm = TRUE),2)

round(mean(df_congress_w2$recipient.cfscore.dyn_scaled_comb[df_congress_w2$seat == "federal:senate"], na.rm = TRUE),2)
round(median(df_congress_w2$recipient.cfscore.dyn_scaled_comb[df_congress_w2$seat == "federal:senate"], na.rm = TRUE),2)
round(sd(df_congress_w2$recipient.cfscore.dyn_scaled_comb[df_congress_w2$seat == "federal:senate"], na.rm = TRUE),2)

df_congress_w2 |> 
  filter(party == "Democrat") |> 
  dplyr::select(name, recipient.cfscore.dyn_scaled_comb)
  arrange(recipient.cfscore.dyn_scaled_comb)
  
df_congress_w2 |> 
    filter(party == "Republican") |> 
    dplyr::select(name, recipient.cfscore.dyn_scaled_comb)
  arrange(recipient.cfscore.dyn_scaled_comb)
