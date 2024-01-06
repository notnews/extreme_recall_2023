# Title: analysis.R
# Date: 2023-12-15
# Author: Daniel Weitzel
# Email: daniel.weitzel@colostate.edu
# Purpose: Analyze the data

# Working directory
setwd(githubdir)

# Libraries
source(file = "extreme_recall2/scripts/qualtrics.R")

## Visualize number of politicians in OPEN
df_er_open |> 
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
