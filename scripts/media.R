# Load libs.

library(tidyverse)
library(lubridate)
library(readr)
library(ggplot2)
library(scales)

## Load dat
load("data/kim_et_al/dataverse_files/screen_panel.RData")
load("data/kim_et_al/dataverse_files/show_screen_panel.RData")
load("data/kim_et_al/dataverse_files/topshows.RData")

# names of non-politicians in the Stanford TV database
nonpols <- read_csv("data/kim_et_al/dataverse_files/non_politicians_check.csv") %>% filter(keep_gs == 0)

# loading show level data
load("data/kim_et_al/dataverse_files/individual_shows.RData")

keep <- c("Cuomo Primetime", "CNN Right Now With Brianna Keilar", "The Lead With Jake Tapper", "Anderson Cooper 360",
          "CNN Tonight", "Tucker Carlson Tonight", "The OReilly Factor", "The Ingraham Angle", "Hannity",
          "The Five", "The Rachel Maddow Show", "Deadline White House", "The 11th Hour With Brian Williams", "The Last Word With Lawrence ODonnell",
          "The Beat With Ari Melber")

# Some issues

# mean(shows$cfscore[shows$stanford_name == "donald trump" & shows$year == 202])
# mean(shows$cfscore[shows$stanford_name == "donald trump" & shows$year == 2020])
# Didn't get the keep so created my own. Not perfect but seems much better.

### Pol. Name Density Plot
counts <- shows %>%
  #filter(show %in% keep) %>% 
  filter(!(stanford_name %in% nonpols$stanford_name)) %>% 
  count(stanford_name) %>%
  mutate(perc = n / sum(n) * 100)

ggplot(counts, aes(x = n)) +
  geom_density(fill = "skyblue", color = "blue", alpha = 0.7) +
  labs(title = "Density Plot of Frequency for All Names",
       x = "Frequency",
       y = "Density") +
  scale_y_continuous(labels = label_number()) +
  theme_minimal()
ggsave("figures/media_pol_density.pdf")

name_percs <- shows %>%
  count(stanford_name) %>%
  arrange(desc(n)) %>%
  slice_head(n = 50) %>%
  mutate(perc = n / sum(n) * 100)

# Plot histogram
ggplot(name_percs, aes(x = reorder(stanford_name, n), y = perc)) +
  geom_col() +
  coord_flip() +
  labs(title = "Top 50 Names and 'All Other' by Frequency",
       y = "Percentage of All Citations") +
  scale_y_continuous(breaks = seq(1, 10, 1)) +
  theme_minimal()
ggsave("figures/media_top_50.pdf")

name_percs <- shows %>%
  filter(!(stanford_name %in% nonpols$stanford_name)) %>% 
  count(stanford_name) %>%
  arrange(desc(n)) %>%
  slice_head(n = 20) %>%
  bind_rows(
    shows %>% 
      count(stanford_name) %>%
      arrange(desc(n)) %>% 
      summarise(stanford_name = 'All other', n = sum(n))
  ) %>%
  mutate(perc = n / sum(n) * 100)

