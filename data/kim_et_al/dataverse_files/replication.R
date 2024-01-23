

# --------------------------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------------------------
# Measuring Dynamic Media Bias 
# Eunji Kim, Yphtalk Lelkes*, and Josh McCrain 2022
# Proceedings of the National Academy of Sciences (PNAS)
# *corresponding author (ylelkes@upenn.edu)
# --------------------------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------------------------


#list.of.packages <- c("tidyverse", "lubridate") 
#new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
#if(length(new.packages)) install.packages(new.packages)

#setwd("[path to where replication archive was downloaded]")


library(tidyverse)
library(lubridate)

########################################################################
############## Loading Needed Data and Basic Cleaning ##################
########################################################################

load("screen_panel.RData")
load("show_screen_panel.RData")
load("topshows.RData")

# names of non-politicians in the Stanford TV database

nonpols <- read_csv("non_politicians_check.csv") %>% filter(keep == 1)

# loading show level data

load("individual_shows.RData")

keep <- c("Cuomo Primetime", "CNN Right Now With Brianna Keilar", "The Lead With Jake Tapper", "Anderson Cooper 360",
          "CNN Tonight", "Tucker Carlson Tonight", "The OReilly Factor", "The Ingraham Angle", "Hannity",
          "The Five", "The Rachel Maddow Show", "Deadline White House", "The 11th Hour With Brian Williams", "The Last Word With Lawrence ODonnell",
          "The Beat With Ari Melber")

showorder <- shows %>% 
  filter(show %in% keep) %>% 
  ungroup %>%
  mutate(week = week(ymd(date))) %>%
  mutate(weighted_cfscore = cfscore*screentime) %>% 
  mutate(show = paste(show, " (", channel, ")", sep = "")) %>% 
  group_by(channel, show) %>% 
  summarize(IQR = IQR(weighted_cfscore),
            meancf = mean(weighted_cfscore), n = n(),
            hours = (sum(screentime)/60)/60) %>% 
  arrange(meancf) %>% 
  mutate(color = case_when(
    channel == "MSNBC" ~ "dodgerblue",
    channel == "CNN" ~ "red2",
    channel == "FOXNEWS" ~ "springgreen4"
  ))

showorder2 <- shows %>% 
  filter(show %in% keep) %>% 
  ungroup %>%
  filter(stanford_name %in% nonpols$stanford_name) %>% 
  mutate(week = week(ymd(date))) %>%
  mutate(weighted_cfscore = cfscore*screentime) %>% 
  mutate(show = paste(show, " (", channel, ")", sep = "")) %>% 
  group_by(channel, show) %>% 
  summarize(IQR = IQR(weighted_cfscore),
            meancf = mean(weighted_cfscore), n = n(),
            hours = (sum(screentime)/60)/60) %>% 
  arrange(meancf) %>% 
  mutate(color = case_when(
    channel == "MSNBC" ~ "dodgerblue",
    channel == "CNN" ~ "red2",
    channel == "FOXNEWS" ~ "springgreen4"
  ))


###########################################################################
################ Figure 1: Program-Level Media Bias  ######################
###########################################################################


shows %>% 
  filter(stanford_name %in% nonpols$stanford_name) %>% 
  filter(show %in% keep) %>%
  ungroup %>% 
  mutate(week = week(ymd(date))) %>%
  mutate(weighted_cfscore = cfscore*screentime) %>% 
  group_by(year, week, show, channel) %>% 
  dplyr::summarize(meancf = mean(weighted_cfscore)) %>%
  ungroup %>% 
  mutate(show = paste(show, " (", channel, ")", sep = "")) %>% 
  mutate(show = factor(show, levels = showorder2$show)) %>% 
  ggplot(aes(x = meancf, y = show, group = show)) + 
  geom_boxplot(outlier.alpha = 0) +
  geom_jitter(width = .15, height = .1, alpha = .1, aes(color = channel), size=0.5 ) +
  scale_color_manual(values = c("FOXNEWS" ="#990000", "CNN"='#333333', "MSNBC" = "#0066CC")) + 
  xlim(-300, 100) +
  theme_minimal() +
  theme(legend.position = "none") + 
  theme(legend.text=element_text(color='black',size=12), 
        axis.text = element_text(color='black', size=12)) +
  xlab("Weighted CF Score") + ylab("")

ggsave(file = "programbias.pdf", units="in", width=13, height=5)


###########################################################################
################ Figure 2 #################################################
###########################################################################

# cleaning needed data for Figure 2 

breaks <- screen.all %>% 
  ungroup %>% 
  mutate(week = week(ymd(date))) %>%
  mutate(weighted_cfscore = cfscore*screentime) %>% 
  group_by(channel, year, week) %>% 
  dplyr::summarize(meancf = mean(weighted_cfscore)) %>% 
  ungroup %>% 
  group_by(channel) %>% 
  mutate(order = row_number()) %>% 
  ungroup %>% 
  filter(week==1 & channel=="CNN")


cnn <- screen.all %>% 
  filter(channel == "CNN") %>% 
  ungroup %>% 
  mutate(week = week(ymd(date))) %>%
  mutate(weighted_cfscore = cfscore*screentime) %>% 
  group_by(channel, year, week) %>% 
  dplyr::summarize(meancf_cnn = mean(weighted_cfscore)) %>% 
  ungroup %>% dplyr::select(-channel)

fox <- screen.all %>% 
  filter(channel == "FOXNEWS") %>% 
  ungroup %>% 
  mutate(week = week(ymd(date))) %>%
  mutate(weighted_cfscore = cfscore*screentime) %>% 
  group_by(channel, year, week) %>% 
  dplyr::summarize(meancf_fox = mean(weighted_cfscore)) %>% 
  ungroup %>% dplyr::select(-channel)

msnbc <- screen.all %>% 
  filter(channel == "MSNBC") %>% 
  ungroup %>% 
  mutate(week = week(ymd(date))) %>%
  mutate(weighted_cfscore = cfscore*screentime) %>% 
  group_by(channel, year, week) %>% 
  dplyr::summarize(meancf_msnbc = mean(weighted_cfscore)) %>% 
  ungroup %>% dplyr::select(-channel)

weekly <- left_join(cnn, fox) %>% left_join(msnbc)



##############################  Figure 2A ##################################

p1 <- weekly %>% 
  ungroup %>% 
  rowwise() %>% 
  mutate(`CNN <> FOX` = dist(rbind(meancf_cnn, meancf_fox)) %>% as.numeric,
         `CNN <> MSNBC` = dist(rbind(meancf_cnn, meancf_msnbc)) %>% as.numeric,
         `MSNBC <> FOX` = dist(rbind(meancf_msnbc, meancf_fox)) %>% as.numeric) %>% 
  ungroup %>% 
  arrange(year, week) %>% 
  mutate(order = row_number()) %>% 
  pivot_longer(`CNN <> FOX`:`MSNBC <> FOX`) %>% 
  ggplot(aes(x = order, y= value, color = name, group = name, linetype = name)) + geom_smooth(se=F) +
  scale_linetype_manual(values=c("solid", "dashed", "twodash"))+ 
  scale_x_continuous(breaks = breaks$order, labels = breaks$year) +
  scale_color_grey() +
  theme_classic() +
  xlab("Year") + ylab("Absolute Distance in Weighted CF Scores") +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(angle = 90),
        legend.position="none") 

p1 <- p1 + 
  
  annotate(
    geom = "curve", x = 80, y = 58, xend = 82, yend = 52 , 
    curvature = .3, arrow = arrow(length = unit(2, "mm"))
  ) +
  annotate(geom = "text", x = 81, y = 50, label = "CNN::FOX", hjust = "left", fontface="bold") +
  
  annotate(
    geom = "curve", x = 150, y = 70, xend = 148, yend = 74 , 
    curvature = .3, arrow = arrow(length = unit(2, "mm"))
  ) +
  annotate(geom = "text", x = 110, y = 77, label = "MSNBC::FOX", hjust = "left", fontface="bold") + 
  
  annotate(
    geom = "curve", x = 220, y = 25, xend = 240, yend = 28 , 
    curvature = .3, arrow = arrow(length = unit(2, "mm"))
  ) +
  annotate(geom = "text", x = 210, y = 31, label = "CNN::MSNBC", hjust = "left", fontface="bold")


p1

ggsave(file = "distance.pdf", units="in", width=8, height=5)


##############################  Figure 2B ##################################

show.all <- show.all %>% 
  mutate(show = case_when(
    hour == "8-10" ~ "Morning",
    hour == "13-15" ~ "Afternoon",
    hour == "19-21" ~ "Primetime"
  ))

screen.all$show <- "Full Day"


show.all %>% 
  bind_rows(screen.all) %>% 
  ungroup %>% 
  mutate(show = as.factor(show)) %>% 
  mutate(show = fct_relevel(show, c("Full Day", "Morning", "Afternoon", "Primetime"))) %>% 
  mutate(week = week(ymd(date))) %>%
  mutate(weighted_cfscore = cfscore*screentime) %>% 
  group_by(channel, show, year, week) %>% 
  dplyr::summarize(meancf = mean(weighted_cfscore), .groups=NULL) %>% 
  ungroup %>% 
  group_by(channel, show) %>% 
  mutate(order = row_number()) %>% 
  ggplot(aes(x = order, y = meancf, group=channel, color=channel, linetype=channel)) +
  geom_smooth(se=F) +
  facet_wrap(~show, ncol = 4) +
  scale_x_continuous(breaks = breaks$order, labels = breaks$year) +
  scale_color_manual(values = c( "#333333" , "#990000", "#0066CC" )) +
  theme_classic() +
  xlab("") + ylab("Weighted CF Score") +
  theme(legend.title = element_blank(), 
        legend.position = c(0.95, 0.9),
        legend.background = element_rect(fill = alpha("white", 0), colour = NA),
        axis.text.x = element_text(angle = 90)) 



ggsave(file = "timeslot_trends.pdf", units="in", width=12, height=4)




###########################################################################
################ Figure 3 #################################################
###########################################################################
  
shows %>% 
  filter(show %in% c("The Rachel Maddow Show", "The OReilly Factor", "The Lead With Jake Tapper")) %>% 
  ungroup %>% 
  mutate(week = week(ymd(date)), month = month(ymd(date))) %>%
  mutate(weighted_cfscore = cfscore*screentime) %>% 
  group_by(year, month, show) %>% 
  dplyr::summarize(meancf = mean(weighted_cfscore)) %>% 
  ungroup %>% 
  mutate(date = ymd(paste(year, month, "01", sep = "-"))) %>%
  arrange(show) %>% 
  ggplot(aes(x = date, y = meancf, color = show, linetype = show)) + 
  geom_line(alpha=.4) + geom_smooth(se=F) +  
  scale_color_manual(values = c( "#333333" ,"#990000", "#0066CC" )) +
  geom_text(data = data.frame(
    x = c(ymd("2018-10-30"), ymd("2017-05-01"), ymd("2019-02-01")),
    y = c(90, -30, 25),
    text = c("The OReilly Factor", "The Rachel Maddow Show", "The Lead With Jake Tapper"), 
    fontface="bold", size=9
  ),
  aes(x, y, label=text, color = text,   fontface="bold", size=10), inherit.aes = F) +
  theme_classic() +
  theme(legend.position = "none", axis.text=element_text(size = 16), axis.title = element_text(size=17))   +
  xlab("Date") + ylab("Weighted CF Score")

ggsave(file = "shows_facet.pdf", units="in", width=7, height=5)









