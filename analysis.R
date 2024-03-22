library(haven)
library(dplyr)
library(readxl)
library(stringr)
library(ggplot2)
library(RColorBrewer)
library(cowplot)

setwd("/Volumes/GoogleDrive/My Drive/ITD")

rm(list = ls())

################################################################################
#import ITD
################################################################################

data <- read_excel("incarceration_trends.xlsx")

################################################################################
#create vars
################################################################################

data <- data %>%
  mutate(bw = black_jail_pop_rate/white_jail_pop_rate,
         jail_capacity_rate = jail_rated_capacity/total_pop_15to64*100000)

################################################################################
#Figure of jail population rate by urbanicity over time
################################################################################

rural <- as.data.frame(tapply(data[which(data$urbanicity=="rural"),]$total_jail_pop_rate,data[which(data$urbanicity=="rural"),]$year,median,na.rm=TRUE)) 
rural <- rural %>% 
  rename(pop_median = c("tapply(data[which(data$urbanicity == \"rural\"), ]$total_jail_pop_rate, ")) %>%
  mutate(year = 1970:2018,
         urbanicity = "rural")
rownames(rural) <- NULL

small <- as.data.frame(tapply(data[which(data$urbanicity=="small/mid"),]$total_jail_pop_rate,data[which(data$urbanicity=="small/mid"),]$year,median,na.rm=TRUE)) 
small <- small %>% 
  rename(pop_median = c("tapply(data[which(data$urbanicity == \"small/mid\"), ]$total_jail_pop_rate, ")) %>%
  mutate(year = 1970:2018,
         urbanicity = "small")
rownames(small) <- NULL

suburban <- as.data.frame(tapply(data[which(data$urbanicity=="suburban"),]$total_jail_pop_rate,data[which(data$urbanicity=="suburban"),]$year,median,na.rm=TRUE)) 
suburban <- suburban %>% 
  rename(pop_median = c("tapply(data[which(data$urbanicity == \"suburban\"), ]$total_jail_pop_rate, ")) %>%
  mutate(year = 1970:2018,
         urbanicity = "suburban")
rownames(suburban) <- NULL

urban <- as.data.frame(tapply(data[which(data$urbanicity=="urban"),]$total_jail_pop_rate,data[which(data$urbanicity=="urban"),]$year,median,na.rm=TRUE))
urban <- urban %>% 
  rename(pop_median = c("tapply(data[which(data$urbanicity == \"urban\"), ]$total_jail_pop_rate, ")) %>%
  mutate(year = 1970:2018,
         urbanicity = "urban")
rownames(urban) <- NULL

pop_data <- rbind(rural,small,suburban,urban)
rm(rural,small,suburban,urban)

pop_data <- pop_data[which(pop_data$year>=1978),] %>%
  mutate(year = as.character(year))

g1 <- ggplot(pop_data, aes(x=year, 
                           y = pop_median, 
                           linetype = urbanicity, 
                           color = urbanicity,
                           group = urbanicity)) +
  geom_line(size = 2) +
  ylab("Total Jail Population Rate") +
  ggtitle(element_blank()) +
  theme_bw() +
  theme(legend.text = element_text(size=10,
                                   family = "serif"),
        axis.text.x = element_text(size=12,
                                   family = "serif",
                                   angle = 45, 
                                   vjust = 1, 
                                   hjust=1,
                                   color = "black"),
        axis.text.y = element_text(size=12,
                                   family = "serif",
                                   color = "black"),
        axis.title.y = element_text(size=15,
                                    family = "serif"),
        legend.title=element_blank(),
        legend.position=c(0.155,0.83),
        axis.title.x = element_blank()) +
  scale_linetype_manual(labels=c("Rural (N=1976)",
                                 "Small/mid metro (N=730)",
                                 "Suburban (N=368)",
                                 "Urban (N=64)"),
                        values=c("solid","dotted","dashed","dotdash")) +
  scale_color_manual(labels=c("Rural (N=1976)",
                              "Small/mid metro (N=730)",
                              "Suburban (N=368)",
                              "Urban (N=64)"),
                     values = brewer.pal(4, "Set1")) +
  scale_x_discrete(breaks=c(1978,1983,1988,1993,1998,2003,2008,2013,2018),
                   labels=c("1978","1983","1988","1993","1998","2003","2008","2013","2018"))

g1 <- ggplot(pop_data, aes(x=year, 
                           y = pop_median, 
                           color = urbanicity,
                           group = urbanicity)) +
  geom_line(size = 2) +
  ylab("Total Jail Population Rate") +
  ggtitle(element_blank()) +
  theme_bw() +
  theme(legend.text = element_text(size=10,
                                   family = "serif"),
        axis.text.x = element_text(size=12,
                                   family = "serif",
                                   angle = 45, 
                                   vjust = 1, 
                                   hjust=1,
                                   color = "black"),
        axis.text.y = element_text(size=12,
                                   family = "serif",
                                   color = "black"),
        axis.title.y = element_text(size=15,
                                    family = "serif"),
        legend.title=element_blank(),
        legend.position=c(0.155,0.83),
        axis.title.x = element_blank()) +
  scale_colour_grey(labels=c("Rural (N=1976)",
                              "Small/mid metro (N=730)",
                              "Suburban (N=368)",
                              "Urban (N=64)"),
                    start = 0.1,
                    end = 0.9) +
  scale_x_discrete(breaks=c(1978,1983,1988,1993,1998,2003,2008,2013,2018),
                   labels=c("1978","1983","1988","1993","1998","2003","2008","2013","2018"))

################################################################################
#Figure of jail admission rate by urbanicity over time
################################################################################

rural <- as.data.frame(tapply(data[which(data$urbanicity=="rural"),]$total_jail_adm_rate,data[which(data$urbanicity=="rural"),]$year,median,na.rm=TRUE)) 
rural <- rural %>% 
  rename(pop_median = c("tapply(data[which(data$urbanicity == \"rural\"), ]$total_jail_adm_rate, ")) %>%
  mutate(year = 1970:2018,
         urbanicity = "rural")
rownames(rural) <- NULL

small <- as.data.frame(tapply(data[which(data$urbanicity=="small/mid"),]$total_jail_adm_rate,data[which(data$urbanicity=="small/mid"),]$year,median,na.rm=TRUE)) 
small <- small %>% 
  rename(pop_median = c("tapply(data[which(data$urbanicity == \"small/mid\"), ]$total_jail_adm_rate, ")) %>%
  mutate(year = 1970:2018,
         urbanicity = "small")
rownames(small) <- NULL

suburban <- as.data.frame(tapply(data[which(data$urbanicity=="suburban"),]$total_jail_adm_rate,data[which(data$urbanicity=="suburban"),]$year,median,na.rm=TRUE)) 
suburban <- suburban %>% 
  rename(pop_median = c("tapply(data[which(data$urbanicity == \"suburban\"), ]$total_jail_adm_rate, ")) %>%
  mutate(year = 1970:2018,
         urbanicity = "suburban")
rownames(suburban) <- NULL

urban <- as.data.frame(tapply(data[which(data$urbanicity=="urban"),]$total_jail_adm_rate,data[which(data$urbanicity=="urban"),]$year,median,na.rm=TRUE))
urban <- urban %>% 
  rename(pop_median = c("tapply(data[which(data$urbanicity == \"urban\"), ]$total_jail_adm_rate, ")) %>%
  mutate(year = 1970:2018,
         urbanicity = "urban")
rownames(urban) <- NULL

adm_data <- rbind(rural,small,suburban,urban)
rm(rural,small,suburban,urban)

adm_data <- adm_data[which(adm_data$year>=1978),] %>%
  mutate(year = as.character(year))

g2 <- ggplot(adm_data, aes(x=year, 
                           y = pop_median, 
                           color = urbanicity,
                           group = urbanicity)) +
  geom_line(size = 2) +
  ylab("Total Jail Admission Rate") +
  ggtitle(element_blank()) +
  theme_bw() +
  theme(text = element_text(size = 15,
                            family = "serif",
                            color = "black"),
        legend.title=element_blank(),
        legend.position="none",
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, color = "black"),
        axis.text.y = element_text(color = "black")) +
  scale_colour_grey(labels=c("Rural (N=1976)",
                             "Small/mid metro (N=730)",
                             "Suburban (N=368)",
                             "Urban (N=64)"),
                    start = 0.1,
                    end = 0.9) +
  scale_x_discrete(breaks=c(1978,1983,1988,1993,1998,2003,2008,2013,2018),
                   labels=c("1978","1983","1988","1993","1998","2003","2008","2013","2018"))

################################################################################
#Figure of pretrial jail population rate by urbanicity over time
################################################################################

rural <- as.data.frame(tapply(data[which(data$urbanicity=="rural"),]$total_jail_pretrial_rate,data[which(data$urbanicity=="rural"),]$year,median,na.rm=TRUE)) 
rural <- rural %>% 
  rename(pop_median = c("tapply(data[which(data$urbanicity == \"rural\"), ]$total_jail_pretrial_rate, ")) %>%
  mutate(year = 1970:2018,
         urbanicity = "rural")
rownames(rural) <- NULL

small <- as.data.frame(tapply(data[which(data$urbanicity=="small/mid"),]$total_jail_pretrial_rate,data[which(data$urbanicity=="small/mid"),]$year,median,na.rm=TRUE)) 
small <- small %>% 
  rename(pop_median = c("tapply(data[which(data$urbanicity == \"small/mid\"), ]$total_jail_pretrial_rate, ")) %>%
  mutate(year = 1970:2018,
         urbanicity = "small")
rownames(small) <- NULL

suburban <- as.data.frame(tapply(data[which(data$urbanicity=="suburban"),]$total_jail_pretrial_rate,data[which(data$urbanicity=="suburban"),]$year,median,na.rm=TRUE)) 
suburban <- suburban %>% 
  rename(pop_median = c("tapply(data[which(data$urbanicity == \"suburban\"), ]$total_jail_pretrial_rate, ")) %>%
  mutate(year = 1970:2018,
         urbanicity = "suburban")
rownames(suburban) <- NULL

urban <- as.data.frame(tapply(data[which(data$urbanicity=="urban"),]$total_jail_pretrial_rate,data[which(data$urbanicity=="urban"),]$year,median,na.rm=TRUE))
urban <- urban %>% 
  rename(pop_median = c("tapply(data[which(data$urbanicity == \"urban\"), ]$total_jail_pretrial_rate, ")) %>%
  mutate(year = 1970:2018,
         urbanicity = "urban")
rownames(urban) <- NULL

pretrial_data <- rbind(rural,small,suburban,urban)
rm(rural,small,suburban,urban)

pretrial_data <- pretrial_data[which(pretrial_data$year>=1978),] %>%
  mutate(year = as.character(year))

g3 <- ggplot(pretrial_data, aes(x=year, 
                                y = pop_median, 
                                color = urbanicity,
                                group = urbanicity)) +
  geom_line(size = 2) +
  ylab("Pretrial Jail Population Rate") +
  ggtitle(element_blank()) +
  theme_bw() +
  theme(text = element_text(size = 15,
                            family = "serif",
                            color = "black"),
        legend.title=element_blank(),
        legend.position="none",
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, color = "black"),
        axis.text.y = element_text(color = "black")) +
  scale_colour_grey(labels=c("Rural (N=1976)",
                             "Small/mid metro (N=730)",
                             "Suburban (N=368)",
                             "Urban (N=64)"),
                    start = 0.1,
                    end = 0.9) +
  scale_x_discrete(breaks=c(1978,1983,1988,1993,1998,2003,2008,2013,2018),
                   labels=c("1978","1983","1988","1993","1998","2003","2008","2013","2018"))

################################################################################
#Figure of black/white jail population rate ratio by urbanicity over time
################################################################################

rural <- as.data.frame(tapply(data[which(data$urbanicity=="rural"),]$bw,data[which(data$urbanicity=="rural"),]$year,median,na.rm=TRUE)) 
rural <- rural %>% 
  rename(bw = c("tapply(data[which(data$urbanicity == \"rural\"), ]$bw, data[which(data$urbanicity == ")) %>%
  mutate(year = 1970:2018,
         urbanicity = "rural")
rownames(rural) <- NULL

small <- as.data.frame(tapply(data[which(data$urbanicity=="small/mid"),]$bw,data[which(data$urbanicity=="small/mid"),]$year,median,na.rm=TRUE)) 
small <- small %>% 
  rename(bw = c("tapply(data[which(data$urbanicity == \"small/mid\"), ]$bw, data[which(data$urbanicity == ")) %>%
  mutate(year = 1970:2018,
         urbanicity = "small")
rownames(small) <- NULL

suburban <- as.data.frame(tapply(data[which(data$urbanicity=="suburban"),]$bw,data[which(data$urbanicity=="suburban"),]$year,median,na.rm=TRUE)) 
suburban <- suburban %>% 
  rename(bw = c("tapply(data[which(data$urbanicity == \"suburban\"), ]$bw, data[which(data$urbanicity == ")) %>%
  mutate(year = 1970:2018,
         urbanicity = "suburban")
rownames(suburban) <- NULL

urban <- as.data.frame(tapply(data[which(data$urbanicity=="urban"),]$bw,data[which(data$urbanicity=="urban"),]$year,median,na.rm=TRUE))
urban <- urban %>% 
  rename(bw = c("tapply(data[which(data$urbanicity == \"urban\"), ]$bw, data[which(data$urbanicity == ")) %>%
  mutate(year = 1970:2018,
         urbanicity = "urban")
rownames(urban) <- NULL

bw_data <- rbind(rural,small,suburban,urban)
rm(rural,small,suburban,urban)

bw_data <- bw_data[which(bw_data$year>=1990),] %>%
  mutate(year = as.character(year))

g4 <- ggplot(bw_data, aes(x=year, 
                           y = bw, 
                           color = urbanicity,
                           group = urbanicity)) +
  geom_line(size = 2) +
  ylab("Black/White Jail Pop. Rate Ratio") +
  ggtitle(element_blank()) +
  theme_bw() +
  theme(text = element_text(size = 15,
                            family = "serif",
                            color = "black"),
        legend.title=element_blank(),
        legend.position="none",
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, color = "black"),
        axis.text.y = element_text(color = "black")) +
  scale_colour_grey(labels=c("Rural (N=1976)",
                             "Small/mid metro (N=730)",
                             "Suburban (N=368)",
                             "Urban (N=64)"),
                    start = 0.1,
                    end = 0.9) +
  scale_x_discrete(breaks=c(1990,1995,2000,2005,2010,2015,2018),
                   labels=c("1990","1995","2000","2005","2010","2015","2018"))

################################################################################
# Create plot
################################################################################
plot_grid(g1, g2, g3, g4, 
          ncol = 2)
