### ANOVA analysis of screentime for 2018, 2022, 2023, 2024 and poll results
# Load lib
library(tidyverse)
library(here)
library(mosaic)
library(readxl)
library(yarrr)

# Load files
poll<- read.csv("poll2023.csv")
all <- read_xlsx(here("all.xlsx"))

###############################################################################
### 2018 vs 2022, 2023, 2024
# One way ANOVA assumptions:

# Normality
pirateplot(screentime ~ year, data = all, inf.method = "ci", inf.disp = "line")
abline(h = mean(all$screentime), lwd = 2, col = "green", lty = 2) # Adds overall mean to plot

# Sample independence (all good)
# Equal variance in all groups **
favstats(screentime ~ year, data = all)

# Screentime continuous (kinda is)

# H0 is that screentime hasn't changed after COVID-19 (mean of all year's screentime is the same)
# HA: there is some sort of change in screentime after COVID-19

#LM for ANOVA:
