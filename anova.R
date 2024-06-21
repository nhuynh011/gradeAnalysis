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

# H0 is that screentime hasn't changed after COVID-19 (mean of all year's screentime is the same)
# HA: there is some sort of change in screentime after COVID-19

# Normality
pirateplot(screentime ~ year, data = all, inf.method = "ci", inf.disp = "line")
abline(h = mean(all$screentime), lwd = 2, col = "green", lty = 2) # Adds overall mean to plot
#So I think this is showing signs of lot of outliers on the upper side. Should I eliminate by year or by range? Or just overall??

# Remove outliers
Q <- quantile(all$screentime, probs=c(.25, .75), na.rm = TRUE)
iqr <- IQR(all$screentime, na.rm = TRUE)
allrm <- subset(all, all$screentime > (Q[1] - 1.5*iqr) & all$screentime < (Q[2]+1.5*iqr))

pirateplot(screentime ~ year, data = allrm, inf.method = "ci", inf.disp = "line")
abline(h = mean(allrm$screentime), lwd = 2, col = "green", lty = 2)
# I think the cutoff for this is pretty reasonable.

# Check residuals (do the residuals have to be normal too?):
par(mfrow = c(2, 2)); plot(lm(screentime ~ year, data = allrm))
#There's a pattern here, makes it look like Chi Sq dist. (right tail doesn't go to 0 completely)
#Also some pretty influential points (but I don't want to remove them), maybe remove point 63?
allrm[63, ]

# Sample independence (all good)
# Equal variance in all groups **
favstats(screentime ~ year, data = all)

# Screentime continuous (kinda is)

#LM for ANOVA:
lm <- lm(screentime ~ year, data = allrm)
anova(lm)

# Yes significance, there is a screentime mean that is different

### 2022, 2023, 2024
# To show that post-COVID-19 screentime was different from pre-COVID-19, I want to show that post-COVID-19 screentime has relatively the same mean

# H0: Mean of screentime is same for post-COVID-19 years
# HA: There is at least 1 year that has a different mean for screentime compared to the others

# Filter out 2018
post<- allrm|> filter(year != 18)

# Normality
pirateplot(screentime ~ year, data = postrm, inf.method = "ci", inf.disp = "line")
abline(h = mean(postrm$screentime), lwd = 2, col = "green", lty = 2)
# Reasonable for now.

# Check residuals (do the residuals have to be normal too?):
par(mfrow = c(2, 2)); plot(lm(screentime ~ year, data = postrm))
# I think there is an issue here...

# Sample independence (all good)
# Equal variance in all groups **
favstats(screentime ~ year, data = postrm)

# Screentime continuous

#LM for ANOVA:
lm2 <- lm(screentime ~ year, data = postrm)
anova(lm2)
#No diff here

### HSD after?