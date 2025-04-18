### ANOVA analysis of screentime for 2018, 2022, 2023, 2024 and poll results
# Load lib
library(tidyverse)
library(here)
library(mosaic)
library(readxl)
library(yarrr)

# Load files
poll<- read.csv("data", "poll2023.csv")
all <- read_xlsx(here("data", "all.xlsx"))

all$year <- as.factor(all$year)

###############################################################################
### 2018 vs 2022, 2023, 2024

# H0 is that screentime hasn't changed after COVID-19 (mean of all year's screentime is the same)
# HA: there is some sort of change in screentime after COVID-19

# Normality
pirateplot(screentime ~ year, data = all, inf.method = "ci", inf.disp = "line")
abline(h = mean(all$screentime), lwd = 2, col = "green", lty = 2) # Adds overall mean to plot


# Remove outliers
Q <- quantile(all$screentime, probs=c(.25, .75), na.rm = TRUE)
iqr <- IQR(all$screentime, na.rm = TRUE)
allrm <- subset(all, all$screentime > (Q[1] - 1.5*iqr) & all$screentime < (Q[2]+1.5*iqr))

pirateplot(screentime ~ year, data = allrm, inf.method = "ci", inf.disp = "line")
abline(h = mean(allrm$screentime), lwd = 2, col = "green", lty = 2)

# Visualize distribution
library(ggpubr)
ggdensity(allrm$screentime, 
          main = "Density plot of screentime",
          xlab = "Screentime")
# QQ Plots
ggqqplot(allrm$screentime)

# Normality: Shapiro Wilk test, Kolmogorov Smirnov test, want small p val
library(wakefield)
normall <- normal(nrow(allrm), mean = mean(allrm$screentime), sd = sd(allrm$screentime), min = NULL, max = NULL, name = "Normal")
shapiro.test(allrm$screentime)
ks.test(all$screentime, normall)

# Test for heterogeneity, Levene and Bartlett's test
# Null is all population variances are equal
# Else at least 2 of them differ (low p val)
library(car)
leveneTest(screentime ~ as.factor(year), allrm) #Fails this too
bartlett.test(screentime~ as.factor(year), allrm) # passes this..?

# Check residuals (do the residuals have to be normal too?):
par(mfrow = c(2, 2)); plot(lm(screentime ~ year, data = allrm))

#There's a pattern here, makes it look like Chi Sq dist. (right tail doesn't go to 0 completely)
#Also some pretty influential points (but I don't want to remove them), maybe remove point 63?
allrm[63, ]

# Perm ANOVA:
library(lmPerm)
allrm$year <- as.factor(allrm$year)
plm <- lmp(screentime ~ year, allrm,  perm="Exact")
anova(plm)
#LM for ANOVA:
lm <- lm(screentime ~ year, data = allrm)
anova(lm)

# Kruskal Wallis test:
kruskal.test(screentime ~ year, data = allrm)

### 2022, 2023, 2024
# To show that post-COVID-19 screentime was different from pre-COVID-19, I want to show that post-COVID-19 screentime has relatively the same mean

# H0: Mean of screentime is same for post-COVID-19 years
# HA: There is at least 1 year that has a different mean for screentime compared to the others

# Filter out 2018
postrm<- allrm|> filter(year != 18)

# Normality
pirateplot(screentime ~ year, data = postrm, inf.method = "ci", inf.disp = "line")
abline(h = mean(postrm$screentime), lwd = 2, col = "green", lty = 2)

# Check residuals (do the residuals have to be normal too?):
par(mfrow = c(2, 2)); plot(lm(screentime ~ year, data = postrm))

# Sample independence (all good)
# Equal variance in all groups **
favstats(screentime ~ year, data = postrm)

#LM for ANOVA:
lm2 <- lm(screentime ~ year, data = postrm)
anova(lm2)

# Kruskal Wallis test:
kruskal.test(screentime ~ year, data = postrm)
#No diff here


###############################################################################
### 2018 vs 2022, 2023, 2024

# H0 is that gradecalc2 hasn't changed after COVID-19 (mean of all year's screentime is the same)
# HA: there is some sort of change in gradecalc2 after COVID-19

# Normality
pirateplot(gradecalc2 ~ year, data = all, inf.method = "ci", inf.disp = "line")
abline(h = mean(all$gradecalc2), lwd = 2, col = "green", lty = 2) # Adds overall mean to plot

# Remove outliers
Q <- quantile(all$gradecalc2, probs=c(.25, .75), na.rm = TRUE)
iqr <- IQR(all$gradecalc2, na.rm = TRUE)
allrm1 <- subset(all, all$gradecalc2 > (Q[1] - 1.5*iqr) & all$gradecalc2 < (Q[2]+1.5*iqr))

pirateplot(gradecalc2 ~ year, data = allrm1, inf.method = "ci", inf.disp = "line")
abline(h = mean(allrm1$gradecalc2), lwd = 2, col = "green", lty = 2)


# Check residuals (do the residuals have to be normal too?):
par(mfrow = c(2, 2)); plot(lm(gradecalc2 ~ year, data = allrm1))
postrm<- allrm1|> filter(year != 18)
#BAD


# Sample independence (all good)
# Equal variance in all groups **
favstats(gradecalc2 ~ year, data = allrm1)

#LM for ANOVA:
lmGrade <- lm(gradecalc2 ~ year, data = allrm1)
anova(lmGrade)
#Poor value

### HSD 
library(multcomp)
hsd <- glht(lm, linfct = mcp(year = "Tukey"))
confint(hsd)
old.par <- par(mai = c(1,2,1,1)) #Makes room on the plot for the group names
plot(hsd)

