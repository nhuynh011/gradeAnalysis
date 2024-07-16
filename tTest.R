# T test for diff eq sections, and year to year t test
# Load lib:
library(tidyverse)
library(here)
library(readxl)
library(dplyr)
library(mosaic)

#Load file:
all <- read_xlsx(here("all.xlsx"))

diffeq <- read_xlsx(here("diffeq.xlsx"))
diffeq <- diffeq[, !names(diffeq) %in% "noprereq"]

# Seperate data by section
diff1 <- diffeq|> filter(year == 23)
diff2 <- diffeq|> filter(time == "09:00:00")
diff3 <- diffeq|> filter(time == "12:00:00", year == 24)

# Remove na and correct columns
diff1<- na.omit(diff1[-c(2:5, 18:19)])
diff2<- na.omit(diff2[-c(2:5, 18:19)])
diff3<- na.omit(diff3[-c(2:5, 18:19)])

# Add more information for diffeq
diff1<- diff1 |> mutate(section = "12PM Spring 2023")
diff2<- diff2 |> mutate(section = "9AM Spring 2024")
diff3<- diff3 |> mutate(section = "12PM Spring 2024")

diffeqtotal <- rbind(diff1, diff2, diff3)

################################################################################
### Screentime
### Want to see if there is a difference between the years in screentime. Comparing all year combos
plot(screentime ~ as.factor(year), data=all)

# 2018 to 2022
aemCalc1 <- all|>filter(year == 18 | year == 22) 
#Remove outliers:
library(ggstatsplot)
Q <- quantile(aemCalc1$screentime, probs=c(.25, .75), na.rm = TRUE)
iqr <- IQR(aemCalc1$screentime, na.rm = TRUE)
aemCalc <- subset(aemCalc1, aemCalc1$screentime > (Q[1] - 1.5*iqr) & aemCalc1$screentime < (Q[2]+1.5*iqr))
t.test(screentime ~ as.factor(year), data = aemCalc)

# 2018 and 2023
aemCalc2 <- all|>filter(year == 18 | year == 23) 
Q <- quantile(aemCalc2$screentime, probs=c(.25, .75), na.rm = TRUE)
iqr <- IQR(aemCalc2$screentime, na.rm = TRUE)
aemCalc <- subset(aemCalc2, aemCalc2$screentime > (Q[1] - 1.5*iqr) & aemCalc2$screentime < (Q[2]+1.5*iqr))
t.test(screentime ~ as.factor(year), data = aemCalc)

# 2018 and 2024
aemCalc3 <- all|>filter(year == 18 | year == 24) 
Q <- quantile(aemCalc3$screentime, probs=c(.25, .75), na.rm = TRUE)
iqr <- IQR(aemCalc3$screentime, na.rm = TRUE)
aemCalc <- subset(aemCalc3, aemCalc3$screentime > (Q[1] - 1.5*iqr) & aemCalc3$screentime < (Q[2]+1.5*iqr))
t.test(screentime ~ as.factor(year), data = aemCalc)

# 2022 to 2023
diffcalc1 <- all|>filter(year == 22 | year == 23) 
Q <- quantile(diffcalc1$screentime, probs=c(.25, .75), na.rm = TRUE)
iqr <- IQR(diffcalc1$screentime, na.rm = TRUE)
diffcalc <- subset(diffcalc1, diffcalc1$screentime > (Q[1] - 1.5*iqr) & diffcalc1$screentime < (Q[2]+1.5*iqr))
t.test(screentime ~ as.factor(year), data = diffcalc)

# 2023 to 2024
diffcalc2 <- all|>filter(year == 24 | year == 23) 
Q <- quantile(diffcalc2$screentime, probs=c(.25, .75), na.rm = TRUE)
iqr <- IQR(diffcalc2$screentime, na.rm = TRUE)
diffcalc <- subset(diffcalc2, diffcalc2$screentime > (Q[1] - 1.5*iqr) & diffcalc2$screentime < (Q[2]+1.5*iqr))
t.test(screentime ~ as.factor(year), data = diffcalc)

# 2022 to 2024
diffcalc3 <- all|>filter(year == 22 | year == 24) 
Q <- quantile(diffcalc3$screentime, probs=c(.25, .75), na.rm = TRUE)
iqr <- IQR(diffcalc3$screentime, na.rm = TRUE)
diffcalc <- subset(diffcalc3, diffcalc3$screentime > (Q[1] - 1.5*iqr) & diffcalc3$screentime < (Q[2]+1.5*iqr))
t.test(screentime ~ as.factor(year), data = diffcalc)

# One sample t-test for the newfound avg screentime for teens being 4.8 hours: https://news.gallup.com/poll/512576/teens-spend-average-hours-social-media-per-day.aspx?utm_source=chartr&utm_medium=email&utm_campaign=chartr_20240617
# 2023 vs 2023 avg:
y23 <- all|>filter(year ==23)
Q <- quantile(y23$screentime, probs=c(.25, .75), na.rm = TRUE)
iqr <- IQR(y23$screentime, na.rm = TRUE)
y23 <- subset(y23, y23$screentime > (Q[1] - 1.5*iqr) & y23$screentime < (Q[2]+1.5*iqr))
t.test(y23$screentime, mu = 4.8) #interesting!

# Gradecalc2
# Comparing gradecalc2 for some years only. Wanted to show a difference from pre-COVID to post-COVID
plot(gradecalc2 ~ as.factor(year), data=all)

# 2018 to 2022
Q <- quantile(aemCalc1$gradecalc2, probs=c(.25, .75), na.rm = TRUE)
iqr <- IQR(aemCalc1$gradecalc2, na.rm = TRUE)
aemCalc <- subset(aemCalc1, aemCalc1$gradecalc2 > (Q[1] - 1.5*iqr) & aemCalc1$gradecalc2 < (Q[2]+1.5*iqr))
t.test(gradecalc2 ~ as.factor(year), data = aemCalc)

# Gradecalc2
# Comparing gradecalc2 for some years only. Wanted to show a difference from pre-COVID to post-COVID
plot(gradecalc2 ~ as.factor(year), data=all)

# 2018 to 2022
Q <- quantile(aemCalc1$gradecalc2, probs=c(.25, .75), na.rm = TRUE)
iqr <- IQR(aemCalc1$gradecalc2, na.rm = TRUE)
aemCalc <- subset(aemCalc1, aemCalc1$gradecalc2 > (Q[1] - 1.5*iqr) & aemCalc1$gradecalc2 < (Q[2]+1.5*iqr))
t.test(gradecalc2 ~ as.factor(year), data = aemCalc)

# 2018 to 2023 ***
aemCalc2 <- all|>filter(year == 18 | year == 23) 

Q <- quantile(aemCalc2$gradecalc2, probs=c(.25, .75), na.rm = TRUE)
iqr <- IQR(aemCalc2$gradecalc2, na.rm = TRUE)
aemCalc <- subset(aemCalc2, aemCalc2$gradecalc2 > (Q[1] - 1.5*iqr) & aemCalc2$gradecalc2 < (Q[2]+1.5*iqr))
t.test(gradecalc2 ~ as.factor(year), data = aemCalc)

# 2018 to 2024
aemCalc3 <- all|>filter(year == 18 | year == 24) 

Q <- quantile(aemCalc3$gradecalc2, probs=c(.25, .75), na.rm = TRUE)
iqr <- IQR(aemCalc3$gradecalc2, na.rm = TRUE)
aemCalc <- subset(aemCalc3, aemCalc3$gradecalc2 > (Q[1] - 1.5*iqr) & aemCalc3$gradecalc2 < (Q[2]+1.5*iqr))
t.test(gradecalc2 ~ as.factor(year), data = aemCalc)


# Any difference between the years?
# 2022 to 2023
CalcDiff1 <- all|>filter(year == 22 | year == 23) 
Q <- quantile(CalcDiff1$gradecalc2, probs=c(.25, .75), na.rm = TRUE)
iqr <- IQR(CalcDiff1$gradecalc2, na.rm = TRUE)
CalcDiff <- subset(CalcDiff1, CalcDiff1$gradecalc2 > (Q[1] - 1.5*iqr) & CalcDiff1$gradecalc2 < (Q[2]+1.5*iqr))
t.test(gradecalc2 ~ as.factor(year), data = CalcDiff)

# 2023 to 2024
CalcDiff2 <- all|>filter(year == 24 | year == 23) 
Q <- quantile(CalcDiff2$gradecalc2, probs=c(.25, .75), na.rm = TRUE)
iqr <- IQR(CalcDiff2$gradecalc2, na.rm = TRUE)
CalcDiff <- subset(CalcDiff2, CalcDiff2$gradecalc2 > (Q[1] - 1.5*iqr) & CalcDiff2$gradecalc2 < (Q[2]+1.5*iqr))
t.test(gradecalc2 ~ as.factor(year), data = CalcDiff)

# 2022 to 2024
CalcDiff3 <- all|>filter(year == 22 | year == 24) 
Q <- quantile(CalcDiff3$gradecalc2, probs=c(.25, .75), na.rm = TRUE)
iqr <- IQR(CalcDiff3$gradecalc2, na.rm = TRUE)
CalcDiff <- subset(CalcDiff3, CalcDiff3$gradecalc2 > (Q[1] - 1.5*iqr) & CalcDiff3$gradecalc2 < (Q[2]+1.5*iqr))
t.test(gradecalc2 ~ as.factor(year), data = CalcDiff)

# Gradediffeq for diffeq only, calc3 survey didn't ask this.
plot(gradediffeq ~ as.factor(year), data=diffeqtotal)

# 2023 to 2024 (both section)
Q <- quantile(diffeqtotal$gradediffeq, probs=c(.25, .75), na.rm = TRUE)
iqr <- IQR(diffeqtotal$gradediffeq, na.rm = TRUE)
Diff <- subset(diffeqtotal, diffeqtotal$gradediffeq > (Q[1] - 1.5*iqr) & diffeqtotal$gradediffeq < (Q[2]+1.5*iqr))
t.test(gradediffeq ~ as.factor(year), data = Diff)

# 2023 section 1 to 2024 section 2 at 9am
diff12 <- diffeqtotal|>filter(section == "12PM Spring 2023" | section == "9AM Spring 2024")
Q <- quantile(diff12$gradediffeq, probs=c(.25, .75), na.rm = TRUE)
iqr <- IQR(diff12$gradediffeq, na.rm = TRUE)
Diff <- subset(diff12, diff12$gradediffeq > (Q[1] - 1.5*iqr) & diff12$gradediffeq < (Q[2]+1.5*iqr))
t.test(gradediffeq ~ as.factor(section), data = Diff)

# 2023 section 1 to 2024 section 3 at 12PM
diff13 <- diffeqtotal|>filter(section == "12PM Spring 2023" | section == "12PM Spring 2024")
Q <- quantile(diff13$gradediffeq, probs=c(.25, .75), na.rm = TRUE)
iqr <- IQR(diff13$gradediffeq, na.rm = TRUE)
Diff <- subset(diff13, diff13$gradediffeq > (Q[1] - 1.5*iqr) & diff13$gradediffeq < (Q[2]+1.5*iqr))
t.test(gradediffeq ~ as.factor(section), data = Diff)

# 2024 sections
diff23 <- diffeqtotal|>filter(section == "9AM Spring 2024" | section == "12PM Spring 2024")
Q <- quantile(diff23$gradediffeq, probs=c(.25, .75), na.rm = TRUE)
iqr <- IQR(diff23$gradediffeq, na.rm = TRUE)
Diff <- subset(diff23, diff23$gradediffeq > (Q[1] - 1.5*iqr) & diff23$gradediffeq < (Q[2]+1.5*iqr))
t.test(gradediffeq ~ as.factor(section), data = Diff)

# Study hours
# Testing for difference in study hours. Observed in the bar charts earlier in prelim.R
calc3total <- read_xlsx(here("calc3.xlsx"))
calc3total <- calc3total[, !names(calc3total) %in% "noprereq"]

# Bind with diffeq
cols3 <- colnames(select(diffeqtotal, -c("gradediffeq", "section")))
postCOVID <- rbind(
  subset(calc3total, select = cols3), 
  subset(diffeqtotal, select = cols3)
)
plot(studyhours ~ as.factor(year), data = postCOVID)

# 2022 to 2023
post1 <- postCOVID|>filter(year == 22 | year == 23)
Q <- quantile(post1$studyhours, probs=c(.25, .75), na.rm = TRUE)
iqr <- IQR(post1$studyhours, na.rm = TRUE)
post <- subset(post1, post1$studyhours > (Q[1] - 1.5*iqr) & post1$studyhours < (Q[2]+1.5*iqr))
t.test(studyhours ~ as.factor(year), data = post)

# 2023 to 2024
post2 <- postCOVID|>filter(year == 24 | year == 23)
Q <- quantile(post2$studyhours, probs=c(.25, .75), na.rm = TRUE)
iqr <- IQR(post2$studyhours, na.rm = TRUE)
post <- subset(post2, post2$studyhours > (Q[1] - 1.5*iqr) & post2$studyhours < (Q[2]+1.5*iqr))
t.test(studyhours ~ as.factor(year), data = post)

# 2022 to 2024
post3 <- postCOVID|>filter(year == 22 | year == 24)
Q <- quantile(post3$studyhours, probs=c(.25, .75), na.rm = TRUE)
iqr <- IQR(post3$studyhours, na.rm = TRUE)
post <- subset(post3, post3$studyhours > (Q[1] - 1.5*iqr) & post3$studyhours < (Q[2]+1.5*iqr))
t.test(studyhours ~ as.factor(year), data = post)

