# BEGINNING ANALYSIS
# Doing correlation graph and some linear models

library(tidyverse)
library(here)
library(readxl)
library(dplyr)
library(mosaic)
library(GGally)

calc3 <- read_xlsx(here("data", "calc3.xlsx"))
diffeq <- read_xlsx(here("data", "diffeq.xlsx"))
aem <- read_xlsx(here("data", "datafor_r.xlsx"))
stat <- read_xlsx(here("data", "stat.xlsx"))

# Remove noprereq, and others we don't need for now
calc3 <- calc3[, !names(calc3) %in% "noprereq"]
diffeq <- diffeq[, !names(diffeq) %in% "noprereq"]
stat <- stat[, !names(stat) %in% "noprereq"]
calcpre<- na.omit(calc3[-c(1:5, 20:21)])
diffpre<- na.omit(diffeq[-c(1:5, 18:19)])
statpre <- na.omit(stat[-c(1:5, 18:19)])

### Calc3
# screentime
Q <- quantile(calcpre$screentime, probs=c(.25, .75), na.rm = TRUE)
iqr <- IQR(calcpre$screentime, na.rm = TRUE)
calc3rm1 <- subset(calcpre, calcpre$screentime > (Q[1] - 1.5*iqr) & calcpre$screentime < (Q[2]+1.5*iqr))

# study hours
Q <- quantile(calcpre$studyhours, probs=c(.25, .75), na.rm = TRUE)
iqr <- IQR(calcpre$studyhours, na.rm = TRUE)
calc3rm2 <- subset(calcpre, calcpre$studyhours > (Q[1] - 1.5*iqr) & calcpre$studyhours < (Q[2]+1.5*iqr))

#Combine the things in common between the two
calc3rm <- inner_join(calc3rm1, calc3rm2)

calc3rm<- calc3rm |> relocate(gradecalc3)

# Column stats
cmean <- apply(calc3rm, 2, mean)
cdev <- apply(calc3rm, 2, sd)
cmin <- apply(calc3rm, 2, min)
cmax <- apply(calc3rm, 2, max)

### DiffEq
# screentime
Q <- quantile(diffpre$screentime, probs=c(.25, .75), na.rm = TRUE)
iqr <- IQR(diffpre$screentime, na.rm = TRUE)
diffeqrm1 <- subset(diffpre, diffpre$screentime > (Q[1] - 1.5*iqr) & diffpre$screentime < (Q[2]+1.5*iqr))

# study hours
Q <- quantile(diffpre$studyhours, probs=c(.25, .75), na.rm = TRUE)
iqr <- IQR(diffpre$studyhours, na.rm = TRUE)
diffeqrm2 <- subset(diffpre, diffpre$studyhours > (Q[1] - 1.5*iqr) & diffpre$studyhours < (Q[2]+1.5*iqr))

#Combine the things in common between the two
diffeqrm <- inner_join(diffeqrm1, diffeqrm2)

diffeqrm <- diffeqrm |> relocate(gradediffeq)

# Column stats
dmean <- apply(diffeqrm, 2, mean)
ddev <- apply(diffeqrm, 2, sd)
dmin <- apply(diffeqrm, 2, min)
dmax <- apply(diffeqrm, 2, max)

### Stat
# Screentime
Q <- quantile(statpre$screentime, probs=c(.25, .75), na.rm = TRUE)
iqr <- IQR(statpre$screentime, na.rm = TRUE)
statrm1 <- subset(statpre, statpre$screentime > (Q[1] - 1.5*iqr) & statpre$screentime < (Q[2]+1.5*iqr))

# study hours
Q <- quantile(statpre$studyhours, probs=c(.25, .75), na.rm = TRUE)
iqr <- IQR(statpre$studyhours, na.rm = TRUE)
statrm2 <- subset(statpre, statpre$studyhours > (Q[1] - 1.5*iqr) & statpre$studyhours < (Q[2]+1.5*iqr))

#Combine the things in common between the two
statrm <- inner_join(statrm1, statrm2)

statrm <- statrm |> relocate(gradestat)

# Column stats
smean <- apply(statrm, 2, mean)
sdev <- apply(statrm, 2, sd)
smin <- apply(statrm, 2, min)
smax <- apply(statrm, 2, max)

################################################################################
### Corr plots
ggpairs(aem)
ggpairs(calc3rm)
ggpairs(diffeqrm)
ggpairs(statrm)

################################################################################
### Stepwise Model
# CALC3
forward <- lm(gradecalc3 ~ 1, data = calc3rm)
calc3model2022 <- step(forward, direction = "forward", scope = formula(lm(gradecalc3 ~ ., data = calc3rm)))
summary(calc3model2022)
confint(calc3model2022)

back <- lm(gradecalc3 ~ ., data = calc3rm)
backlm <- step(back, direction = "backward", trace = 0)
summary(backlm)

both <- lm(gradecalc3 ~ ., data = calc3rm)
summary(both)
bothlm <- step(both, direction = "both", trace = 0)
summary(bothlm) # confirmed all are the same

# AEM
aem <- na.omit(aem[-c(6, 9, 13:16)])

# Column stats
amean <- apply(aem, 2, mean)
adev <- apply(aem, 2, sd)
amin <- apply(aem, 2, min)
amax <- apply(aem, 2, max)

forward <- lm(AEM ~ 1, data = aem)
aemmodel2018 <- step(forward, direction = "forward", scope = formula(lm(AEM ~ ., data = aem)))
summary(aemmodel2018)

back <- lm(AEM ~ ., data = aem)
backlm <- step(back, direction = "backward", trace = 0)
summary(backlm)

both <- lm(AEM ~ ., data = aem)
summary(both)
bothlm <- step(both, direction = "both", trace = 0)
summary(bothlm) # confirmed all same
confint(aemmodel2018)

# DIFFEQ (2024)
diff2024 <- diffeq |> filter(year == "24")
diff2024 <- na.omit(diff2024[-c(1:5, 18:19)])

# Remove outliers
# Study hours:
Q <- quantile(diff2024$studyhours, probs=c(.25, .75), na.rm = TRUE)
iqr <- IQR(diff2024$studyhours, na.rm = TRUE)
diffeq2024_1 <- subset(diff2024, diff2024$studyhours > (Q[1] - 1.5*iqr) & diff2024$studyhours < (Q[2]+1.5*iqr))

# screentime:
Q <- quantile(diff2024$screentime, probs=c(.25, .75), na.rm = TRUE)
iqr <- IQR(diff2024$screentime, na.rm = TRUE)
diffeq2024_2 <- subset(diff2024, diff2024$screentime > (Q[1] - 1.5*iqr) & diff2024$screentime < (Q[2]+1.5*iqr))

# Combine
diffeq2024 <- inner_join(diffeq2024_1, diffeq2024_2)

# Column stats
dmean <- apply(diffeq2024, 2, mean)
ddev <- apply(diffeq2024, 2, sd)
dmin <- apply(diffeq2024, 2, min)
dmax <- apply(diffeq2024, 2, max)

# Stepwise
forward <- lm(gradediffeq ~ 1, data = diffeq2024)
diffeqmodel24 <- step(forward, direction = "forward", scope = formula(lm(gradediffeq ~ ., data = diffeq2024)))
summary(diffeqmodel24)
confint(diffeqmodel24)

back <- lm(gradediffeq ~ ., data = diffeq2024)
backlm <- step(back, direction = "backward", trace = 0)
summary(backlm)

both <- lm(gradediffeq ~ ., data = diffeq2024)
summary(both)
bothlm <- step(both, direction = "both", trace = 0)
summary(bothlm)

# STAT (2024)
# Stepwise
forward <- lm(gradestat ~ 1, data = statrm)
statmodel2024 <- step(forward, direction = "forward", scope = formula(lm(gradestat ~ ., data = statrm)))
summary(statmodel2024)
confint(statmodel2024)

back <- lm(gradestat ~ ., data = statrm)
backlm <- step(back, direction = "backward", trace = 0)
summary(backlm)

both <- lm(gradestat ~ ., data = statrm)
summary(both)
bothlm <- step(both, direction = "both", trace = 0)
summary(bothlm)
################################################################################
### UNUSED MODELS
# DIFFEQ (2023)
# Didn't use this one
diff2023 <- diffeq |> filter(year == "23")
diff2023 <- na.omit(diff2023[-c(1:5, 18:19)])

# Remove outliers
#Study hours
Q <- quantile(diff2023$studyhours, probs=c(.25, .75), na.rm = TRUE)
iqr <- IQR(diff2023$studyhours, na.rm = TRUE)
diffeq2023_1 <- subset(diff2023, diff2023$studyhours > (Q[1] - 1.5*iqr) & diff2023$studyhours < (Q[2]+1.5*iqr)) #save over original

#Screentime
Q <- quantile(diff2023$screentime, probs=c(.25, .75), na.rm = TRUE)
iqr <- IQR(diff2023$screentime, na.rm = TRUE)
diffeq2023_2 <- subset(diff2023, diff2023$screentime > (Q[1] - 1.5*iqr) & diff2023$screentime < (Q[2]+1.5*iqr)) #save over original

# Combine
diffeq2023 <- inner_join(diffeq2023_1, diffeq2023_2)

### Linear model (2023)
forward <- lm(gradediffeq ~ 1, data = diffeq2023)
diffeqmodel23 <- step(forward, direction = "forward", scope = formula(lm(gradediffeq ~ ., data = diffeq2023)))
summary(diffeqmodel23)

back <- lm(gradediffeq ~ ., data = diffeq2023)
backlm <- step(back, direction = "backward", trace = 0)
summary(backlm)

both <- lm(gradediffeq ~ ., data = diffeq2023)
summary(both)
bothlm <- step(both, direction = "both", trace = 0)
summary(bothlm)
# confirmed all the same

# DIFFEQ (both years)
# We don't use this one
forward <- lm(gradediffeq ~ 1, data = diffeqrm)
diffeqrmmodel <- step(forward, direction = "forward", scope = formula(lm(gradediffeq ~ ., data = diffeqrm)))
summary(diffeqrmmodel)

back <- lm(gradediffeq ~ ., data = diffeqrm)
backlm <- step(back, direction = "backward", trace = 0)
summary(backlm)

both <- lm(gradediffeq ~ ., data = diffeqrm)
summary(both)
bothlm <- step(both, direction = "both", trace = 0)
summary(bothlm) #confirmed all 3 are same

### Stepwise model for each diffeq section
# Make a different model for each of the semesters (applies to diffeq only)
diff1 <- diffeq|> filter(year == 23)
diff2 <- diffeq|> filter(time == "09:00:00")
diff3 <- diffeq|> filter(time == "12:00:00", year == 24)

# Remove na and correct columns
diff1<- na.omit(diff1[-c(1:5, 18:19)])
diff2<- na.omit(diff2[-c(1:5, 18:19)])
diff3<- na.omit(diff3[-c(1:5, 18:19)])

# Correlation plots
ggpairs(diff1)
ggpairs(diff2)
ggpairs(diff3)

### Diff1 model after removing both sets of outliers:
# study hours
Q <- quantile(diff1$studyhours, probs=c(.25, .75), na.rm = TRUE)
iqr <- IQR(diff1$studyhours, na.rm = TRUE)
diff1rm1 <- subset(diff1, diff1$studyhours > (Q[1] - 1.5*iqr) & diff1$studyhours < (Q[2]+1.5*iqr))

# screentime
Q <- quantile(diff1$screentime, probs=c(.25, .75), na.rm = TRUE)
iqr <- IQR(diff1$screentime, na.rm = TRUE)
diff1rm2 <- subset(diff1, diff1$screentime > (Q[1] - 1.5*iqr) & diff1$screentime < (Q[2]+1.5*iqr))

# Combine
diff1rm <- inner_join(diff1rm1, diff1rm2)

forward <- lm(gradediffeq ~ 1, data = diff1rm)
diffeq1rmmodel <- step(forward, direction = "forward", scope = formula(lm(gradediffeq ~ ., data = diff1rm)))
summary(diffeq1rmmodel)

back <- lm(gradediffeq ~ ., data = diff1rm)
backlm <- step(back, direction = "backward", trace = 0)
summary(backlm)

both <- lm(gradediffeq ~ ., data = diff1rm)
summary(both)
bothlm <- step(both, direction = "both", trace = 0)
summary(bothlm)

### Diff2 Model (9AM Spring 2024) removed outliers:
# study hours
Q <- quantile(diff2$studyhours, probs=c(.25, .75), na.rm = TRUE)
iqr <- IQR(diff2$studyhours, na.rm = TRUE)
diff2rm1 <- subset(diff2, diff2$studyhours > (Q[1] - 1.5*iqr) & diff2$studyhours < (Q[2]+1.5*iqr))

# screentime
Q <- quantile(diff2$screentime, probs=c(.25, .75), na.rm = TRUE)
iqr <- IQR(diff2$screentime, na.rm = TRUE)
diff2rm2 <- subset(diff2, diff2$screentime > (Q[1] - 1.5*iqr) & diff2$screentime < (Q[2]+1.5*iqr))

# Combine
diff2rm <- inner_join(diff2rm1, diff2rm2)

forward <- lm(gradediffeq ~ 1, data = diff2rm)
diffeq2rmmodel <- step(forward, direction = "forward", scope = formula(lm(gradediffeq ~ ., data = diff2rm)))
summary(diffeq2rmmodel)

back <- lm(gradediffeq ~ ., data = diff2rm)
backlm <- step(back, direction = "backward", trace = 0)
summary(backlm)

both <- lm(gradediffeq ~ ., data = diff2rm)
summary(both)
bothlm <- step(both, direction = "both", trace = 0)
summary(bothlm)

# Diff3 Model (12PM Spring 2024) removed outliers:
# study hours
Q <- quantile(diff3$studyhours, probs=c(.25, .75), na.rm = TRUE)
iqr <- IQR(diff3$studyhours, na.rm = TRUE)
diff3rm1 <- subset(diff3, diff3$studyhours > (Q[1] - 1.5*iqr) & diff3$studyhours < (Q[2]+1.5*iqr))

# screentime
Q <- quantile(diff3$screentime, probs=c(.25, .75), na.rm = TRUE)
iqr <- IQR(diff3$screentime, na.rm = TRUE)
diff3rm2 <- subset(diff3, diff3$screentime > (Q[1] - 1.5*iqr) & diff3$screentime < (Q[2]+1.5*iqr))

# Combine
diff3rm <- inner_join(diff3rm1, diff3rm2)

forward <- lm(gradediffeq ~ 1, data = diff3rm)
diffeq3rmmodel <- step(forward, direction = "forward", scope = formula(lm(gradediffeq ~ ., data = diff3rm)))
summary(diffeq3rmmodel)

back <- lm(gradediffeq ~ ., data = diff3rm)
backlm <- step(back, direction = "backward", trace = 0)
summary(backlm)

both <- lm(gradediffeq ~ ., data = diff3rm)
summary(both)
bothlm <- step(both, direction = "both", trace = 0)
summary(bothlm)

