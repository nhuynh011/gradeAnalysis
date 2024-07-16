# BEGINNING ANALYSIS
# Doing correlation graph and some linear models

library(tidyverse)
library(here)
library(readxl)
library(dplyr)
library(mosaic)

calc3 <- read_xlsx(here("calc3.xlsx"))
diffeq <- read_xlsx(here("diffeq.xlsx"))
aem <- read_xlsx("datafor_r.xlsx")
# Check na.omit: removes any row with 1 or more na

# Remove noprereq, and others we don't need for now
calc3 <- calc3[, !names(calc3) %in% "noprereq"]
diffeq <- diffeq[, !names(diffeq) %in% "noprereq"]
calcrm<- na.omit(calc3[-c(1:5, 20:21)])
diffrm<- na.omit(diffeq[-c(1:5, 18:19)])

### Correlation coeff, warnings are na that haven't been removed yet
library(GGally)

#Want to remove outliers first for these:
# screentime
calciii<- as.data.frame(calc3) # There's an issue if I used calc3 as the name for outlier removal.
Q <- quantile(calciii$screentime, probs=c(.25, .75), na.rm = TRUE)
iqr <- IQR(calciii$screentime, na.rm = TRUE)
calc1 <- subset(calciii, calciii$screentime > (Q[1] - 1.5*iqr) & calciii$screentime < (Q[2]+1.5*iqr)) #save over original

# study hours
Q <- quantile(calciii$studyhours, probs=c(.25, .75), na.rm = TRUE)
iqr <- IQR(calciii$studyhours, na.rm = TRUE)
calc1 <- subset(calciii, calciii$studyhours > (Q[1] - 1.5*iqr) & calciii$studyhours < (Q[2]+1.5*iqr)) #save

# screentime
diffeq<- as.data.frame(diffeq)
Q <- quantile(diffeq$screentime, probs=c(.25, .75), na.rm = TRUE)
iqr <- IQR(diffeq$screentime, na.rm = TRUE)
diff <- subset(diffeq, diffeq$screentime > (Q[1] - 1.5*iqr) & diffeq$screentime < (Q[2]+1.5*iqr)) #save over original

# study hours
Q <- quantile(diffeq$studyhours, probs=c(.25, .75), na.rm = TRUE)
iqr <- IQR(diffeq$studyhours, na.rm = TRUE)
diff <- subset(diffeq, diffeq$studyhours > (Q[1] - 1.5*iqr) & diffeq$studyhours < (Q[2]+1.5*iqr)) #save

# AEM is the response
ggpairs(aem)

ggpairs(calc1[c(17, 5:16, 18:21)])

# gradediffeq is the response
ggpairs(diff[c(15, 5:14, 16:19)])

################################################################################
### Making the stepwise models (with outliers):
# Calc3
forward <- lm(gradecalc3 ~ 1, data = calcrm)
calc3model <- step(forward, direction = "forward", scope = formula(lm(gradecalc3 ~ ., data = calcrm)))
summary(calc3model)

back <- lm(gradecalc3 ~ ., data = calcrm)
backlm <- step(back, direction = "backward", trace = 0)
summary(backlm)

both <- lm(gradecalc3 ~ ., data = calcrm)
summary(both)
bothlm <- step(both, direction = "both", trace = 0)
summary(bothlm)
# confirmed that all 3 models are the same

###
# DiffEq
forward <- lm(gradediffeq ~ 1, data = diffrm)
diffeqmodel <- step(forward, direction = "forward", scope = formula(lm(gradediffeq ~ ., data = diffrm)))
summary(diffeqmodel)

back <- lm(gradediffeq ~ ., data = diffrm)
backlm <- step(back, direction = "backward", trace = 0)
summary(backlm)

both <- lm(gradediffeq ~ ., data = diffrm)
summary(both)
bothlm <- step(both, direction = "both", trace = 0)
summary(bothlm)
# confirmed that all 3 models are the same

################################################################################
### MAKING THE STEPWISE MODEL AFTER REMOVING OUTLIERS
# DIFFEQ
# factors to consider for outliers (non-binary variables only):

# screentime
Q <- quantile(diffrm$screentime, probs=c(.25, .75), na.rm = TRUE)
iqr <- IQR(diffrm$screentime, na.rm = TRUE)
diffrm <- subset(diffrm, diffrm$screentime > (Q[1] - 1.5*iqr) & diffrm$screentime < (Q[2]+1.5*iqr)) #save over original
# Original was 0 to 18

# study hours
Q <- quantile(diffrm$studyhours, probs=c(.25, .75), na.rm = TRUE)
iqr <- IQR(diffrm$studyhours, na.rm = TRUE)
diffrm <- subset(diffrm, diffrm$studyhours > (Q[1] - 1.5*iqr) & diffrm$studyhours < (Q[2]+1.5*iqr)) #save

nrow(diffrm) #-11 outliers from screentime (range is 2 to 8 hours)
#-20 from study hours (range is 1 to 4)

#Diffeq model
forward <- lm(gradediffeq ~ 1, data = diffrm)
diffeqrmmodel <- step(forward, direction = "forward", scope = formula(lm(gradediffeq ~ ., data = diffrm)))
summary(diffeqrmmodel)

back <- lm(gradediffeq ~ ., data = diffrm)
backlm <- step(back, direction = "backward", trace = 0)
summary(backlm)

both <- lm(gradediffeq ~ ., data = diffrm)
summary(both)
bothlm <- step(both, direction = "both", trace = 0)
summary(bothlm)
# confirmed all are the same

### CALC3
# Lm without outliers
# factors to consider for outliers:

# screentime
Q <- quantile(calcrm$screentime, probs=c(.25, .75), na.rm = TRUE)
iqr <- IQR(calcrm$screentime, na.rm = TRUE)
calcrm <- subset(calcrm, calcrm$screentime > (Q[1] - 1.5*iqr) & calcrm$screentime < (Q[2]+1.5*iqr)) #save over original

# study hours
Q <- quantile(calcrm$studyhours, probs=c(.25, .75), na.rm = TRUE)
iqr <- IQR(calcrm$studyhours, na.rm = TRUE)
calcrm <- subset(calcrm, calcrm$studyhours > (Q[1] - 1.5*iqr) & calcrm$studyhours < (Q[2]+1.5*iqr)) #save
nrow(calcrm)

#Calc3 model
forward <- lm(gradecalc3 ~ 1, data = calcrm)
calc3model2022 <- step(forward, direction = "forward", scope = formula(lm(gradecalc3 ~ ., data = calcrm)))
summary(calc3model2022)

back <- lm(gradecalc3 ~ ., data = calcrm)
backlm <- step(back, direction = "backward", trace = 0)
summary(backlm)

both <- lm(gradecalc3 ~ ., data = calcrm)
summary(both)
bothlm <- step(both, direction = "both", trace = 0)
summary(bothlm)
# confirmed all are the same

### AEM
# Do models to compare:
#aem model (2018)
aem <- na.omit(aem[-c(6, 9, 13:16)])

forward <- lm(AEM ~ 1, data = aem)
aemmodel2018 <- step(forward, direction = "forward", scope = formula(lm(AEM ~ ., data = aem)))
summary(aemmodel2018)

back <- lm(AEM ~ ., data = aem)
backlm <- step(back, direction = "backward", trace = 0)
summary(backlm)

both <- lm(AEM ~ ., data = aem)
summary(both)
bothlm <- step(both, direction = "both", trace = 0)
summary(bothlm)
# confirmed all same

################################################################################
# Models by year
### Calc3 model (2022)
# Already done above, called calc3model2022

### Diffeq model (2023)
diff2023 <- diffeq |> filter(year == "23") #50 rows
diff2023 <- na.omit(diff2023[-c(1:5, 18:19)])

# Remove outliers
#Study hours
Q <- quantile(diff2023$studyhours, probs=c(.25, .75), na.rm = TRUE)
iqr <- IQR(diff2023$studyhours, na.rm = TRUE)
diff2023 <- subset(diff2023, diff2023$studyhours > (Q[1] - 1.5*iqr) & diff2023$studyhours < (Q[2]+1.5*iqr)) #save over original

#Screentime
Q <- quantile(diff2023$screentime, probs=c(.25, .75), na.rm = TRUE)
iqr <- IQR(diff2023$screentime, na.rm = TRUE)
diff2023 <- subset(diff2023, diff2023$screentime > (Q[1] - 1.5*iqr) & diff2023$screentime < (Q[2]+1.5*iqr)) #save over original

### Linear model (2023)
forward <- lm(gradediffeq ~ 1, data = diff2023)
diffeqmodel23 <- step(forward, direction = "forward", scope = formula(lm(gradediffeq ~ ., data = diff2023)))
summary(diffeqmodel23)

back <- lm(gradediffeq ~ ., data = diff2023)
backlm <- step(back, direction = "backward", trace = 0)
summary(backlm)

both <- lm(gradediffeq ~ ., data = diff2023)
summary(both)
bothlm <- step(both, direction = "both", trace = 0)
summary(bothlm)
# confirmed all the same

### Diffeq model (2024)
diff2024 <- diffeq |> filter(year == "24")
diff2024 <- na.omit(diff2024[-c(1:5, 18:19)])

# Remove outliers
# Study hours:
Q <- quantile(diff2024$studyhours, probs=c(.25, .75), na.rm = TRUE)
iqr <- IQR(diff2024$studyhours, na.rm = TRUE)
diff2024 <- subset(diff2024, diff2024$studyhours > (Q[1] - 1.5*iqr) & diff2024$studyhours < (Q[2]+1.5*iqr)) #save over original

# screentime:
Q <- quantile(diff2024$screentime, probs=c(.25, .75), na.rm = TRUE)
iqr <- IQR(diff2024$screentime, na.rm = TRUE)
diff2024 <- subset(diff2024, diff2024$screentime > (Q[1] - 1.5*iqr) & diff2024$screentime < (Q[2]+1.5*iqr)) #save over original

### Linear model (2024)
forward <- lm(gradediffeq ~ 1, data = diff2024)
diffeqmodel24 <- step(forward, direction = "forward", scope = formula(lm(gradediffeq ~ ., data = diff2024)))
summary(diffeqmodel24)

back <- lm(gradediffeq ~ ., data = diff2024)
backlm <- step(back, direction = "backward", trace = 0)
summary(backlm)

both <- lm(gradediffeq ~ ., data = diff2024)
summary(both)
bothlm <- step(both, direction = "both", trace = 0)
summary(bothlm)
# confirmed all are same

### Do each of the sections give a different result?
# Split DIFFEQ ANALYSIS
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

# Diff1 Model (12PM Spring 2023):
forward <- lm(gradediffeq ~ 1, data = diff1)
diffeq1model <- step(forward, direction = "forward", scope = formula(lm(gradediffeq ~ ., data = diff1)))
summary(diffeq1model)

back <- lm(gradediffeq ~ ., data = diff1)
backlm <- step(back, direction = "backward", trace = 0)
summary(backlm)

both <- lm(gradediffeq ~ ., data = diff1)
summary(both)
bothlm <- step(both, direction = "both", trace = 0)
summary(bothlm)
# confirmed all models are the same

# Diff1 model after removing both sets of outliers:
# study hours
Q <- quantile(diff1$studyhours, probs=c(.25, .75), na.rm = TRUE)
iqr <- IQR(diff1$studyhours, na.rm = TRUE)
diff1 <- subset(diff1, diff1$studyhours > (Q[1] - 1.5*iqr) & diff1$studyhours < (Q[2]+1.5*iqr)) #save over original

# screentime
Q <- quantile(diff1$screentime, probs=c(.25, .75), na.rm = TRUE)
iqr <- IQR(diff1$screentime, na.rm = TRUE)
diff1 <- subset(diff1, diff1$screentime > (Q[1] - 1.5*iqr) & diff1$screentime < (Q[2]+1.5*iqr)) #save over original

forward <- lm(gradediffeq ~ 1, data = diff1)
diffeq1rmmodel <- step(forward, direction = "forward", scope = formula(lm(gradediffeq ~ ., data = diff1)))
summary(diffeq1rmmodel)

back <- lm(gradediffeq ~ ., data = diff1)
backlm <- step(back, direction = "backward", trace = 0)
summary(backlm)

both <- lm(gradediffeq ~ ., data = diff1)
summary(both)
bothlm <- step(both, direction = "both", trace = 0)
summary(bothlm)

# Diff2 Model (9AM Spring 2024):
forward <- lm(gradediffeq ~ 1, data = diff2)
diffeq2model <- step(forward, direction = "forward", scope = formula(lm(gradediffeq ~ ., data = diff2)))
summary(diffeq2model)

back <- lm(gradediffeq ~ ., data = diff2)
backlm <- step(back, direction = "backward", trace = 0)
summary(backlm)

both <- lm(gradediffeq ~ ., data = diff2)
summary(both)
bothlm <- step(both, direction = "both", trace = 0)
summary(bothlm)
# all are the same

# Diff2 model after removing both sets of outliers:
# study hours
Q <- quantile(diff2$studyhours, probs=c(.25, .75), na.rm = TRUE)
iqr <- IQR(diff2$studyhours, na.rm = TRUE)
diff2 <- subset(diff2, diff2$studyhours > (Q[1] - 1.5*iqr) & diff2$studyhours < (Q[2]+1.5*iqr)) #save over original

# screentime
Q <- quantile(diff2$screentime, probs=c(.25, .75), na.rm = TRUE)
iqr <- IQR(diff2$screentime, na.rm = TRUE)
diff2 <- subset(diff2, diff2$screentime > (Q[1] - 1.5*iqr) & diff2$screentime < (Q[2]+1.5*iqr)) #save over original

forward <- lm(gradediffeq ~ 1, data = diff2)
diffeq2rmmodel <- step(forward, direction = "forward", scope = formula(lm(gradediffeq ~ ., data = diff2)))
summary(diffeq2rmmodel)

back <- lm(gradediffeq ~ ., data = diff2)
backlm <- step(back, direction = "backward", trace = 0)
summary(backlm)

both <- lm(gradediffeq ~ ., data = diff2)
summary(both)
bothlm <- step(both, direction = "both", trace = 0)
summary(bothlm)

# Diff3 Model (12PM Spring 2024):
forward <- lm(gradediffeq ~ 1, data = diff3)
diffeq3model <- step(forward, direction = "forward", scope = formula(lm(gradediffeq ~ ., data = diff3)))
summary(diffeq3model)

back <- lm(gradediffeq ~ ., data = diff3)
backlm <- step(back, direction = "backward", trace = 0)
summary(backlm)

both <- lm(gradediffeq ~ ., data = diff3)
summary(both)
bothlm <- step(both, direction = "both", trace = 0)
summary(bothlm)
# all are the same

# Diff3 model after removing both sets of outliers:
# study hours
Q <- quantile(diff3$studyhours, probs=c(.25, .75), na.rm = TRUE)
iqr <- IQR(diff3$studyhours, na.rm = TRUE)
diff3 <- subset(diff3, diff3$studyhours > (Q[1] - 1.5*iqr) & diff3$studyhours < (Q[2]+1.5*iqr)) #save over original

# screentime
Q <- quantile(diff3$screentime, probs=c(.25, .75), na.rm = TRUE)
iqr <- IQR(diff3$screentime, na.rm = TRUE)
diff3 <- subset(diff3, diff3$screentime > (Q[1] - 1.5*iqr) & diff3$screentime < (Q[2]+1.5*iqr)) #save over original

forward <- lm(gradediffeq ~ 1, data = diff3)
diffeq3rmmodel <- step(forward, direction = "forward", scope = formula(lm(gradediffeq ~ ., data = diff3)))
summary(diffeq3rmmodel)

back <- lm(gradediffeq ~ ., data = diff3)
backlm <- step(back, direction = "backward", trace = 0)
summary(backlm)

both <- lm(gradediffeq ~ ., data = diff3)
summary(both)
bothlm <- step(both, direction = "both", trace = 0)
summary(bothlm)

