#BEGINNING ANALYSIS
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

nrow(calcrm) #need 5 x variables to have enough to do analysis
nrow(diffrm)

# Correlation coeff
library(GGally)
# AEM is the response
ggpairs(aem)

# gradecalc3 is the response
#library(corrplot)
#corrplot(cor(calc3[-c(1:5)]))
ggpairs(calc3[c(17, 5:16, 18:22)])

# gradediffeq is the response
ggpairs(diffeq[c(15, 5:14, 16:20)])

#https://www.spsanderson.com/steveondata/posts/2023-12-06/index.html
###
# Calc3
# Forward model:
forward <- lm(gradecalc3 ~ 1, data = calcrm)
forward <- step(forward, direction = "forward", scope = formula(lm(gradecalc3 ~ ., data = calcrm)))
summary(forward)
#Backwards model:
back <- lm(gradecalc3 ~ ., data = calcrm)
back <- step(back, direction = "backward", trace = 0)
summary(back)

###
# DiffEq
# Forward model:
forward <- lm(gradediffeq ~ 1, data = diffrm)
forwardlm <- step(forward, direction = "forward", scope = formula(lm(gradediffeq ~ ., data = diffrm)))
summary(forwardlm)
#Backwards model:
back <- lm(gradediffeq ~ ., data = na.omit(diffrm))
summary(back)
backlm <- step(back, direction = "backward", trace = 0)
summary(backlm)

# Back+forward step model:
both <- lm(gradecalc3 ~ ., data = calcrm)
summary(both)
calc3model <- step(both, direction = "both", trace = 0)
summary(calc3model)

both <- lm(gradediffeq ~ ., data = diffrm)
summary(both)
diffeqmodel <- step(both, direction = "both", trace = 0)
summary(diffeqmodel)


###
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

#Diff1 Model:
forward <- lm(gradediffeq ~ 1, data = diff1)
forward <- step(forward, direction = "forward", scope = formula(lm(gradediffeq ~ ., data = diff1)))
summary(forward)

back <- lm(gradediffeq ~ ., data = diff1)
backlm <- step(back, direction = "backward", trace = 0)
summary(backlm)

both <- lm(gradediffeq ~ ., data = diff1)
summary(both)
diffeqmodel <- step(both, direction = "both", trace = 0)
summary(diffeqmodel)

#Diff2 Model:
forward <- lm(gradediffeq ~ 1, data = diff2)
forward <- step(forward, direction = "forward", scope = formula(lm(gradediffeq ~ ., data = diff2)))
summary(forward)

back <- lm(gradediffeq ~ ., data = diff2)
backlm <- step(back, direction = "backward", trace = 0)
summary(backlm)

both <- lm(gradediffeq ~ ., data = diff2)
summary(both)
diffeqmodel <- step(both, direction = "both", trace = 0)
summary(diffeqmodel)

#Diff3 Model:
forward <- lm(gradediffeq ~ 1, data = diff3)
forward <- step(forward, direction = "forward", scope = formula(lm(gradediffeq ~ ., data = diff3)))
summary(forward)

back <- lm(gradediffeq ~ ., data = diff3)
backlm <- step(back, direction = "backward", trace = 0)
summary(backlm)

both <- lm(gradediffeq ~ ., data = diff3)
summary(both)
diffeqmodel <- step(both, direction = "both", trace = 0)
summary(diffeqmodel)

# Recheck amount of students per dataset (is it enough) done

#bar plots about cool stuff:
diff1<- diff1 |> mutate(section = 1)
diff2<- diff2 |> mutate(section = 2)
diff3<- diff3 |> mutate(section = 3)
difftotal <- rbind(diff1, diff2, diff3)

ggplot(difftotal, aes(x = factor(section), y = iphone)) + 
  geom_bar(stat = "summary", fun = "mean") 

ggplot(difftotal, aes(x = factor(section), y = studyhours)) + 
  geom_bar(stat = "summary", fun = "mean") 

ggplot(difftotal, aes(x = factor(section), y = screentime)) + 
  geom_bar(stat = "summary", fun = "mean") 

ggplot(difftotal, aes(x = factor(section), y = gradecalc2)) + 
  geom_bar(stat = "summary", fun = "mean") 

ggplot(difftotal, aes(x = factor(section), y = gradediffeq)) + 
  geom_bar(stat = "summary", fun = "mean") 

################################################################################
# DIFFEQ
# Redo lin model without outliers?
# factors to consider for outliers:

# screentime
Q <- quantile(diffrm$screentime, probs=c(.25, .75), na.rm = TRUE)
iqr <- IQR(diffrm$screentime, na.rm = TRUE)
diffrm <- subset(diffrm, diffrm$screentime > (Q[1] - 1.5*iqr) & diffrm$screentime < (Q[2]+1.5*iqr)) #save over original
# Original was 0 to 18

# minor degrees
#nothing to remove for diffeq here
Q <- quantile(diffrm$mndegrees, probs=c(.25, .75), na.rm = TRUE)
iqr <- IQR(diffrm$mndegrees, na.rm = TRUE)
diffrm <- subset(diffrm, diffrm$mndegrees > (Q[1] - 1.5*iqr) & diffrm$mndegrees < (Q[2]+1.5*iqr)) #save over original

# study hours
Q <- quantile(diffrm$studyhours, probs=c(.25, .75), na.rm = TRUE)
iqr <- IQR(diffrm$studyhours, na.rm = TRUE)
diffspecial <- subset(diffrm, diffrm$studyhours > (Q[1] - 1.5*iqr) & diffrm$studyhours < (Q[2]+1.5*iqr)) #save new
#original is 3 to 10 hours

# grade calc 2? doesn't make sense to remove this
nrow(diffrm) #-11 outliers from screentime (range is 2 to 8 hours)
             #-20 from study hours (range is 1 to 4)
             #I don't feel that it's fair to remove people with big study hours so I may save a diff version over

# Do all 3 models to compare:
#Diffeq model no funny study hours
forward <- lm(gradediffeq ~ 1, data = diffrm)
forward <- step(forward, direction = "forward", scope = formula(lm(gradediffeq ~ ., data = diffrm)))
summary(forward)

back <- lm(gradediffeq ~ ., data = diffrm)
backlm <- step(back, direction = "backward", trace = 0)
summary(backlm)

both <- lm(gradediffeq ~ ., data = diffrm)
# summary(both)
diffeqmodel <- step(both, direction = "both", trace = 0)
summary(diffeqmodel)


#Diffeq model removed study hours
forward <- lm(gradediffeq ~ 1, data = diffspecial)
forward <- step(forward, direction = "forward", scope = formula(lm(gradediffeq ~ ., data = diffspecial)))
summary(forward)

back <- lm(gradediffeq ~ ., data = diffspecial)
backlm <- step(back, direction = "backward", trace = 0)
summary(backlm)

both <- lm(gradediffeq ~ ., data = diffspecial)
# summary(both)
diffeqmodel <- step(both, direction = "both", trace = 0)
summary(diffeqmodel)

################################################################################
# CALC3
# Redo lin model without outliers?
# factors to consider for outliers:

# screentime
Q <- quantile(calcrm$screentime, probs=c(.25, .75), na.rm = TRUE)
iqr <- IQR(calcrm$screentime, na.rm = TRUE)
calcrm <- subset(calcrm, calcrm$screentime > (Q[1] - 1.5*iqr) & calcrm$screentime < (Q[2]+1.5*iqr)) #save over original
#63 rows, 2 hours to 10 hours yikes...
#original was 2 to 21

# minor degrees
# None removed here
Q <- quantile(calcrm$mndegrees, probs=c(.25, .75), na.rm = TRUE)
iqr <- IQR(calcrm$mndegrees, na.rm = TRUE)
calcrm <- subset(calcrm, calcrm$mndegrees > (Q[1] - 1.5*iqr) & calcrm$mndegrees < (Q[2]+1.5*iqr)) #save over original


# study hours
Q <- quantile(calcrm$studyhours, probs=c(.25, .75), na.rm = TRUE)
iqr <- IQR(calcrm$studyhours, na.rm = TRUE)
calcrm <- subset(calcrm, calcrm$studyhours > (Q[1] - 1.5*iqr) & calcrm$studyhours < (Q[2]+1.5*iqr)) #save 
nrow(calcrm) #removed 2, range is 0 to 9 hours
# original was 0 to 15

# Do all 3 models to compare:
#Calc3 model
forward <- lm(gradecalc3 ~ 1, data = calcrm)
forward <- step(forward, direction = "forward", scope = formula(lm(gradecalc3 ~ ., data = calcrm)))
summary(forward)

back <- lm(gradecalc3 ~ ., data = calcrm)
backlm <- step(back, direction = "backward", trace = 0)
summary(backlm)

both <- lm(gradecalc3 ~ ., data = calcrm)
# summary(both)
calcmodel <- step(both, direction = "both", trace = 0)
summary(calcmodel)
