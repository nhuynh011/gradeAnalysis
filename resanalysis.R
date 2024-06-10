#BEGINNING ANALYSIS
# Doing correlation graph and some residual analysis

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

# Recheck amount of students per dataset (is it enough)

#Check column significance
sum(diffrm$noprereq)/nrow(diffrm)
# Residual analysis

