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

# Looking at students inside (5 times column numbers for amount of students needed)
#17 x 5 for aem = 85 not enough
#18 x 5 for calc3 = 90 good
#16 x 5 for diffeq = 80 good

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

# Back+forward step model:
back <- lm(gradecalc3 ~ ., data = na.omit(calc3[-c(1:4)]))
summary(back)
calc3model <- step(back, direction = "both", trace = 0)
summary(calc3model)

back <- lm(gradediffeq ~ ., data = na.omit(diffeq[-c(1:4)]))
summary(back)
diffeqmodel <- step(back, direction = "both", trace = 0)
summary(diffeqmodel)

# Recheck amount of students per dataset (is it enough)
#18 x 5 for calc3 = 90
nrow(na.omit(calc3))

#16 x 5 for diffeq = 80 good
nrow(na.omit(diffeq))

# Residual analysis

