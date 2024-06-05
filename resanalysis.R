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
ggpairs(calc3[-c(1:5)])

# gradediffeq is the response
ggpairs(diffeq[-c(1:5)])

# Residual analysis
