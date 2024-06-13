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
diff1<- na.omit(diff1[-c(1:5, 18:19)])
diff2<- na.omit(diff2[-c(1:5, 18:19)])
diff3<- na.omit(diff3[-c(1:5, 18:19)])

# Add more information for diffeq
diff1<- diff1 |> mutate(section = 1)
diff2<- diff2 |> mutate(section = 2)
diff3<- diff3 |> mutate(section = 3)
difftotal <- rbind(diff1, diff2, diff3)