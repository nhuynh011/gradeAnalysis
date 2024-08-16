### Cleaning file
# Only cleans 1 file at a time
# Currently set for cleaning MA231 files only, read comments to see which one is needed for diffeq/calc3

# PRE-CLEANING
# Load libraries for cleaning:
library(tidyverse)
library(readxl)
library(here)

# Load file 
df <- read_xlsx(here("raw", "SURVEY FOR MA231-01 in Fall 2022.xlsx"))

#Remove columns A to K (the first question should be favorite season)
df <- df[ -c(1:11) ]

#Remove rows (first row should be row 4)
df <- df[-c(1:2), ]

# Rename columns:
# MA231 (Calc 3)
names(df) <- c("season", "iphone", "windows", "mndegrees", "calc1", "apcalc", "gradecalc2", "gradediffeq", "calc2", "calc3", "diffeq", "noprereq", "gradecalc3", "screentime", "studyhours", "difficulty", "coursepref", "inpersonperf")
# MA232 (Diff Eq)
#names(df) <- c("season", "iphone", "windows", "mndegrees", "calc1", "apcalc", "gradecalc2", "calc2", "calc3", "noprereq", "gradediffeq", "screentime", "studyhours", "difficulty", "coursepref", "inpersonperf")


# Data dictionary:
# mndegrees = number of minor degrees student is pursuing
# calc1 = did the student attend calculus 1 (MA131) at Clarkson University?
# nocourses = did the student not attend calculus 2 and calculus 3 and differential equations at Clarkson?
# coursepref = does the student prefer the course to be in person or online?
# inpersonperf = which format (online or in person) does the student feel they perform better in?

# CLEANING
#Changing responses in columns that need 0 and 1:
#If we need to replace NA:
# df[is.na(df)] <- 0

#iPhone (keep NA as NA):
df$iphone<-replace(df$iphone, df$iphone == "Android based phone", 0)
df$iphone<-replace(df$iphone, df$iphone == "iPhone", 1)

#Windows (keep NA as NA):
df$windows<-replace(df$windows, df$windows == "Mac OS", 0)
df$windows<-replace(df$windows, df$windows == "Windows", 1)

#Calc1 (keep NA as NA):
df$calc1<-replace(df$calc1, df$calc1 == "No", 0)
df$calc1<-replace(df$calc1, df$calc1 == "Yes", 1)

#APCalc (keep NA as NA):
df$apcalc<-replace(df$apcalc, df$apcalc == "No", 0)
df$apcalc<-replace(df$apcalc, df$apcalc == "Yes", 1)

#Calc2 (NA = 0):
df$calc2[is.na(df$calc2)] <- 0
df$calc2<-replace(df$calc2, df$calc2 != 0, 1)

#Calc3 (NA = 0):
df$calc3[is.na(df$calc3)] <- 0
df$calc3<-replace(df$calc3, df$calc3 != 0, 1)

#DiffEq (NA = 0): (Only for Calulus 3)
df$diffeq[is.na(df$diffeq)] <- 0
df$diffeq<-replace(df$diffeq, df$diffeq != 0, 1)

#NoPrereq(uisites) (NA = 0):
df$noprereq[is.na(df$noprereq)] <- 0
df$noprereq<-replace(df$noprereq, df$noprereq != 0, 1)

#CoursePref (in person = 1, else 0, keep NA as NA):
df$coursepref<-replace(df$coursepref, df$coursepref == "In-person", 1)
df$coursepref<-replace(df$coursepref, df$coursepref == "Online", 0)

#InPersonPerf (in person = 1, online = 0, keep NA as NA):
df$inpersonperf<-replace(df$inpersonperf, df$inpersonperf == "Online format", 0)
df$inpersonperf<-replace(df$inpersonperf, df$inpersonperf == "In-person format", 1)

# Converting responses from a categorical to numeric:
#GradeCalc2 (0 to 12):
df$gradecalc2<-replace(df$gradecalc2, df$gradecalc2 == "A+", 13)
df$gradecalc2<-replace(df$gradecalc2, df$gradecalc2 == "A", 12)
df$gradecalc2<-replace(df$gradecalc2, df$gradecalc2 == "A-", 11)
df$gradecalc2<-replace(df$gradecalc2, df$gradecalc2 == "B+", 10)
df$gradecalc2<-replace(df$gradecalc2, df$gradecalc2 == "B", 9)
df$gradecalc2<-replace(df$gradecalc2, df$gradecalc2 == "B-", 8)
df$gradecalc2<-replace(df$gradecalc2, df$gradecalc2 == "C+", 7)
df$gradecalc2<-replace(df$gradecalc2, df$gradecalc2 == "C", 6)
df$gradecalc2<-replace(df$gradecalc2, df$gradecalc2 == "C-", 5)
df$gradecalc2<-replace(df$gradecalc2, df$gradecalc2 == "D", 3)
df$gradecalc2<-replace(df$gradecalc2, df$gradecalc2 == "F", 0)

#GradeDiffEq (0 to 12), only use for MA231 (Calculus 3):
df$gradediffeq<-replace(df$gradediffeq, df$gradediffeq == "A+", 13)
df$gradediffeq<-replace(df$gradediffeq, df$gradediffeq == "A", 12)
df$gradediffeq<-replace(df$gradediffeq, df$gradediffeq == "A-", 11)
df$gradediffeq<-replace(df$gradediffeq, df$gradediffeq == "B+", 10)
df$gradediffeq<-replace(df$gradediffeq, df$gradediffeq == "B", 9)
df$gradediffeq<-replace(df$gradediffeq, df$gradediffeq == "B-", 8)
df$gradediffeq<-replace(df$gradediffeq, df$gradediffeq == "C+", 7)
df$gradediffeq<-replace(df$gradediffeq, df$gradediffeq == "C", 6)
df$gradediffeq<-replace(df$gradediffeq, df$gradediffeq == "C-", 5)
df$gradediffeq<-replace(df$gradediffeq, df$gradediffeq == "D", 3)
df$gradediffeq<-replace(df$gradediffeq, df$gradediffeq == "F", 0)

#Difficulty (More = 1, Less = -1, else = 0, keep NA as NA):
df$difficulty<-replace(df$difficulty, df$difficulty == "more than other courses", 1)
df$difficulty<-replace(df$difficulty, df$difficulty == "about the same as other courses", 0)
df$difficulty<-replace(df$difficulty, df$difficulty == "less than other courses", -1)

# Format as numeric to help excel export:
for (x in 2:18) {
  df[x] <- as.numeric(unlist(df[x]))
}

# Format some things by hand after (study hours, most students answered with a sentence instead of a number)

# Write out as an xlsx file
# Change the output name depending on the file. 
library(openxlsx)
write.xlsx(df, "MA231 TEN AM FALL 2022.xlsx")



