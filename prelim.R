#PRELIM OBSERVATIONS
# What questions can we ask with this data?

# Load lib
library(tidyverse)
library(here)
library(chron)
library(readxl)
library(dplyr)

# Load files (5)
c1<- read_xlsx(here("MA23(...) files", "MA231 TEN AM FALL 2022.xlsx"))
c2<- read_xlsx(here("MA23(...) files", "MA231 ONE PM FALL 2022.xlsx"))
d1<- read_xlsx(here("MA23(...) files", "MA232 NINE AM SPRING 2024.xlsx"))
d2<- read_xlsx(here("MA23(...) files", "MA232 TWELVE PM SPRING 2024.xlsx"))
s<- read_xlsx(here("MA23(...) files", "MA232 TWELVE PM SPRING 2023.xlsx"))
stat1 <- read_xlsx(here("MA23(...) files", "MA383 TWO PM FALL 2024.xlsx"))
stat2 <- read_xlsx(here("MA23(...) files", "MA383 THREE PM FALL 2024.xlsx"))

# Add more information to the files and combine them
# Add column describing the year, course name, and semester
c1 <- c1|> mutate(year = 22,
                  semester = 0, #fall = 0, spring = 1
                  time = times(paste0(10, ":00", ":00")),
                  coursecode = 231)
c2 <- c2|> mutate(year = 22,
                  semester = 0, #fall = 0, spring = 1
                  time = times(paste0(13, ":00", ":00")),
                  coursecode = 231) 
d1 <- d1|> mutate(year = 24,
                  semester = 1, #fall = 0, spring = 1
                  time = times(paste0(9, ":00", ":00")),
                  coursecode = 232,) 
d2 <- d2|> mutate(year = 24,
                  semester = 1, #fall = 0, spring = 1
                  time = times(paste0(12, ":00", ":00")),
                  coursecode = 232) 
s <- s|> mutate(year = 23,
                semester = 1, #fall = 0, spring =1
                time = times(paste0(12, ":00", ":00")),
                coursecode = 232) 
stat1 <- stat1|> mutate(year = 24,
                    semester = 0, #fall = 0, spring =1
                    time = times(paste0(14, ":00", ":00")),
                    coursecode = 383) 
stat2 <- stat2|> mutate(year = 24,
                        semester = 0, #fall = 0, spring =1
                        time = times(paste0(15, ":00", ":00")),
                        coursecode = 383) 

# Move newly added columns to the front
c1 <- c1|>relocate(c("year", "semester", "time", "coursecode"))
c2 <- c2|>relocate(c("year", "semester", "time", "coursecode"))
d1 <- d1|>relocate(c("year", "semester", "time", "coursecode"))
d2 <- d2|>relocate(c("year", "semester", "time", "coursecode"))
s <- s|>relocate(c("year", "semester", "time", "coursecode"))
stat1 <- stat1|>relocate(c("year", "semester", "time", "coursecode"))
stat2 <- stat2|>relocate(c("year", "semester", "time", "coursecode"))

# Combine sets
calc3total <- rbind(c1, c2)
diffeqtotal <- rbind(d1, d2, s)
diffeqdf <- rbind(d1, d2)
stattotal <- rbind(stat1, stat2)

# Moved columns around so dataset matches
calc3total <- calc3total|>relocate("gradecalc2", .after = "noprereq")
diffeqtotal <- diffeqtotal|>relocate("gradecalc2", .after = "noprereq")
stattotal <- stattotal|>relocate("gradecalc2", .after = "noprereq")

# Bind all recent survey data
cols3 <- colnames(dplyr::select(calc3total, -c("gradediffeq", "diffeq", "noprereq", "gradecalc3")))
postCOVID <- rbind(
  subset(calc3total, select = cols3), 
  subset(diffeqtotal, select = cols3)
)
postCOVID <- rbind(
  postCOVID,
  subset(stattotal, select = cols3)
)

# We want to do meta analysis of past data
# Load data
preCOVID <- read_xlsx(here("data", "datafor_r.xlsx"))

# Clean
# time here is if the class is MWF or TuTh, where MWF is 1.
names(preCOVID) <- c("aem", "gradecalc1", "gradecalc2", "gradecalc3", "gradediffeq", "gpapr", "gradyr", "iphone", "time", "mjdegrees", "mndegrees", "NUniPre", "PhAlwys", "PhAwy", "PhDistr", "PhOnce", "screentime")
preCOVID <- preCOVID|> mutate(year = 18,
                  semester = 0, #fall = 0, spring = 1
                  coursecode = 330)
preCOVID <- preCOVID|>relocate(c("year", "semester", "time", "coursecode"))
preCOVID <- preCOVID|>relocate("mndegrees", .after = "iphone")

# rbind all together by common columns
common <- colnames(dplyr::select(postCOVID, c("year", "semester", "time", "coursecode", "iphone", "mndegrees", "gradecalc2", "screentime")))
all <- rbind(
  subset(preCOVID, select = common), 
  subset(postCOVID, select = common)
)

### GRAPHS
# Now do some general graphs throughout covid
ggplot(all, aes(x = factor(year), y = iphone)) + 
  geom_bar(stat = "summary", fun = "mean") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))

ggplot(all, aes(x = factor(year), y = gradecalc2)) + 
  geom_bar(stat = "summary", fun = "mean") + theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))

ggplot(all, aes(x = factor(year), y = screentime)) + 
  geom_bar(stat = "summary", fun = "mean") + theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))

ggplot(all, aes(x=factor(year), y = mndegrees))+
  geom_bar(stat = "summary", fun = "mean") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))

# Some for only post COVID
ggplot(postCOVID, aes(x=factor(year), y = studyhours))+
  geom_bar(stat = "summary", fun = "mean") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))

ggplot(postCOVID, aes(x=factor(year), y = mndegrees))+
  geom_bar(stat = "summary", fun = "mean") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))

ggplot(postCOVID, aes(x=factor(year), y = gradecalc2))+
  geom_bar(stat = "summary", fun = "mean") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))

# Export needed files
library(openxlsx)
write.xlsx(calc3total, here("data", "calc3.xlsx"))
write.xlsx(diffeqtotal, here("data", "diffeq.xlsx"))
write.xlsx(stattotal, here("data", "stat.xlsx"))
write.xlsx(all, here("data", "all.xlsx"))

