library(knitr)
library(tidyverse)
library(magrittr)
library(kableExtra)

load("strawberries.Rdata")
head(stb)
aa <- summarize_all(stb, n_distinct)
unique(stb$Production)


stb_d <- filter(stb,Values != "(NA)" & Values != "(D)" & Values != "(Z)")
stb_d$Values <- gsub(",", "",stb_d$Values)
stb_d$Values <- as.numeric(stb_d$Values)

str(stb_d)
stb_d$Year <- as.ordered(stb_d$Year)
stb_d$State <- as.factor(stb_d$State)
stb_d$Production <- as.factor(stb_d$Production)
stb_d$Measures <- as.factor(stb_d$Measures)
stb_d$Avg <- as.factor(stb_d$Avg)
stb_d$Materials <- as.factor(stb_d$Materials)
stb_d$Chemical <- as.factor(stb_d$Chemical)


###############################################################################
ay1 <- filter(stb_d, Measures == " MEASURED IN LB / ACRE / APPLICATION")

p1 <- ggplot(ay1, aes(x = Chemical, y = Values)) +
      geom_boxplot() +
      theme(axis.text.x = element_text(angle = 60, hjust = 1),
      axis.text = element_text(size = 11),
      axis.title = element_text(size = 13, face = "bold"))
p1

ay2 <- filter(ay1, Chemical != "OTHER")

p2 <- ggplot(ay2, aes(x = Chemical, y = Values)) +
      geom_boxplot() +
      theme(axis.text.x = element_text(angle = 60, hjust = 1),
      axis.text = element_text(size = 11),
      axis.title = element_text(size = 13, face = "bold"))
p2

###############################################################################

ay3 <- filter(stb_d, Production == " ACRES PLANTED")

p3 <- ggplot(ay3, aes(x = State, y = Values)) +
      geom_boxplot() +
      theme(axis.text.x = element_text(angle = 60, hjust = 1),
      axis.text = element_text(size = 11),
      axis.title = element_text(size = 13, face = "bold"))
p3

ay4 <- filter(ay3, State != "CALIFORNIA")

p4 <- ggplot(ay4, aes(x = State, y = Values)) +
      geom_boxplot() +
      theme(axis.text.x = element_text(angle = 60, hjust = 1),
      axis.text = element_text(size = 11),
      axis.title = element_text(size = 13, face = "bold"))
p4

###############################################################################


