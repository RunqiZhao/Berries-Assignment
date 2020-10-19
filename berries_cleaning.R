# USDA database selector https://quickstats.nass.usda.gov
# 
# berries
# https://quickstats.nass.usda.gov/results/D416E96E-3D5C-324C-9334-1D38DF88FFF1
# 
# corn farming economics
# https://quickstats.nass.usda.gov/results/471133FF-98EA-306C-8C8F-4A607B48E4BB
# 

library(knitr)
library(tidyverse)
library(magrittr)
library(kableExtra)
# read data
berries <- read_csv("berries.csv", col_names = TRUE)

# unique values 
aa <- summarize_all(berries, n_distinct)

bb <- which(aa[1,]==1)

colnames(berries)[bb]

single_values <- berries[1,bb]
single_values

berries <- select(berries, -all_of(bb))

aa <- select(aa, -all_of(bb)) 

# delate state ANSI code

berries <- select(berries, -4)
aa <- select(aa, -4) 
aa

unique(berries$Year)
unique(berries$Period)


# Focus on: period = "Year" and Commodity = "STRAWBERRIES"
# strawberry data
stb <- filter(berries, (Commodity=="STRAWBERRIES") & (Period=="YEAR"))

stb <- select(stb, -c(Period, Commodity))

sum(str_detect(stb$`Data Item`, "^STRAWBERRIES")) == length(stb$`Data Item`)
unique(stb$`Data Item`)

# separate Data Item
# two subset
s1 <- str_detect(stb$`Data Item`, "^STRAWBERRIES, ")
s1 <- which(s1 == TRUE)
stb1 <- stb[s1,]
sum(str_detect(stb1$`Data Item`, "^STRAWBERRIES, ")) == length(stb1$`Data Item`)

s2 <- str_detect(stb$`Data Item`, "^STRAWBERRIES - ")
s2 <- which(s2 == TRUE)
stb2 <- stb[s2,]
sum(str_detect(stb2$`Data Item`, "^STRAWBERRIES - ")) == length(stb2$`Data Item`)


unique(stb1$`Data Item`)

stb1 <- separate(stb1, `Data Item`, c("Berry","type", "measure", "other"), sep = ",") 
unique(stb1$Berry)
unique(stb1$type)
unique(stb1$measure)

s11 <- str_detect(stb1$measure, "^ MEASURED")

s12 <- which(s11 == FALSE)
stb12 <- stb1[s12,]
stb12$type <- paste(stb12$type, stb12$measure, sep= ",")
stb12$measure <- stb12$other
sum(str_detect(stb12$measure, "^ MEASURED")) == length(stb12$measure)

stb12$other <- NA

s11 <- which(s11 == TRUE)
stb11 <- stb1[s11,]

stb1 <- rbind(stb11,stb12)

unique(stb1$Berry)
unique(stb1$type)
unique(stb1$measure)
unique(stb1$other)


stb2 <- separate(stb2, `Data Item`, c("Berry","type"), sep = "-") 
unique(stb2$Berry)
unique(stb2$type)
stb2 <- separate(stb2, `type`, c("type", "measure", "other"), sep = ",") 
unique(stb2$measure)
unique(stb2$other)

stb <- rbind(stb1,stb2)
unique(stb$Berry)
unique(stb$type)
unique(stb$measure)
unique(stb$other)

stb <- select(stb, -Berry)
head(stb)
stb[is.na(stb)] <- " "
# kable(head(stb, n=10)) %>% kable_styling(font_size=12)

# Separate Domain
unique(stb$Domain)
stb  <-  separate(stb,Domain, c("D_left", "D_right"), sep = ", ")
unique(stb$D_left)
unique(stb$D_right)

# Separate Domain Category
unique(stb$`Domain Category`)

s1 <- str_detect(stb$`Domain Category`,"^CHEMICAL, INSECTICIDE: \\(CYFLUMETOFEN")

s2 <- which(s1 == FALSE)
stb2 <- stb[s2,]

s1 <- which(s1 == TRUE)
stb1 <- stb[s1,]
unique(stb1$`Domain Category`)
stb1$`Domain Category` <- "CHEMICAL, INSECTICIDE: (CYFLUMETOFEN = 138831)"
unique(stb1$`Domain Category`)

stb <- rbind(stb1,stb2)

stb <-  separate(stb,`Domain Category`, c("DC_left", "DC_right"), sep = ", ")
unique(stb$DC_left)
unique(stb$DC_right)

stb <- separate(stb, DC_left, c("DC_left_l", "DC_left_r"), sep = ": ")
stb <- separate(stb, DC_right, c("DC_right_l", "DC_right_r"), sep = ": ")
stb[is.na(stb)] <- " "

###################################################################################
unique(paste(stb$D_left,stb$DC_left_l))
stb <- select(stb, -DC_left_l)

unique(paste(stb$D_right,stb$DC_right_l))
stb <- select(stb, -DC_right_l)

unique(paste(stb$D_left, stb$D_right))

stb <- mutate(stb, D_left = "CHEMICAL", D_left = "") 
stb[is.na(stb)] <- " "
stb <- mutate(stb, Chemical=paste(D_left, D_right)) 
stb <- select(stb, -c(D_left, D_right)) 
stb[is.na(stb)] <- " "
stb <- mutate(stb, Chemical = str_trim(paste(DC_left_r, Chemical)))

stb <- select(stb, Year, State, type, measure, other, DC_right_r, Chemical, Value )
unique(stb$type) # rename Production
unique(stb$measure) # rename Measures
unique(stb$other) # rename Avg
unique(stb$DC_right_r) # rename materials
unique(stb$Chemical)
unique(stb$Value) # Values

stb <- rename(stb, Production = type, Measures = measure, Avg = other, Materials = DC_right_r, Values = Value)
stb <- select(stb, Year, State, Production, Measures, Avg, Materials, Chemical, Values)
stb[is.na(stb)] <- " "

save(stb, file = "strawberries.Rdata")
