library(readxl)
library(dplyr)
library(psych)
library(janitor)
library(SemNeT)
library(reshape2)
library(data.table)
library(readr)



CRA <- read_csv("CRA.csv")

#Replace RT's of 0 with 30000
CRA$cratSlide2.RT <- ifelse(CRA$cratSlide2.RT == "0", "30000", CRA$cratSlide2.RT)

#drop categorical responses, empty rows & absent responses for thinking style
CRA1 <- dplyr::select(CRA, -1,-3:-6, -8)

###
#remove entire rows with absent thinking style
CRA1 <- CRA1[!(CRA1$inSlide.RESP ==0),]
CRA1 <- CRA1[!is.na(CRA1$Subject),]

#Group together thinking styles -- 1 = 1 & 2 -- 2 = 3 & 4
CRA1 <- CRA1 %>% 
  mutate(Style = case_when(inSlide.RESP==1 | inSlide.RESP==2 ~ 1, 
                           inSlide.RESP==3 | inSlide.RESP==4 ~ 2))

##Accuracy Score
PartStyles <- CRA1 %>% 
  group_by(Subject) %>% 
  summarise(Accuracy = sum(cratSlide2.ACC),
            An = sum(Style==1),
            In = sum(Style==2))

#Remove participants who score below 2 SDs on either task
FLU <- read.csv(file = "animal_cleaned_responses.csv")
FLU <- FLU[!is.na(FLU$Response_12),]
PartStyles <- PartStyles[!(PartStyles$Accuracy <=1),]

#######
#Check which subjects appear in both the RAT and the Fluency task
#Identify participants that are in only one of the two datasets
#Create participant vectors
FLUsubjects <- c(unique(FLU$X))
PartStyles1 <- c(unique(PartStyles$Subject))
#Return non-matches
#Missing from RAT (present in Fluency)
MissingCRA <- FLUsubjects[!FLUsubjects %in% PartStyles1]
#Missing from Fluency data (present in RAT)
MissingFLU <- PartStyles1[!PartStyles1 %in% FLUsubjects]

#Remove participants in each dataset that are missing in the other dataset
CRAclean <- data.frame(PartStyles[ ! PartStyles$Subject %in% MissingFLU, ])
FLUclean <- data.frame(FLU[ ! FLU$X %in% MissingCRA, ])

####Separate groups by median split
###
#Create The Insight Analysis Calculation
CRAclean <- CRAclean %>% mutate(InsightGroup = case_when(CRAclean$In > median(CRAclean$In) ~ 2, CRAclean$In < median(CRAclean$In) ~ 1))
CRAclean <- CRAclean %>% mutate(AnalyticalGroup = case_when(CRAclean$An > median(CRAclean$An) ~ 2, An < median(CRAclean$An) ~ 1))

CRAclean$AnalyticalGroup <- ifelse(CRAclean$AnalyticalGroup == "1", "LowAn", ifelse(CRAclean$AnalyticalGroup == "2", "HiAn", "NA"))
CRAclean$InsightGroup <- ifelse(CRAclean$InsightGroup == "1", "LowIn", ifelse(CRAclean$InsightGroup == "2", "HiIn", "NA"))

#REMOVE MEDIAN
CRAclean <- CRAclean[!is.na(CRAclean$AnalyticalGroup),]
CRAclean <- CRAclean[!is.na(CRAclean$InsightGroup),]

#Create single group variablble
CRAclean$group <- gsub(" ", "", paste(CRAclean$InsightGroup,".", CRAclean$AnalyticalGroup))

CRAclean <- CRAclean %>% filter(group == "HiIn.LowAn" | group == "LowIn.HiAn")

#######
#Check which subjects appear in both the RAT and the Fluency task
#Identify participants that are in only one of the two datasets
#Create participant vectors
FLUsubjects <- c(unique(FLUclean$X))
CRAsubjects <- c(unique(CRAclean$Subject))
#Return non-matches
#Missing from RAT (present in Fluency)
MissingCRA <- FLUsubjects[!FLUsubjects %in% CRAsubjects]
#Missing from Fluency data (present in RAT)
MissingFLU <- CRAsubjects[!CRAsubjects %in% FLUsubjects]
#Remove participants in each dataset that are missing in the other dataset
CRAclean <- data.frame(CRAclean[ ! CRAclean$Subject %in% MissingFLU, ])
FLUclean <- data.frame(FLUclean[ ! FLUclean$X %in% MissingCRA, ])

#order df's
CRAclean <- CRAclean[order(CRAclean$Subject),]
FLUclean <- FLUclean[order(FLUclean$X),]

#finalize df's
CRAclean2 <- dplyr::select(CRAclean, 7)
FLUclean2 <- dplyr::select(FLUclean, -1)
groups1 <- CRAclean2
fluency1 <- FLUclean2

#Write csv's
write.csv(groups1, file = "groupsIn.csv", row.names=FALSE)
write.csv(fluency1, file = "responsesIn.csv", row.names=FALSE)



############RUN FROM SCRATCH TO GET THE ACCURACY GROUPING############
library(readxl)
library(dplyr)
library(psych)
library(janitor)
library(car)
library(ggplot2)
library(SemNeT)
library(ggpubr)
library(ggsignif)
library(ggtext)

CRA <- read_excel("CRA.xlsx")

#Replace RT's of 0 with 30000
CRA$cratSlide2.RT <- ifelse(CRA$cratSlide2.RT == "0", "30000", CRA$cratSlide2.RT)

#drop categorical responses, empty rows & absent responses for thinking style
CRA1 <- dplyr::select(CRA, -1,-3:-6, -8)

###
#remove entire rows with absent thinking style
CRA1 <- CRA1[!(CRA1$inSlide.RESP ==0),]
CRA1 <- CRA1[!is.na(CRA1$Subject),]

#Group together thinking styles -- 1 = 1 & 2 -- 2 = 3 & 4
CRA1 <- CRA1 %>% 
  mutate(Style = case_when(inSlide.RESP==1 | inSlide.RESP==2 ~ 1, 
                           inSlide.RESP==3 | inSlide.RESP==4 ~ 2))

##Accuracy Score
PartStyles <- CRA1 %>% 
  group_by(Subject) %>% 
  summarise(Accuracy = sum(cratSlide2.ACC),
            An = sum(Style==1),
            In = sum(Style==2))

#Remove participants who score below 2 SDs on either task
FLU <- read.csv(file = "animal_cleaned_responses.csv")
FLU <- FLU[!is.na(FLU$Response_12),]
PartStyles <- PartStyles[!(PartStyles$Accuracy <=1),]

#######
#Check which subjects appear in both the RAT and the Fluency task
#Identify participants that are in only one of the two datasets
#Create participant vectors
FLUsubjects <- c(unique(FLU$X))
PartStyles1 <- c(unique(PartStyles$Subject))
#Return non-matches
#Missing from RAT (present in Fluency)
MissingCRA <- FLUsubjects[!FLUsubjects %in% PartStyles1]
#Missing from Fluency data (present in RAT)
MissingFLU <- PartStyles1[!PartStyles1 %in% FLUsubjects]

#Remove participants in each database that are missing in the other database
CRAcleanAcc <- data.frame(PartStyles[ ! PartStyles$Subject %in% MissingFLU, ])
FLUclean <- data.frame(FLU[ ! FLU$X %in% MissingCRA, ])

#Create groups for participants that use more of style 1 or 2 (Major Style) 
CRAcleanAcc <- CRAcleanAcc %>%
  mutate(Group = case_when(Accuracy > median(Accuracy) ~ 2, 
                           Accuracy < median(Accuracy) ~ 1))

#Rename 1's and 2's
CRAcleanAcc$Group <- ifelse(CRAcleanAcc$Group == "1", "LowAcc", ifelse(CRAcleanAcc$Group == "2", "HighAcc", "NA"))
#REMOVE MEDIAN
CRAcleanAcc <- CRAcleanAcc[!is.na(CRAcleanAcc$Group),]

#######
#Check which subjects appear in both the RAT and the Fluency task
#Identify participants that are in only one of the two datasets
#Create participant vectors
FLUsubjects <- c(unique(FLUclean$X))
CRAsubjects <- c(unique(CRAcleanAcc$Subject))
#Return non-matches
#Missing from RAT (present in Fluency)
MissingCRA <- FLUsubjects[!FLUsubjects %in% CRAsubjects]
#Missing from Fluency data (present in RAT)
MissingFLU <- CRAsubjects[!CRAsubjects %in% FLUsubjects]

#Remove participants in each database that are missing in the other database
CRAcleanAcc <- data.frame(CRAcleanAcc[ ! CRAcleanAcc$Subject %in% MissingFLU, ])
FLUclean <- data.frame(FLUclean[ ! FLUclean$X %in% MissingCRA, ])

#Prepare df's for SemNeT
CRAcleanAcc <- CRAcleanAcc[order(CRAcleanAcc$Subject),]
FLUclean <- FLUclean[order(FLUclean$X),]

groups <- dplyr::select(CRAcleanAcc, 5)
responses <- dplyr::select(FLUclean, -1)

write.csv(groups, file = "groupsAcc.csv", row.names=FALSE)
write.csv(responses, file = "responsesAcc.csv", row.names=FALSE)

