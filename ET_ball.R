#### GETTING SET UP: READ ME. --------------------------------

# What do you want the export file to be called?

export_filename <- "Int-Mech_Control1.csv"

## A few notes about the script. ----------------

# This script was written for R Version 3.3.2 (Sincere Pumpkin Patch).
# Please update R and reinstall packages as some parts of the script may not work properly in previous versions of R.

# This script is meant to handle eyetracking datasets that have already been cleaned by log_filter.php. 

## Installing and loading packages. ----------------

# To install a package, use: install.packages("plyr").
# Packages must be reloaded each time R is opened but only need to be installed once. To load:

library(plyr)
library(dtplyr)
library(reshape)
library(reshape2)
library(data.table)
library(dplyr)

# If the following error comes up while trying to load the library:
# Error in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]): there is no package called 'XXXXX'
# Error: package or namespace load failed for 'plyr'
# That means that you do not have XXXXX package installed and you need to install it for other packages to work

#### IMPORTING THE DATA: READ ME --------------------------------

# Input correct file directory (and use / (not \) to denote different folder levels)

data <- read.csv("C:/Users/kelse/Desktop/Combined_data/Control_1.csv", header=T)

####  SETTING UP PARAMETERS: READ ME --------------------------------

fix_length_row           <- 5 #minimum number of rows to be considered a fixation
subtrial_length_row      <- 150 #number of rows for ball to go from one side of screen to the other side
subtrial_length_time     <- 2500 #time for ball to go from one side of screen to the other side
ball_behind_time_lr      <- 866.66 #when ball moves L->R, time for ball to go from edge to behind box
ball_behind_time_rl      <- 550 #when ball moves R->L, time for ball to go from edge to behind box
ball_behind_row_lr       <- 52 #when ball moves L->R, rows for ball to go from edge to behind box
ball_behind_row_rl       <- 33 #when ball moves R->L, rows for ball to go from edge to behind box
missing_threshold        <- .5 #percent missing threshold for exclusion, e.g., .5 = 50% missing or less included
ball_reappear_time_lr    <- 1967 #when ball moves L->R, time when ball reappears in ms
ball_reappear_time_rl    <- 1650 #when ball moves R->L, time when ball reappears in ms
ball_reappear_row_lr     <- 118 #when ball moves L->R, row when ball reappears 
ball_reappear_row_rl     <- 98 #when ball moves R->L, row when ball reappears 
al_buffer                <- 150 #extra ms after ball reappears when look still is anticipatory
al_not_before            <- 150 #period of time after subtrial begins that anticipatory look doesn't count
missing_post_cutoff_time <- 400 #time in ms after cutoff at the end of each subtrial that won't be included in missing data calculations
missing_post_cutoff_row  <- 24 #rows after cutoff at the end of each subtrial that won't be included in missing data calculations
missing_data_label       <- -9999 #value for when too much missing data during trial
no_fixation_label        <- -8888 #value for when there is no fixation to the ball pre ball going behind box but not too much missing data
no_look_label            <- -7777 #value for when no look to post-box ball region

#### MAKING SUBTRIALS WITHIN SINGLE TRIAL --------------------------------

# Dividing each trial into 12 subtrials (ball passes behind box 12 times in single trial).

data[, "Subtrial"] <- ceiling(data$time/subtrial_length_time)
data$Subtrial[data$Subtrial %in% 0] <- 1

# Giving each subtrial a unique ID.

i <- 0
data$ID <- 1

while (i <= nrow(data)){
  ifelse(as.numeric(data$Subtrial[i])-as.numeric(data$Subtrial[i-1]) != 0, 
         data$ID[i] <- as.numeric(data$ID[i-1]) + 1,
         data$ID[i] <- as.numeric(data$ID[i-1]))
  i <- i+1
}

#### DETERMINING WHICH TRIALS ARE USEABLE --------------------------------

## Determining % of missing data per subtrial before ball goes behind box. ----------------

# Filtering out looks to the ball after ball goes behind box. 

data_prebox <- data
data_prebox <- data_prebox[!(data_prebox$Subtrial %% 2 != 0 & 
                               data_prebox$time >= (ball_behind_time_lr + 
                               subtrial_length_time*(data_prebox$Subtrial-1))), ]
data_prebox <- data_prebox[!(data_prebox$Subtrial %% 2 == 0 & 
                               data_prebox$time >= (ball_behind_time_rl + 
                               subtrial_length_time*(data_prebox$Subtrial-1))), ]


# Determining % misssing data pre-ball behind box on each subtrial.

PercMissingSub_prebox <- ddply(subset(data_prebox, data_prebox$fixationX != -10000 & 
                                        data_prebox$Subtrial %% 2 == 0), 
                                        .(ID, Participant, Trial, Subtrial), nrow)
PercMissingSub_prebox$PercMissing_Pre <- 1 - PercMissingSub_prebox$V1/ball_behind_row_rl
PercMissingSub_prebox1 <- ddply(subset(data_prebox, data_prebox$fixationX != -10000 & 
                                         data_prebox$Subtrial %% 2 != 0), 
                                         .(ID, Participant, Trial, Subtrial), nrow)
PercMissingSub_prebox1$PercMissing_Pre <- 1 - PercMissingSub_prebox1$V1/ball_behind_row_lr
PercMissingSub_prebox <- merge(PercMissingSub_prebox, PercMissingSub_prebox1, all = TRUE)
remove(PercMissingSub_prebox1)

## Determining % of missing data per subtrial after ball goes behind box. ----------------

# Filtering out looks to the ball after ball goes behind box and reemerges.

data_postbox <- data
data_postbox <- data_postbox[!(data_postbox$Subtrial %% 2 != 0 & 
                                 (data_postbox$time <= (ball_behind_time_lr + subtrial_length_time*(data_postbox$Subtrial-1)) |
                                  data_postbox$time >= (ball_reappear_time_lr + missing_post_cutoff_time + subtrial_length_time*(data_postbox$Subtrial-1)))),]
data_postbox <- data_postbox[!(data_postbox$Subtrial %% 2 == 0 &
                                 (data_postbox$time <= (ball_behind_time_rl + subtrial_length_time*(data_postbox$Subtrial-1)) |
                                 data_postbox$time >= (ball_reappear_time_rl + missing_post_cutoff_time + subtrial_length_time*(data_postbox$Subtrial-1)))),]

# Determining % misssing data post-ball behind box on subtrial.

PercMissingSub_postbox <- ddply(subset(data_postbox, data_postbox$fixationX != -10000 & 
                                         data_postbox$Subtrial %% 2 == 0),
                                         .(ID, Participant, Trial, Subtrial), nrow)
PercMissingSub_postbox$PercMissing_Post <- 1 - PercMissingSub_postbox$V1/(ball_reappear_row_rl + missing_post_cutoff_row - ball_behind_row_rl)
PercMissingSub_postbox1 <- ddply(subset(data_postbox, data_postbox$fixationX != -10000 & 
                                          data_postbox$Subtrial %% 2 != 0),
                                          .(ID, Participant, Trial, Subtrial), nrow)
PercMissingSub_postbox1$PercMissing_Post <- 1 - PercMissingSub_postbox1$V1/(ball_reappear_row_lr + missing_post_cutoff_row - ball_behind_row_lr + 1)
PercMissingSub_postbox <- merge(PercMissingSub_postbox, PercMissingSub_postbox1, all = TRUE)
remove(PercMissingSub_postbox1)

## Merging pre- and post-box missing data percentages. ----------------

PercMissingSub <- merge(PercMissingSub_postbox, PercMissingSub_prebox, by = c('ID', 'Participant', 'Trial', 'Subtrial'), all = TRUE)
PercMissingSub$PercMissing_Post[is.na(PercMissingSub$PercMissing_Post)] <- 1
PercMissingSub$PercMissing_Pre[is.na(PercMissingSub$PercMissing_Pre)] <- 1
remove(PercMissingSub_postbox, PercMissingSub_prebox)

## Determining if baby fixated to ball prior to ball going behind box. ----------------

# Creating data subset of only looks to the ball.

dataL2B <- subset(data_prebox, data_prebox$fixated == 'yes')

# Assigning row an ID based on consecutive looks to ball.

i <- 2
dataL2B$consec <- 1

while (i<=nrow(dataL2B)){
  ifelse(as.numeric(dataL2B$TrialRowNum[i]) - as.numeric(dataL2B$TrialRowNum[i-1]) != 1, 
         dataL2B$consec[i] <- as.numeric(dataL2B$consec[i-1]) + 1,
         dataL2B$consec[i] <- as.numeric(dataL2B$consec[i-1]))
  i <- i+1
}

# Filtering out looks to ball with fewer than 5 rows of looks.
# Done by: creating freq of "consec" column; filtering out freq < 5; removing freq column when done with it.

setDT(dataL2B)
dataL2B <- dataL2B[,n:=.N,consec][n>(fix_length_row - 1),,][,n:=NULL]

# Filtering out duplicates. 

dataL2B <- as.data.frame(dataL2B)
dataL2B <- dataL2B[!duplicated(dataL2B[,c("ID","Participant", "Trial", "Subtrial")]), ]

# Getting only the trials that are useable.

data_useabletrials <- merge(dataL2B, PercMissingSub, by = c('ID','Participant','Trial','Subtrial'), all = TRUE)
data_useabletrials_only <- subset(data_useabletrials, time != 'na' & PercMissing_Pre < missing_threshold & PercMissing_Post < missing_threshold)

# Combining back the rest of the rows for the useable subtrials.
 
selectedRows <- (data$ID %in% data_useabletrials_only$ID)
dfReduced <- data[selectedRows,]

#### CREATING DATASET SUMMARY --------------------------------

## Determining whether look was anticipatory/latency of look to ball. ----------------

# Reducing dataset to looks to anticipatory region (if any looks at all).

dfReduced_anticp <- subset(dfReduced, dfReduced$fixationRegion == 3 & dfReduced$Subtrial %% 2 != 0)
dfReduced_anticp1 <- subset(dfReduced, dfReduced$fixationRegion == 1 & dfReduced$Subtrial %% 2 == 0)
dfReduced_anticp <- merge(dfReduced_anticp, dfReduced_anticp1, all = TRUE)
remove(dfReduced_anticp1)

# Removing rows that are too soon & only keeping first look to anticipatory region. 

dfReduced_anticp<-dfReduced_anticp[!(dfReduced_anticp$time <= (al_not_before + (subtrial_length_time*(dfReduced_anticp$Subtrial-1)))),]
dfReduced_anticp <- dfReduced_anticp[!duplicated(dfReduced_anticp[,c("ID")]), ]

#Determining latency and whether anticipatory for all useable trials.

i <- 0
dfReduced_anticp$Latency <- no_look_label
dfReduced_anticp$Anticipatory <- 0

while(i<=nrow(dfReduced_anticp)){
         dfReduced_anticp$Latency[i] <- (ifelse(dfReduced_anticp$Subtrial[i] %% 2 != 0, 
                  (sum(dfReduced_anticp$time[i], -(subtrial_length_time*(as.numeric(dfReduced_anticp$Subtrial[i]) - 1)), -ball_reappear_time_lr)),
                  (sum(dfReduced_anticp$time[i], -(subtrial_length_time*(as.numeric(dfReduced_anticp$Subtrial[i]) - 1)), -ball_reappear_time_rl))))
         dfReduced_anticp$Anticipatory[i] <- ifelse(dfReduced_anticp$Latency[i] < al_buffer, 1, 0)
         i <- sum(i,1)
        }

## Labeling trials without looks without appropriate label. ----------------

# Creating a column to define the type of missing data.

data_useabletrials$missingType <- ifelse(data_useabletrials$fixated == "NA", "NA", 0)
data_useabletrials[data_useabletrials$PercMissing_Post > .50 | data_useabletrials$PercMissing_Pre > .50,]$missingType <- missing_data_label
data_useabletrials[is.na(data_useabletrials$missingType),]$missingType <- no_fixation_label
data_useabletrials <- data_useabletrials[,c("ID", "Participant", "Trial", "Subtrial", "missingType")]

Looksbytrials <- merge(dfReduced_anticp, PercMissingSub, by = c('ID', 'Participant', 'Trial', 'Subtrial'), all = TRUE)
Looksbytrials <- merge(Looksbytrials, data_useabletrials, by = c('ID', 'Participant', 'Trial', 'Subtrial'), all = TRUE)
Looksbytrials[is.na(Looksbytrials$Latency) & Looksbytrials$missingType == 0,]$missingType = no_look_label

# Assigning that missing type label to the latency and anticipatory columns. 

i <- 0

while(i<=nrow(Looksbytrials)){
  Looksbytrials$Latency[i] <- ifelse(Looksbytrials$missingType[i] < 0, Looksbytrials$missingType[i], Looksbytrials$Latency[i])
  Looksbytrials$Anticipatory[i] <- ifelse(Looksbytrials$missingType[i] < 0, Looksbytrials$missingType[i], Looksbytrials$Anticipatory[i])
  i <- sum(i,1)
}

## Computing frequencies anticipatory looks, total looks, and useable trials. ----------------

# Using only the relevant output. 

LooksbytrialsOutput <- Looksbytrials[,c("ID", "Participant","Trial", "Subtrial", "time", "Latency", "Anticipatory", "missingType")]

# Counting total number of anticipatory looks.

TotAL <- as.data.frame(subset(LooksbytrialsOutput, LooksbytrialsOutput$Anticipatory == "1"))
TotAL <- count(TotAL, Participant)
names(TotAL)[names(TotAL)=="n"] <- "totalAL"

# Counting total number of looks (whether anticipatory or not).

TotL <- as.data.frame(subset(LooksbytrialsOutput, LooksbytrialsOutput$Latency >= (no_look_label + 1)))
TotL <- count(TotL, Participant)
names(TotL)[names(TotL)=="n"] <- "totalLooks"

# Counting total number of useable trials.

TotTrials <- as.data.frame(subset(LooksbytrialsOutput, LooksbytrialsOutput$Latency >= no_look_label))
TotTrials <- count(TotTrials, Participant)
names(TotTrials)[names(TotTrials)=="n"] <- "totalUseableTrials"

# Combining TotAL, TotL, and TotTrials and the removing those dataframes.

sumAL <- join_all(list(TotAL,TotL,TotTrials), by = 'Participant', type = 'full')
sumAL[is.na(sumAL)] <- 0
rm(TotAL,TotL,TotTrials)

## Converting latency and anticipatory data per participant for each subtrial to wide format. ----------------

# Converting latency dataset to wide, replacing missing with -9999, and reordering columns in right order

LooksbytrialsOutput$TrialSubTrial <- as.numeric(with(LooksbytrialsOutput, paste0(Trial, Subtrial)))
LooksbytrialsOutput1 <- LooksbytrialsOutput[,c("TrialSubTrial", "Participant", "Latency")]
LooksbytrialsOutput1 <- LooksbytrialsOutput1[order(LooksbytrialsOutput$TrialSubTrial),]
# LooksbytrialsOutput1 = dplyr::rename(LooksbytrialsOutput1, Latency_trialsub=Latency)

LooksbytrialsOutput_wide <- reshape(LooksbytrialsOutput1, 
                              timevar = ("TrialSubTrial"),
                              idvar = "Participant",
                              direction = "wide")  
LooksbytrialsOutput_wide[is.na(LooksbytrialsOutput_wide)] <- missing_data_label
LooksbytrialsOutput_wide <- LooksbytrialsOutput_wide[,c(1:10, 38:40, 11:19, 41:43, 20:28, 44:46, 29:37, 47:48)]

# Converting anticipatory dataset to wide.

LooksbytrialsOutput_anticp <- LooksbytrialsOutput[,c("TrialSubTrial", "Participant", "Anticipatory")]
LooksbytrialsOutput_anticp <- LooksbytrialsOutput_anticp[order(LooksbytrialsOutput$TrialSubTrial),]
# LooksbytrialsOutput_anticp = dplyr::rename(LooksbytrialsOutput_anticp, AL_trialsub=Anticipatory)

LooksbytrialsOutput_anticp_wide <- reshape(LooksbytrialsOutput_anticp, 
                                    timevar = ("TrialSubTrial"),
                                    idvar = "Participant",
                                    direction = "wide")  
LooksbytrialsOutput_anticp_wide[is.na(LooksbytrialsOutput_anticp_wide)] <- missing_data_label
LooksbytrialsOutput_anticp_wide <- LooksbytrialsOutput_anticp_wide[,c(1:10, 38:40, 11:19, 41:43, 20:28, 44:46, 29:37, 47:48)]

# Merging the two wide datasets. 

LooksbytrialsOutput_wide <- merge (LooksbytrialsOutput_wide, LooksbytrialsOutput_anticp_wide, by.x = "Participant", by.y = 1, all = TRUE)

# Merging counts of looks with the wide dataset. 

sumAL <- merge(sumAL, LooksbytrialsOutput_wide, by.x = "Participant", by.y = 1, all = TRUE)

## Calculating proportions of anticipatory looks/looks to ball. ----------------

# Calculating for all trials. 

sumAL$PropAL <- sumAL$totalAL/sumAL$totalUseableTrials
sumAL$PropTotalLooks <- sumAL$totalLooks/sumAL$totalUseableTrials

# Calculating for first ten trials.

# Identifying trials that are not useable and keeping only the useable trials.

Looksby_best10 <- LooksbytrialsOutput
Looksby_best10$count <- ifelse(Looksby_best10$Latency >= no_look_label, 1, 0)
Looksby_best10 <- subset(Looksby_best10, count == 1)
Looksby_best10 <- Looksby_best10[order(Looksby_best10$ID),]

# Creating a counting column that assigns ascending sequence for each row per participant and resets with new participant.

Looksby_best10_DT <- data.table(Looksby_best10)
Looksby_best10_DT[, countID := seq_len(.N), by = Participant]
Looksby_best10_DT[, countID := 1:.N, by = Participant]

# Subsetting just the first 10 trials (so now we only have first 10 useable trials).

Looksby_best10 <- as.data.frame(subset(Looksby_best10_DT, countID <= 10))

# Setting no look trials to NA and then getting means for latency and ALs by participant.

Looksby_best10$Latency[Looksby_best10$Latency == no_look_label] <- NA
means_first10 <- aggregate(Latency ~ Participant, data=Looksby_best10, mean)
Looksby_best10$Anticipatory[Looksby_best10$Anticipatory == no_look_label] <- NA
means_first10_a <- aggregate(Anticipatory ~ Participant, data=Looksby_best10, mean)
means_first10 <- merge(means_first10, means_first10_a, by = 'Participant', all = TRUE)

# Subsetting to just trials of the first 10 that are anticipatory. 

Looksby_best10_anticp <- subset(Looksby_best10, Anticipatory == 1)
means_first10_a <- aggregate(Latency ~ Participant, data=Looksby_best10_anticp, mean)
means_first10 <- merge(means_first10, means_first10_a, by = 'Participant', all = TRUE)
remove(means_first10_a)

names(means_first10)[names(means_first10)=="Latency.x"] <- "TotalLat_avg_first10"
names(means_first10)[names(means_first10)=="Latency.y"] <- "AnticpLat_avg_first10"
names(means_first10)[names(means_first10)=="Anticipatory"] <- "AL_avg_first10"

# Merging first 10 data with data summary. 

sumAL <- merge(sumAL, means_first10, by = 'Participant', all = TRUE)

## Exporting data. ----------------

write.csv(sumAL, file = export_filename, row.names = FALSE)
