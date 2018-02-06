#### GETTING SET UP: READ ME. --------------------------------

## Important things you must do/make sure are right. ----------------

# Are you running a right or left condition?

condition <- "left" #put "right" or "left"

# What do you want the export file to be called?

export_filename <- "Int-Mech2_Success_Left_T1.csv"

## A few notes about the script. ----------------

# This script was written for R Version 3.3.2 (Sincere Pumpkin Patch).
# Please update R and reinstall packages as some parts of the script may not work properly in previous versions of R.

# This script is meant to handle eyetracking datasets that have already been cleaned by log_filter.php. 

## Installing and loading packages. ----------------

# To install a package, use: install.packages("plyr").
# Packages must be reloaded each time R is opened but only need to be installed once. To load:

library(plyr)
library(data.table)

# If the following error comes up while trying to load the library:
# Error in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]): there is no package called 'XXXXX'
# Error: package or namespace load failed for 'plyr'
# That means that you do not have XXXXX package installed and you need to install it for other packages to work

#### IMPORTING THE DATA --------------------------------

# Input correct file directory (and use / (not \) to denote different folder levels)

data <- read.csv("C:/Users/kelse/Desktop/ETdata-left.csv", header=T)

####  SETTING UP PARAMETERS --------------------------------

X                        <- data$FixationX #which column in the datafile measures X coordinate of gaze 
Y                        <- data$FixationY #which column in the datafile measures Y coordinate of gaze 
miss_data_cutoff_time1   <- 6083 #cutoff time (in ms) for measuring missing data in trial (just trial 1)
miss_data_cutoff_time2   <- 4083 #cutoff time (in ms) for measuring missing data in trial (trial 2 - 10)
miss_data_cutoff_row1    <- 365 #cutoff time (in rows) for measuring missing data in trial (just trial 1)
miss_data_cutoff_row2    <- 245 #cutoff time (in rows) for measuring missing data in trial (jtrial 2 - 10)
ball_coord_X_left        <- 485 #X coordinate for center of ball (when ball is on left side)
ball_coord_X_right       <- 1435 #X coordinate for center of ball (when ball is on right side)
ball_coord_Y             <- 842 #Y coordinate for center of ball 
ball_AOI_radius          <- 100 #radius of the ball AOI 
ball_looks_too_early1    <- 2750 #looks to the ball (on trial 1) that are too early to be anticipatory
ball_looks_too_early2    <- 750  #looks to the ball (on trial 2 - 10) that are too early to be anticipatory
missing_data_threshold   <- .50 #point at which there's too much missing data for trial to be used
not_useable_label        <- -9999 #too much missing data/trial not useable
no_look_label            <- -99 #no look to ball on this trial but trial is useable
anticipatory_cutoff1     <- 5500 #looks count as anticipatory before this time (in ms; for trial 1)
anticipatory_cutoff2     <- 3500 #looks count as anticipatory before this time (in ms; for trial 2 - 10)

#### DETERMINING WHICH TRIALS ARE USEABLE BASED ON MISSING DATA. --------------------------------

# Subsetting data, treating trial one differently because of extra pause at beginning.

data_1 <- subset(data, data$Time <= miss_data_cutoff_time1 & data$Trial == 1, select = c(TrialRowNum, Participant, Trial))
data_210 <- subset(data, data$Time <= miss_data_cutoff_time2 & data$Trial != 1, select = c(TrialRowNum, Participant, Trial))

# Calculating percentage of usable data.

perc_miss <- ddply(data_210, .(Participant, Trial), nrow)
perc_miss$PercMissing <- 1 - (perc_miss$V1/miss_data_cutoff_row2)
perc_miss1 <- ddply(data_1, .(Participant, Trial), nrow)
perc_miss1$PercMissing <- 1 - (perc_miss1$V1/miss_data_cutoff_row1)

# Combining the dataframes.

miss_data_perc <- rbind(perc_miss, perc_miss1)
miss_data_perc <- miss_data_perc[with(miss_data_perc, order(Participant, Trial)), ]
rm(perc_miss1, perc_miss, data_1, data_210)

#### CREATING THE AREA OF INTEREST FOR THE BALL. --------------------------------

# Creating a circular area of interest around the ball.

ifelse(condition == "right", 
       data$distance <-  with(data,(abs(sqrt(((X-ball_coord_X_right)^2)+((Y-ball_coord_Y)^2))))),
       data$distance <-  with(data,(abs(sqrt(((X - ball_coord_X_left) ^ 2)+((Y - ball_coord_Y) ^ 2))))))

# Determining if look is within the AOI. 

data$ball <- ifelse(data$distance <= ball_AOI_radius, TRUE, FALSE)

#### FILTERING OUT NON-FIXATIONS TO BALL AND NON-FIRST LOOKS TO BALL.  --------------------------------

# Creating data subset of only looks to the ball.

dataL2B <- subset(data, data$ball == TRUE)

# Filtering out looks to the ball before 2750 ms (for Trials 1) or 750 ms (for Trials 2 - 10).

dataL2B<-dataL2B[!(dataL2B$Trial == 1 & dataL2B$Time <= ball_looks_too_early1),]
dataL2B<-dataL2B[!(dataL2B$Trial >= 2 & dataL2B$Time <= ball_looks_too_early2),]

# Assigning row an ID based on consecutive looks to ball.

i <- 2
dataL2B$consec <- 1

while (i<=nrow(dataL2B)){
  ifelse(as.numeric(dataL2B$TrialRowNum[i])-as.numeric(dataL2B$TrialRowNum[i-1]) != 1, 
         dataL2B$consec[i] <- as.numeric(dataL2B$consec[i-1]) + 1,
         dataL2B$consec[i] <- as.numeric(dataL2B$consec[i-1]))
  i <- i+1
}

# Filtering out looks to ball with fewer than 5 rows of looks.
# Done by: creating freq of "consec" column; filtering out freq < 5; removing freq column when done with it.

setDT(dataL2B)
dataL2B <- dataL2B[,n:=.N,consec][n>4,,][,n:=NULL]

# Filtering out duplicates.

dataL2B <- as.data.frame(dataL2B)
dataL2B1 <- dataL2B[!duplicated(dataL2B[,c("Participant", "Trial")]), ]

#### CREATING DATASET SUMMARY --------------------------------

# Labeling trials as no look or too much missing data.

looking_data <- dataL2B1[2:4]

looking_data <- merge(looking_data, miss_data_perc, by = c('Participant','Trial'), all = TRUE)
looking_data[looking_data$PercMissing < missing_data_threshold & is.na(looking_data$Time),]$Time = no_look_label
looking_data$Time <- ifelse(looking_data$PercMissing >= missing_data_threshold, not_useable_label, looking_data$Time)

# Identifying looks as anticipatory or not.

looking_data$anticipatory <- ifelse(looking_data$Trial == 1, 
                               ifelse(looking_data$Time <= anticipatory_cutoff1 & looking_data$Time > 0, 1, 0), 
                               ifelse(looking_data$Time <= anticipatory_cutoff2 & looking_data$Time > 0, 1, 0))

## Counting frequencies of looks. ----------------

# Counting total number of anticipatory looks.

TotAL <- as.data.frame(subset(looking_data, looking_data$anticipatory == 1))
TotAL1 <- count(TotAL, Participant)
names(TotAL1)[names(TotAL1) == "n"] <- "totalAL"

# Counting total number of looks.

TotL <- as.data.frame(subset(looking_data, looking_data$Time >= 0))
TotL1 <- count(TotL, Participant)
names(TotL1)[names(TotL1) == "n"] <- "totalLooks"

# Counting total number of useable trials.

TotTrials <- as.data.frame(subset(looking_data, looking_data$Time >= no_look_label))
TotTrials <- count(TotTrials, Participant)
names(TotTrials)[names(TotTrials) == "n"] <- "totalUseableTrials"

# Combining TotAL, TotL, and TotTrials and the removing those dataframes.

summary_data <- join_all(list(TotAL1,TotL1,TotTrials), by = 'Participant', type = 'full')
summary_data[is.na(summary_data)] <- 0
rm(TotAL1,TotL1,TotTrials)

## Converting latency and anticipatory data per participant for each subtrial to wide format. ----------------

# Converting latency dataset to wide, replacing missing with -9999, and reordering columns in right order.

looking_data1 <- looking_data[1:3]

looking_data_wide <- reshape(looking_data1, 
                              timevar = "Trial",
                              idvar = "Participant",
                              direction = "wide")
looking_data_wide[is.na(looking_data_wide)] <- not_useable_label


# Merging counts of looks with trial by trial looking time.

summary_data <- merge(summary_data, looking_data_wide, by.x = "Participant", by.y = 1, all = TRUE)

# Converting latency dataset to wide, replacing missing with -9999, and reordering columns in right order.

looking_data1 <- looking_data[,c(1:2,6)]

looking_data_wide <- reshape(looking_data1, 
                             timevar = "Trial",
                             idvar = "Participant",
                             direction = "wide")
looking_data_wide[is.na(looking_data_wide)] <- not_useable_label

# Merging counts of looks with trial by trial looking time.

summary_data <- merge(summary_data, looking_data_wide, by.x = "Participant", by.y = 1, all = TRUE)

# Calculating average latency of anticipatory looks and adding to sumAL, replacing values for participants with no ALs.

TotAL <- as.data.frame(subset(TotAL, TotAL$Trial > 1))
avgAnticLatency <- aggregate(TotAL[, 3], list(TotAL$Participant), mean)
summary_data <- merge(summary_data, avgAnticLatency, by.x = "Participant", by.y = 1,  all = TRUE)
summary_data[is.na(summary_data)] <- no_look_label
names(summary_data)[names(summary_data)=="x"] <- "avgAnticipatoryLatency"

# Calculating average latency of all looks and adding to sumAL, replacing values for participants with no ALs.

TotL <- as.data.frame(subset(TotL, TotL$Trial > 1))
avgAllLatency <- aggregate(TotL[, 3], list(TotL$Participant), mean)
summary_data <- merge(summary_data, avgAllLatency, by.x = "Participant", by.y = 1,  all = TRUE)
summary_data[is.na(summary_data)] <- no_look_label
names(summary_data)[names(summary_data)=="x"] <- "avgAllLooksLatency"

# Calculating proportion of anticipatory looks and looks to the ball.

summary_data$PropAL <- summary_data$totalAL/summary_data$totalUseableTrials
summary_data$PropLooksToBall <- summary_data$totalLooks/summary_data$totalUseableTrials

## Exporting data. ----------------

write.csv(summary_data, file = export_filename, row.names = FALSE)



