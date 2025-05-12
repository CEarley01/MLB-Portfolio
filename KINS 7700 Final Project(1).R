### KINS 7700 Final Project - Connor Earley

rm(list=ls())
options(scipen = 4)

library(tidyverse)
library(glmnet)

### Setting Working Directory
setwd("/Volumes/Flashdrive/All_Games")
TD = read.csv("Trackman.csv")


### Cleaning Data
## Removing all unnecessary variables
TD = subset(TD,select = -c(PitchofPA))
TD = subset(TD,select = -c(TaggedPitchType,RunsScored,Notes,RelSpeed,VertRelAngle,
                           HorzRelAngle,SpinRate,SpinAxis,Tilt,RelHeight,RelSide,
                           Extension,VertBreak,InducedVertBreak,HorzBreak,
                           PlateLocHeight,PlateLocSide,ZoneSpeed,VertApprAngle,
                           HorzApprAngle,ZoneTime,ExitSpeed,Angle,Direction,
                           HitSpinRate))
TD = subset(TD,select = -c(PositionAt110X,PositionAt110Y,PositionAt110Z,Distance,
                           LastTrackedDistance,Bearing,HangTime,pfxx,pfxz,x0,y0,
                           z0,vx0,vy0,vz0,ax0,ay0,az0,Stadium,Level,EffectiveVelo,
                           MaxHeight,MeasuredDuration,SpeedDrop,PitchLastMeasuredX,
                           PitchLastMeasuredY,PitchLastMeasuredZ,ContactPositionX,
                           ContactPositionY,ContactPositionZ))
TD = subset(TD,select = -c(UTCTime,LocalDateTime,UTCDateTime,AutoHitType,System,
                           Catcher,CatcherId,CatcherThrows,CatcherTeam,
                           PitchTrajectoryXc0,PitchTrajectoryXc1,PitchTrajectoryXc2,
                           PitchTrajectoryYc0,PitchTrajectoryYc1,PitchTrajectoryYc2,
                           PitchTrajectoryZc0,PitchTrajectoryZc1,PitchTrajectoryZc2,
                           HitSpinAxis,HitTrajectoryXc0,HitTrajectoryXc1,HitTrajectoryXc2,
                           HitTrajectoryXc3,HitTrajectoryXc4,HitTrajectoryXc5,
                           HitTrajectoryXc6,HitTrajectoryXc7,HitTrajectoryXc8,
                           HitTrajectoryYc0,HitTrajectoryYc1,HitTrajectoryYc2,
                           HitTrajectoryYc3,HitTrajectoryYc4,HitTrajectoryYc5,
                           HitTrajectoryYc6,HitTrajectoryYc7,HitTrajectoryYc8,
                           HitTrajectoryZc0,HitTrajectoryZc1,HitTrajectoryZc2,
                           HitTrajectoryZc3,HitTrajectoryZc4,HitTrajectoryZc5,
                           HitTrajectoryZc6,HitTrajectoryZc7,HitTrajectoryZc8))
TD = subset(TD,select = -c(ThrowSpeed,PopTime,ExchangeTime,TimeToBase,
                           CatchPositionX,CatchPositionY,CatchPositionZ,
                           ThrowPositionX,ThrowPositionY,ThrowPositionZ,
                           BasePositionX,BasePositionY,BasePositionZ,
                           ThrowTrajectoryXc0,ThrowTrajectoryXc1,ThrowTrajectoryXc2,
                           ThrowTrajectoryYc0,ThrowTrajectoryYc1,ThrowTrajectoryYc2,
                           ThrowTrajectoryZc0,ThrowTrajectoryZc1,ThrowTrajectoryZc2,
                           PitchReleaseConfidence,PitchLocationConfidence,
                           PitchMovementConfidence,HitLaunchConfidence,
                           HitLandingConfidence,CatcherThrowCatchConfidence,
                           CatcherThrowReleaseConfidence,CatcherThrowLocationConfidence))
TD = subset(TD,select = -c(Time,Inning,Outs,Balls,Strikes,AutoPitchType,
                           PitchUID,HomeTeam,AwayTeam,GameUID,UTCDate,
                           HomeTeamForeignID,AwayTeamForeignID,GameForeignID,PlayID,
                           Top.Bottom,PitcherSet,OutsOnPlay))


## Next step make sure no NAs & no weird things occur 
unique(TD$PitchNo)
unique(TD$Date)
unique(TD$Pitcher) 
unique(TD$PitcherId) 
unique(TD$PitcherThrows) 
unique(TD$PitcherTeam) 
unique(TD$Batter) 
unique(TD$BatterId) 
unique(TD$BatterSide) 
unique(TD$BatterTeam) 
unique(TD$PitchCall) 
unique(TD$KorBB) 
unique(TD$TaggedHitType) 
unique(TD$PlayResult) 
unique(TD$League) 
unique(TD$GameID) 


## Gets rid of undefined pitches
TD = TD[!(TD$KorBB == "Undefined" & TD$PlayResult == "Undefined" & TD$TaggedHitType == "Undefined"), ]

## Gets rid of any pitch where a stolen base or caught stealing was the result
TD = TD %>% filter(PlayResult != "StolenBase")
TD = TD %>% filter(PlayResult != "CaughtStealing")


## Replaced the undefined in PlayResult with BB, SO, HitByPitch
TD$PlayResult = ifelse(TD$PitchCall == "HitByPitch" & TD$PlayResult == "Undefined",
                       TD$PlayResult[TD$PlayResult == "HitByPitch"],
                       TD$PlayResult)
# Replaces NAs created in previous line
TD$PlayResult = TD$PlayResult %>% replace_na("HitbyPitch")

# Replace "Undefined" values in PlayResult w/ K & BB
TD$PlayResult[TD$PlayResult == "Undefined"] = TD$KorBB[TD$PlayResult == "Undefined"]

# Checks value to ensure everything ran correctly 
unique(TD$PlayResult)
table(TD$PlayResult)

TD$Batter[which(TD$PlayResult=="Undefined")]


## Gets rid of the weird values in PlayResult
TD = TD %>% filter(PlayResult %in% c("Single", "FieldersChoice", "Strikeout", "Out", "Double", "Walk",
                                     "HomeRun", "Error", "Triple", "Sacrifice", "HitByPitch"))

## Unnecessary columns w/new column created
TD = TD %>% select(-c(PitchCall, KorBB))

## Gets rid of that one miss st pitcher (outlier pitcher)
TD = TD %>% filter(PitcherThrows != "Both")


## Gets rid of duplicated rows
TD = TD[!duplicated(TD), ]

## Gets rid of Non-D1 teams 
TD = TD %>% filter(League %in% c("A10", "ACC", "ASUN", "AMEAST", "AMER", "BIG10",
                                 "BIG12", "BEAST", "BSOU", "BW", "CUSA", "CAA",
                                 "HORZ", "IVY", "MAAC", "MAC", "MW", "PAC12", "OVC",
                                 "NEC", "PAT", "SEC", "SOCON", "SWAC", "SLAND", 
                                 "SUM", "SBELT", "WAC", "WCC"))


## Next step creates separate tables for matchups 
## Batter Matchups
# Righty vs Righty (Batters)
RvR = TD %>% filter(BatterSide != "Left")
RvR = RvR %>% filter(PitcherThrows != "Left")
RvR = RvR %>% filter(PitcherThrows != "Undefined")

#Check for correct data 
unique(BID_RHB_V_RHP$BatterSide)
unique(BID_RHB_V_RHP$PitcherThrows)


## Next step creates cunulative sums for all individual games
## creates df for each handedness with cumulated stats
His_RHB_V_RHP = RvR %>%
  
  # Ensure data is sorted by date
  arrange(GameID) %>%
  
  # This creates a running total of each event
  mutate(cumulative_strikeouts = cumsum(PlayResult == "Strikeout"),
         cumulative_singles = cumsum(PlayResult == "Single"),
         cumulative_doubles = cumsum(PlayResult == "Double"),
         cumulative_triples = cumsum(PlayResult == "Triple"),
         cumulative_fielders_choice = cumsum(PlayResult == "FieldersChoice"),
         cumulative_errors = cumsum(PlayResult == "Error"),
         cumulative_home_runs = cumsum(PlayResult == "HomeRun"),
         cumulative_walks = cumsum(PlayResult == "Walk"),
         cumulative_outs = cumsum(PlayResult == "Out"),
         cumulative_sacrifice = cumsum(PlayResult == "Sacrifice"),
         cumulative_hit_by_pitch = cumsum(PlayResult == "HitByPitch"),
         cumulative_plays = row_number(),
         
         # This creates a running rate of each event
         so_rate = cumulative_strikeouts / cumulative_plays,
         obp = (cumulative_singles + cumulative_doubles + cumulative_triples + 
                  cumulative_home_runs + cumulative_errors + cumulative_fielders_choice +
                  cumulative_walks + cumulative_hit_by_pitch)/cumulative_plays,
         bipo = (cumulative_outs + cumulative_sacrifice)/cumulative_plays) %>%
  
  # This creates an average of the running metrics
  group_by(GameID)

# Creates summy variables for each team 
His_RHB_V_RHP = dummy_cols(His_RHB_V_RHP,select_columns = "PlayResult")

# Removes unecessary variables 
His_RHB_V_RHP = subset(His_RHB_V_RHP,select = -c(BatterId, PlateID, Pitcher, PitcherId, 
                                                 PitcherThrows, Batter, BatterSide, 
                                                 League, PitchNo, PAofInning))

## Creates running totals for stats in df 
# Initialize the running total and a new total indicator column
# Cumulative plays 
His_RHB_V_RHP$cumulative_plays <- NA
cumulative_plays <- 0

# Loop through the rows to calculate the running total
for (i in 1:nrow(His_RHB_V_RHP)) {
  if (i == 1 || His_RHB_V_RHP$GameID[i] != His_RHB_V_RHP$GameID[i-1]) {
    # Reset running total if the value is different from the previous row
    cumulative_plays <- 1  # Start the running total from 1 (or any number you prefer)
  } else {
    # Add to running total if the value is the same as the previous row
    cumulative_plays <- cumulative_plays + 1
  }
  His_RHB_V_RHP$cumulative_plays[i] <- cumulative_plays
}


# Cumulative strikeouts 
# Initialize the running total column
His_RHB_V_RHP$cumulative_strikeouts <- NA
cumulative_strikeouts <- 0

# Loop through the rows to calculate the running total
for (i in 1:nrow(His_RHB_V_RHP)) {
  if (i == 1 || His_RHB_V_RHP$GameID[i] != His_RHB_V_RHP$GameID[i-1]) {
    # Reset running total if the value is different from the previous row
    cumulative_strikeouts <- His_RHB_V_RHP$PlayResult_Strikeout[i]
  } else {
    # Add to running total if the value is the same as the previous row
    cumulative_strikeouts <- cumulative_strikeouts + His_RHB_V_RHP$PlayResult_Strikeout[i]
  }
  His_RHB_V_RHP$cumulative_strikeouts[i] <- cumulative_strikeouts
}


# Cumulative singles 
# Initialize the running total column
His_RHB_V_RHP$cumulative_singles <- NA
cumulative_singles <- 0

# Loop through the rows to calculate the running total
for (i in 1:nrow(His_RHB_V_RHP)) {
  if (i == 1 || His_RHB_V_RHP$GameID[i] != His_RHB_V_RHP$GameID[i-1]) {
    # Reset running total if the value is different from the previous row
    cumulative_singles <- His_RHB_V_RHP$PlayResult_Single[i]
  } else {
    # Add to running total if the value is the same as the previous row
    cumulative_singles <- cumulative_singles + His_RHB_V_RHP$PlayResult_Single[i]
  }
  His_RHB_V_RHP$cumulative_singles[i] <- cumulative_singles
}

# Cumulative doubles
# Initialize the running total column
His_RHB_V_RHP$cumulative_doubles <- NA
cumulative_doubles <- 0

# Loop through the rows to calculate the running total
for (i in 1:nrow(His_RHB_V_RHP)) {
  if (i == 1 || His_RHB_V_RHP$GameID[i] != His_RHB_V_RHP$GameID[i-1]) {
    # Reset running total if the value is different from the previous row
    cumulative_doubles <- His_RHB_V_RHP$PlayResult_Double[i]
  } else {
    # Add to running total if the value is the same as the previous row
    cumulative_doubles <- cumulative_doubles + His_RHB_V_RHP$PlayResult_Double[i]
  }
  His_RHB_V_RHP$cumulative_doubles[i] <- cumulative_doubles
}

# Cumulative triples
# Initialize the running total column
His_RHB_V_RHP$cumulative_triples <- NA
cumulative_triples<- 0

# Loop through the rows to calculate the running total
for (i in 1:nrow(His_RHB_V_RHP)) {
  if (i == 1 || His_RHB_V_RHP$GameID[i] != His_RHB_V_RHP$GameID[i-1]) {
    # Reset running total if the value is different from the previous row
    cumulative_triples <- His_RHB_V_RHP$PlayResult_Triple[i]
  } else {
    # Add to running total if the value is the same as the previous row
    cumulative_triples <- cumulative_triples + His_RHB_V_RHP$PlayResult_Triple[i]
  }
  His_RHB_V_RHP$cumulative_triples[i] <- cumulative_triples
}

# Cumulative fieldersc choice 
# Initialize the running total column
His_RHB_V_RHP$cumulative_fielders_choice <- NA
cumulative_fielders_choice <- 0

# Loop through the rows to calculate the running total
for (i in 1:nrow(His_RHB_V_RHP)) {
  if (i == 1 || His_RHB_V_RHP$GameID[i] != His_RHB_V_RHP$GameID[i-1]) {
    # Reset running total if the value is different from the previous row
    cumulative_fielders_choice <- His_RHB_V_RHP$PlayResult_FieldersChoice[i]
  } else {
    # Add to running total if the value is the same as the previous row
    cumulative_fielders_choice <- cumulative_fielders_choice + His_RHB_V_RHP$PlayResult_FieldersChoice[i]
  }
  His_RHB_V_RHP$cumulative_fielders_choice[i] <- cumulative_fielders_choice
}

# Cumulative errors 
# Initialize the running total column
His_RHB_V_RHP$cumulative_errors <- NA
cumulative_errors <- 0

# Loop through the rows to calculate the running total
for (i in 1:nrow(His_RHB_V_RHP)) {
  if (i == 1 || His_RHB_V_RHP$GameID[i] != His_RHB_V_RHP$GameID[i-1]) {
    # Reset running total if the value is different from the previous row
    cumulative_errors <- His_RHB_V_RHP$PlayResult_Error[i]
  } else {
    # Add to running total if the value is the same as the previous row
    cumulative_errors <- cumulative_errors + His_RHB_V_RHP$PlayResult_Error[i]
  }
  His_RHB_V_RHP$cumulative_errors[i] <- cumulative_errors
}

# Cumulative home runs 
# Initialize the running total column
His_RHB_V_RHP$cumulative_home_runs <- NA
cumulative_home_runs <- 0

# Loop through the rows to calculate the running total
for (i in 1:nrow(His_RHB_V_RHP)) {
  if (i == 1 || His_RHB_V_RHP$GameID[i] != His_RHB_V_RHP$GameID[i-1]) {
    # Reset running total if the value is different from the previous row
    cumulative_home_runs<- His_RHB_V_RHP$PlayResult_HomeRun[i]
  } else {
    # Add to running total if the value is the same as the previous row
    cumulative_home_runs <- cumulative_home_runs + His_RHB_V_RHP$PlayResult_HomeRun[i]
  }
  His_RHB_V_RHP$cumulative_home_runs[i] <- cumulative_home_runs
}

# Cumulative walks 
# Initialize the running total column
His_RHB_V_RHP$cumulative_walks <- NA
cumulative_walks <- 0

# Loop through the rows to calculate the running total
for (i in 1:nrow(His_RHB_V_RHP)) {
  if (i == 1 || His_RHB_V_RHP$GameID[i] != His_RHB_V_RHP$GameID[i-1]) {
    # Reset running total if the value is different from the previous row
    cumulative_walks <- His_RHB_V_RHP$PlayResult_Walk[i]
  } else {
    # Add to running total if the value is the same as the previous row
    cumulative_walks <- cumulative_walks + His_RHB_V_RHP$PlayResult_Walk[i]
  }
  His_RHB_V_RHP$cumulative_walks[i] <- cumulative_walks
}

# Cumulative outs 
# Initialize the running total column
His_RHB_V_RHP$cumulative_outs <- NA
cumulative_outs <- 0

# Loop through the rows to calculate the running total
for (i in 1:nrow(His_RHB_V_RHP)) {
  if (i == 1 || His_RHB_V_RHP$GameID[i] != His_RHB_V_RHP$GameID[i-1]) {
    # Reset running total if the value is different from the previous row
    cumulative_outs <- His_RHB_V_RHP$PlayResult_Out[i]
  } else {
    # Add to running total if the value is the same as the previous row
    cumulative_outs <- cumulative_outs + His_RHB_V_RHP$PlayResult_Out[i]
  }
  His_RHB_V_RHP$cumulative_outs[i] <- cumulative_outs
}

# Cumulative sacrifice 
# Initialize the running total column
His_RHB_V_RHP$cumulative_sacrifice <- NA
cumulative_sacrifice <- 0

# Loop through the rows to calculate the running total
for (i in 1:nrow(His_RHB_V_RHP)) {
  if (i == 1 || His_RHB_V_RHP$GameID[i] != His_RHB_V_RHP$GameID[i-1]) {
    # Reset running total if the value is different from the previous row
    cumulative_sacrifice<- His_RHB_V_RHP$PlayResult_Sacrifice[i]
  } else {
    # Add to running total if the value is the same as the previous row
    cumulative_sacrifice <- cumulative_sacrifice + His_RHB_V_RHP$PlayResult_Sacrifice[i]
  }
  His_RHB_V_RHP$cumulative_sacrifice[i] <- cumulative_sacrifice
}


# so rate 
# Initialize the percentage column
His_RHB_V_RHP$so_rate <- NA

# Loop through the rows to calculate the percentage
for (i in 1:nrow(His_RHB_V_RHP)) {
  if (i == 1 || His_RHB_V_RHP$GameID[i] != His_RHB_V_RHP$GameID[i-1]) {
    # Reset the percentage if the 'value' is different from the previous row
    His_RHB_V_RHP$so_rate[i] <- His_RHB_V_RHP$cumulative_strikeouts[i] / His_RHB_V_RHP$cumulative_plays[i]
  } else {
    # Add percentage for the same 'value' group
    His_RHB_V_RHP$so_rate[i] <- His_RHB_V_RHP$cumulative_strikeouts[i] / His_RHB_V_RHP$cumulative_plays[i]
  }
}

# obp
# Initialize the percentage column
His_RHB_V_RHP$obp <- NA

# Loop through the rows to calculate the percentage
for (i in 1:nrow(His_RHB_V_RHP)) {
  if (i == 1 || His_RHB_V_RHP$GameID[i] != His_RHB_V_RHP$GameID[i-1]) {
    # Reset the percentage if the 'value' is different from the previous row
    His_RHB_V_RHP$obp[i] <- (His_RHB_V_RHP$cumulative_singles[i] + His_RHB_V_RHP$cumulative_doubles[i] + 
                               His_RHB_V_RHP$cumulative_triples[i] + His_RHB_V_RHP$cumulative_home_runs[i] + 
                               His_RHB_V_RHP$cumulative_errors[i] + His_RHB_V_RHP$cumulative_fielders_choice[i]+ 
                               His_RHB_V_RHP$cumulative_walks[i] + His_RHB_V_RHP$cumulative_hit_by_pitch[i]) / His_RHB_V_RHP$cumulative_plays[i]
  } else {
    # Add percentage for the same 'value' group
    His_RHB_V_RHP$obp[i] <- (His_RHB_V_RHP$cumulative_singles[i] + His_RHB_V_RHP$cumulative_doubles[i] + 
                               His_RHB_V_RHP$cumulative_triples[i] + His_RHB_V_RHP$cumulative_home_runs[i] + 
                               His_RHB_V_RHP$cumulative_errors[i] + His_RHB_V_RHP$cumulative_fielders_choice[i]+ 
                               His_RHB_V_RHP$cumulative_walks[i] + His_RHB_V_RHP$cumulative_hit_by_pitch[i]) / His_RHB_V_RHP$cumulative_plays[i]
  }
}

# bipo
# Initialize the percentage column
His_RHB_V_RHP$bipo <- NA

# Loop through the rows to calculate the percentage
for (i in 1:nrow(His_RHB_V_RHP)) {
  if (i == 1 || His_RHB_V_RHP$GameID[i] != His_RHB_V_RHP$GameID[i-1]) {
    # Reset the percentage if the 'value' is different from the previous row
    His_RHB_V_RHP$bipo[i] <- (His_RHB_V_RHP$cumulative_outs[i] + His_RHB_V_RHP$cumulative_sacrifice[i]) / His_RHB_V_RHP$cumulative_plays[i]
  } else {
    # Add percentage for the same 'value' group
    His_RHB_V_RHP$bipo[i] <- (His_RHB_V_RHP$cumulative_outs[i] + His_RHB_V_RHP$cumulative_sacrifice[i]) / His_RHB_V_RHP$cumulative_plays[i]
  }
}


# find max cumsum for each group
max_cumulative_sum <- ave(His_RHB_V_RHP$cumulative_plays, His_RHB_V_RHP$GameID, FUN = max)

# Filter the rows where the cumulative sum equals the maximum cumulative sum for each group
His_RvR_filtered <- His_RHB_V_RHP[His_RHB_V_RHP$cumulative_plays == max_cumulative_sum, ]



### Summary of Stats
## Summary stats for each team (excluding NA)
string_summary = His_RvR_filtered %>%
  filter(!is.na(BatterTeam)) %>%
  group_by(BatterTeam) %>%
  reframe(
    count = n(),  # Total count of the string value
    mean_value = count / n(),  # Same as count, this is just the count itself
    proportion = count / nrow(His_RvR_filtered),  # Proportion of the string in the dataset
    sd_occurrences = sd(table(BatterTeam)[BatterTeam]),  # Standard deviation of the occurrences
    .groups = "drop"
  ) %>%
  arrange(desc(count))  # optional: sort by frequency

print(string_summary)

# Summary of stats for DV
summary(His_RvR_filtered)
sd(His_RHB_V_RHP$so_rate)
sd(His_RHB_V_RHP$obp)
sd(His_RHB_V_RHP$bipo)


## Creating variables for ridge regressions 
# Creates Batter team variables 
Batter_Team = factor(His_RvR_filtered$BatterTeam) 
x = model.matrix(~ Batter_Team)[,-1]

# Creates Pitching team variables
Pitcher_Team = factor(His_RvR_filtered$PitcherTeam)
v = model.matrix(~ Pitcher_Team)[,-1]

# Dependent variables 
y = His_RvR_filtered$so_rate
z = His_RvR_filtered$obp
w = His_RvR_filtered$bipo



### Ridge Regressions 
## Batter SO%
# Determines coefficients, standard errors, p-values & significance of ridge regression 
bootstrap_ridge_summary_Bso <- function(x, y, lambda, n_boot = 1000) {
  n <- nrow(x)
  p <- ncol(x)
  
  boot_coefs <- matrix(NA, nrow = n_boot, ncol = p + 1)  # +1 for intercept
  
  for (i in 1:n_boot) {
    sample_idx <- sample(1:n, replace = TRUE)
    x_boot <- x[sample_idx, ]
    y_boot <- y[sample_idx]
    
    boot_fit <- glmnet(x_boot, y_boot, alpha = 0, lambda = lambda, intercept = TRUE)
    boot_coefs[i, ] <- as.numeric(coef(boot_fit))
  }
  
  # Original model
  orig_fit <- glmnet(x, y, alpha = 0, lambda = lambda, intercept = TRUE)
  orig_coefs <- as.numeric(coef(orig_fit))
  
  # Standard errors
  se_vals <- apply(boot_coefs, 2, sd)
  
  # P-values (two-sided test)
  p_vals <- sapply(1:length(orig_coefs), function(j) {
    2 * mean(abs(boot_coefs[, j]) >= abs(orig_coefs[j]))
  })
  
  # Significance stars
  significance <- cut(p_vals,
                      breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf),
                      labels = c("***", "**", "*", ".", " "),
                      right = TRUE)
  
  # Assemble output
  coef_names <- rownames(coef(orig_fit))
  summary_df <- data.frame(
    Term = coef_names,
    Coefficient = round(orig_coefs, 4),
    Std_Error = round(se_vals, 4),
    P_Value = round(p_vals, 4),
    Significance = significance
  )
  
  return(summary_df)
}

# perform k-fold cross-validation to find optimal lambda value 
cv_Bso = cv.glmnet(x, y, alpha = 0)

# find optimal lambda value that minimizes test MSE 
best_lambda_Bso = cv_Bso$lambda.min 
best_lambda_Bso

# Ridge regression results 
ridge_summary_Bso = bootstrap_ridge_summary(x, y, lambda = best_lambda_Bso, n_boot = 1000)
print(ridge_summary_Bso)



## Batter OBP
# Determines coefficients, standard errors, p-values & significance of ridge regression 
bootstrap_ridge_summary_Bobp <- function(x, z, lambda, n_boot = 1000) {
  n <- nrow(x)
  p <- ncol(x)
  
  boot_coefs <- matrix(NA, nrow = n_boot, ncol = p + 1)  # +1 for intercept
  
  for (i in 1:n_boot) {
    sample_idx <- sample(1:n, replace = TRUE)
    x_boot <- x[sample_idx, ]
    z_boot <- z[sample_idx]
    
    boot_fit <- glmnet(x_boot, z_boot, alpha = 0, lambda = lambda, intercept = TRUE)
    boot_coefs[i, ] <- as.numeric(coef(boot_fit))
  }
  
  # Original model
  orig_fit <- glmnet(x, z, alpha = 0, lambda = lambda, intercept = TRUE)
  orig_coefs <- as.numeric(coef(orig_fit))
  
  # Standard errors
  se_vals <- apply(boot_coefs, 2, sd)
  
  # P-values (two-sided test)
  p_vals <- sapply(1:length(orig_coefs), function(j) {
    2 * mean(abs(boot_coefs[, j]) >= abs(orig_coefs[j]))
  })
  
  # Significance stars
  significance <- cut(p_vals,
                      breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf),
                      labels = c("***", "**", "*", ".", " "),
                      right = TRUE)
  
  # Assemble output
  coef_names <- rownames(coef(orig_fit))
  summary_df <- data.frame(
    Term = coef_names,
    Coefficient = round(orig_coefs, 4),
    Std_Error = round(se_vals, 4),
    P_Value = round(p_vals, 4),
    Significance = significance
  )
  
  return(summary_df)
}

# perform k-fold cross-validation to find optimal lambda value 
cv_Bobp = cv.glmnet(x, z, alpha = 0)

# find optimal lambda value that minimizes test MSE 
best_lambda_Bobp = cv_Bobp$lambda.min 
best_lambda_Bobp

# Ridge regression results 
ridge_summary_Bobp = bootstrap_ridge_summary(x, z, lambda = best_lambda_Bobp, n_boot = 1000)
print(ridge_summary_Bobp)



## Batter SO%
# Determines coefficients, standard errors, p-values & significance of ridge regression 
bootstrap_ridge_summary_Bbipo <- function(x, w, lambda, n_boot = 1000) {
  n <- nrow(x)
  p <- ncol(x)
  
  boot_coefs <- matrix(NA, nrow = n_boot, ncol = p + 1)  # +1 for intercept
  
  for (i in 1:n_boot) {
    sample_idx <- sample(1:n, replace = TRUE)
    x_boot <- x[sample_idx, ]
    w_boot <- w[sample_idx]
    
    boot_fit <- glmnet(x_boot, w_boot, alpha = 0, lambda = lambda, intercept = TRUE)
    boot_coefs[i, ] <- as.numeric(coef(boot_fit))
  }
  
  # Original model
  orig_fit <- glmnet(x, w, alpha = 0, lambda = lambda, intercept = TRUE)
  orig_coefs <- as.numeric(coef(orig_fit))
  
  # Standard errors
  se_vals <- apply(boot_coefs, 2, sd)
  
  # P-values (two-sided test)
  p_vals <- sapply(1:length(orig_coefs), function(j) {
    2 * mean(abs(boot_coefs[, j]) >= abs(orig_coefs[j]))
  })
  
  # Significance stars
  significance <- cut(p_vals,
                      breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf),
                      labels = c("***", "**", "*", ".", " "),
                      right = TRUE)
  
  # Assemble output
  coef_names <- rownames(coef(orig_fit))
  summary_df <- data.frame(
    Term = coef_names,
    Coefficient = round(orig_coefs, 4),
    Std_Error = round(se_vals, 4),
    P_Value = round(p_vals, 4),
    Significance = significance
  )
  
  return(summary_df)
}

# perform k-fold cross-validation to find optimal lambda value 
cv_Bbipo = cv.glmnet(x, w, alpha = 0)

# find optimal lambda value that minimizes test MSE 
best_lambda_Bbipo = cv_Bbipo$lambda.min 
best_lambda_Bbipo

# Ridge regression results 
ridge_summary_Bbipo = bootstrap_ridge_summary(x, w, lambda = best_lambda_Bbipo, n_boot = 1000)
print(ridge_summary_Bbipo)




## Pitcher SO%
# Determines coefficients, standard errors, p-values & significance of ridge regression 
bootstrap_ridge_summary_Pso <- function(v, y, lambda, n_boot = 1000) {
  n <- nrow(v)
  p <- ncol(v)
  
  boot_coefs <- matrix(NA, nrow = n_boot, ncol = p + 1)  # +1 for intercept
  
  for (i in 1:n_boot) {
    sample_idx <- sample(1:n, replace = TRUE)
    v_boot <- v[sample_idx, ]
    y_boot <- y[sample_idx]
    
    boot_fit <- glmnet(v_boot, y_boot, alpha = 0, lambda = lambda, intercept = TRUE)
    boot_coefs[i, ] <- as.numeric(coef(boot_fit))
  }
  
  # Original model
  orig_fit <- glmnet(v, y, alpha = 0, lambda = lambda, intercept = TRUE)
  orig_coefs <- as.numeric(coef(orig_fit))
  
  # Standard errors
  se_vals <- apply(boot_coefs, 2, sd)
  
  # P-values (two-sided test)
  p_vals <- sapply(1:length(orig_coefs), function(j) {
    2 * mean(abs(boot_coefs[, j]) >= abs(orig_coefs[j]))
  })
  
  # Significance stars
  significance <- cut(p_vals,
                      breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf),
                      labels = c("***", "**", "*", ".", " "),
                      right = TRUE)
  
  # Assemble output
  coef_names <- rownames(coef(orig_fit))
  summary_df <- data.frame(
    Term = coef_names,
    Coefficient = round(orig_coefs, 4),
    Std_Error = round(se_vals, 4),
    P_Value = round(p_vals, 4),
    Significance = significance
  )
  
  return(summary_df)
}

# perform k-fold cross-validation to find optimal lambda value 
cv_Pso = cv.glmnet(v, y, alpha = 0)

# find optimal lambda value that minimizes test MSE 
best_lambda_Pso = cv_Pso$lambda.min 
best_lambda_Pso

# Ridge regression results 
ridge_summary_Pso = bootstrap_ridge_summary(v, y, lambda = best_lambda_Pso, n_boot = 1000)
print(ridge_summary_Pso)



## Pitcher OBP
# Determines coefficients, standard errors, p-values & significance of ridge regression 
bootstrap_ridge_summary_Pobp <- function(v, z, lambda, n_boot = 1000) {
  n <- nrow(v)
  p <- ncol(v)
  
  boot_coefs <- matrix(NA, nrow = n_boot, ncol = p + 1)  # +1 for intercept
  
  for (i in 1:n_boot) {
    sample_idx <- sample(1:n, replace = TRUE)
    v_boot <- v[sample_idx, ]
    z_boot <- z[sample_idx]
    
    boot_fit <- glmnet(v_boot, z_boot, alpha = 0, lambda = lambda, intercept = TRUE)
    boot_coefs[i, ] <- as.numeric(coef(boot_fit))
  }
  
  # Original model
  orig_fit <- glmnet(v, z, alpha = 0, lambda = lambda, intercept = TRUE)
  orig_coefs <- as.numeric(coef(orig_fit))
  
  # Standard errors
  se_vals <- apply(boot_coefs, 2, sd)
  
  # P-values (two-sided test)
  p_vals <- sapply(1:length(orig_coefs), function(j) {
    2 * mean(abs(boot_coefs[, j]) >= abs(orig_coefs[j]))
  })
  
  # Significance stars
  significance <- cut(p_vals,
                      breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf),
                      labels = c("***", "**", "*", ".", " "),
                      right = TRUE)
  
  # Assemble output
  coef_names <- rownames(coef(orig_fit))
  summary_df <- data.frame(
    Term = coef_names,
    Coefficient = round(orig_coefs, 4),
    Std_Error = round(se_vals, 4),
    P_Value = round(p_vals, 4),
    Significance = significance
  )
  
  return(summary_df)
}

# perform k-fold cross-validation to find optimal lambda value 
cv_Pobp = cv.glmnet(v, z, alpha = 0)

# find optimal lambda value that minimizes test MSE 
best_lambda_Pobp = cv_Pobp$lambda.min 
best_lambda_Pobp

# Ridge regression results 
ridge_summary_Pobp = bootstrap_ridge_summary(v, z, lambda = best_lambda_Pobp, n_boot = 1000)
print(ridge_summary_Pobp)



## Pitcher BIPO
# Determines coefficients, standard errors, p-values & significance of ridge regression 
bootstrap_ridge_summary_Pbipo <- function(v, w, lambda, n_boot = 1000) {
  n <- nrow(v)
  p <- ncol(v)
  
  boot_coefs <- matrix(NA, nrow = n_boot, ncol = p + 1)  # +1 for intercept
  
  for (i in 1:n_boot) {
    sample_idx <- sample(1:n, replace = TRUE)
    v_boot <- v[sample_idx, ]
    w_boot <- w[sample_idx]
    
    boot_fit <- glmnet(v_boot, w_boot, alpha = 0, lambda = lambda, intercept = TRUE)
    boot_coefs[i, ] <- as.numeric(coef(boot_fit))
  }
  
  # Original model
  orig_fit <- glmnet(v, w, alpha = 0, lambda = lambda, intercept = TRUE)
  orig_coefs <- as.numeric(coef(orig_fit))
  
  # Standard errors
  se_vals <- apply(boot_coefs, 2, sd)
  
  # P-values (two-sided test)
  p_vals <- sapply(1:length(orig_coefs), function(j) {
    2 * mean(abs(boot_coefs[, j]) >= abs(orig_coefs[j]))
  })
  
  # Significance stars
  significance <- cut(p_vals,
                      breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf),
                      labels = c("***", "**", "*", ".", " "),
                      right = TRUE)
  
  # Assemble output
  coef_names <- rownames(coef(orig_fit))
  summary_df <- data.frame(
    Term = coef_names,
    Coefficient = round(orig_coefs, 4),
    Std_Error = round(se_vals, 4),
    P_Value = round(p_vals, 4),
    Significance = significance
  )
  
  return(summary_df)
}

# perform k-fold cross-validation to find optimal lambda value 
cv_Pbipo = cv.glmnet(v, w, alpha = 0)

# find optimal lambda value that minimizes test MSE 
best_lambda_Pbipo = cv_Pso$lambda.min 
best_lambda_Pbipo

# Ridge regression results 
ridge_summary_Pbipo = bootstrap_ridge_summary(v, w, lambda = best_lambda_Pbipo, n_boot = 1000)
print(ridge_summary_Pbipo)

