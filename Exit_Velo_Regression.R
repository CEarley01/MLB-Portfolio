### Pitching type model effects on Exit Velo 

rm(list=ls())

library(tidyverse)
library(dplyr)
library(fastDummies)

options(scipen = 4)

### Setting Working Directory
setwd("/Volumes/Flashdrive/All_Games")
TD = read.csv("Trackman.csv")



### Cleaning Data



## Removing all unnecessary variables
TD1 = subset(TD,select = -c(PitchofPA,RunsScored,Notes,VertRelAngle,
                           HorzRelAngle,SpinRate,SpinAxis,Tilt,RelHeight,RelSide,
                           Extension,PlateLocHeight,PlateLocSide,ZoneSpeed,VertApprAngle,
                           HorzApprAngle,ZoneTime,Direction,HitSpinRate))
TD1 = subset(TD1,select = -c(PositionAt110X,PositionAt110Y,PositionAt110Z,
                           LastTrackedDistance,Bearing,HangTime,pfxx,pfxz,x0,y0,
                           z0,vx0,vy0,vz0,ax0,ay0,az0,Stadium,Level,EffectiveVelo,
                           MaxHeight,MeasuredDuration,SpeedDrop,PitchLastMeasuredX,
                           PitchLastMeasuredY,PitchLastMeasuredZ,ContactPositionX,
                           ContactPositionY,ContactPositionZ))
TD1 = subset(TD1,select = -c(UTCTime,LocalDateTime,UTCDateTime,AutoHitType,System,
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
TD1 = subset(TD1,select = -c(ThrowSpeed,PopTime,ExchangeTime,TimeToBase,
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
TD1 = subset(TD1,select = -c(Time,PitchUID,HomeTeam,AwayTeam,GameUID,UTCDate,
                           HomeTeamForeignID,AwayTeamForeignID,GameForeignID,PlayID,
                           Top.Bottom,PitcherSet,OutsOnPlay))
TD1 = subset(TD1,select = -c(Date,TaggedHitType,KorBB,PAofInning,Outs,Balls,Strikes, 
                             League, TaggedPitchType, Inning, BatterId, PitcherId, VertBreak))


## Check to make sure values are good 
unique(TD1$AutoPitchType)
unique(TD1$PitchCall)
unique(TD1$PlayResult)
unique(TD1$RelSpeed)
unique(TD1$InducedVertBreak)
unique(TD1$HorzBreak)
unique(TD1$ExitSpeed)
unique(TD1$Angle)
unique(TD1$Distance)


## Filter out unneeded values 

# filters PitchCall to in play
TD1 = TD1 %>% filter(PitchCall == "InPlay")

# Filter PlayResult
TD1 = TD1 %>% filter(PlayResult %in% c("Single", "FieldersChoice", "Out", "Double", "undefined",
                                     "HomeRun", "Error", "Triple", "Sacrifice"))

## Converts all values from strings into integers 

# Specify the columns you want to convert
columns_to_convert <- c("RelSpeed", "InducedVertBreak", "HorzBreak", "ExitSpeed", "Angle", "Distance")

# Convert specified columns to numeric, replacing non-numeric strings with NA
TD1_converted <- TD1 %>%
  mutate(across(all_of(columns_to_convert), ~ suppressWarnings(as.numeric(.))))

# filters PitchCall to in play
TD1_converted = TD1_converted %>% filter(AutoPitchType != "Other")

## removes all NA created 
TD1_converted <- TD1_converted %>%
  filter(!is.na(InducedVertBreak))

TD1_converted <- TD1_converted %>%
  filter(!is.na(HorzBreak))

TD1_converted <- TD1_converted %>%
  filter(!is.na(ExitSpeed))

TD1_converted <- TD1_converted %>%
  filter(!is.na(Angle))

TD1_converted <- TD1_converted %>%
  filter(!is.na(Distance))

TD1_converted <- TD1_converted %>%
  filter(!is.na(RelSpeed))

TD1_converted = dummy_cols(TD1_converted, select_columns = "AutoPitchType")


### Summary of stats

## Summary statistics for each unique string (excluding NA)
string_summary = TD1_converted %>%
  filter(!is.na(AutoPitchType)) %>%
  group_by(AutoPitchType) %>%
  reframe(
    count = n(),  # Total count of the string value
    mean_value = count / n(),  # Same as count, this is just the count itself
    proportion = count / nrow(TD1_converted),  # Proportion of the string in the dataset
    sd_occurrences = sd(table(AutoPitchType)[AutoPitchType]),  # Standard deviation of the occurrences
    .groups = "drop"
  ) %>%
  arrange(desc(count))  # optional: sort by frequency

print(string_summary)
# min for each pitch = 0
# Max for each pitch = 1


# Summary stats for other variables
summary(TD1_converted)

# Standard Deviation of all variables 
sd(TD1_converted$InducedVertBreak)
sd(TD1_converted$HorzBreak)
sd(TD1_converted$ExitSpeed)
sd(TD1_converted$Angle)
sd(TD1_converted$Distance)
sd(TD1_converted$RelSpeed)
sd(TD1_converted$AutoPitchType_Changeup)
sd(TD1_converted$AutoPitchType_Curveball)
sd(TD1_converted$AutoPitchType_Cutter)
sd(TD1_converted$`AutoPitchType_Four-Seam`)
sd(TD1_converted$AutoPitchType_Sinker)
sd(TD1_converted$AutoPitchType_Slider)
sd(TD1_converted$AutoPitchType_Splitter)


### Regressions 

## Reg1
reg1 = lm(ExitSpeed ~ InducedVertBreak + HorzBreak + RelSpeed, data = TD1_converted)
summary(reg1)

## Reg2
reg2 = lm(ExitSpeed ~ Angle + Distance, data = TD1_converted)
summary(reg2)

## Reg3 
reg3 = lm(ExitSpeed ~ InducedVertBreak + HorzBreak + RelSpeed + factor(AutoPitchType), 
          data = TD1_converted)
summary(reg3)

## Reg4
reg4 = lm(ExitSpeed ~ InducedVertBreak + HorzBreak + RelSpeed + factor(AutoPitchType) + 
            Angle + Distance, data = TD1_converted)
summary(reg4)
