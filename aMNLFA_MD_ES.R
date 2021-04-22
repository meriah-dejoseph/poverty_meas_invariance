library(aMNLFA)
library(MplusAutomation)
library(readr)

library(ggplot2)
library(MplusAutomation)
library(reshape2)
library(gridExtra)
library(stringr)
library(plyr)
library(dplyr)
library(devtools)

library(grDevices)
library(graphics)
library(stats)
library(utils)

####################################################
# Set directory to where .csv is. 
#This is where script will output mplus files
####################################################
wd <- ("~/Desktop/ES")
setwd(wd)

df<- read_csv("mr.csv")
df <- as.data.frame(df)
head(df)

#Make character vars factors for plotting. Tried this to fix plotting error (note below), but it still gave me an error
str(df)

df$GENDER <- factor(df$GENDER) 
df$STATE <- factor(df$STATE)
df$RACE <- factor(df$RACE)

#Centering age at mean age at 6mo
summary(df$REALAGE) #mean is .64
df$AGE <- df$REALAGE - .64
summary(df$AGE)

#Also making wave centered in case we decide to go with that as time. Centering on first timepoint 6mo
df$WAVEC <- df$WAVE -6 
summary(df$WAVEC)

#Create interaction term for time 
df$AGE_2 <- df$AGE * df$AGE
summary(df$AGE_2)

####################################################
# 1.	Define aMNLFA object (aMNLFA.object) 
####################################################
# creating aMNLFA object (ob)
ob <-aMNLFA.objectmeriah(dir          = wd, # , indicate the location of the data. 
                         mrdata        = df,
                         indicators    = c("ECONS1",
                                           "ECONS2",
                                           "ECONS3",
                                           "ECONS4",
                                           "ECONS5",
                                           "ECONS6"), # list a set of indicators of a single factor
                         
                         catindicators = c("ECONS1",
                                           "ECONS2",
                                           "ECONS3",
                                           "ECONS4",
                                           "ECONS5",
                                           "ECONS6"), # list a set of indicators that are binary or ordinal
                         time        = c("AGE"),
                         meanimpact    = c("AGE", "AGE_2", "FEMALE", "BLACK", "NC"), # List all variables that should be tested in the mean impact models
                         varimpact     = c("AGE"),
                         measinvar     = c("AGE", "FEMALE", "BLACK", "NC"), # List variables to be included in tests for measurement non-invariance. 
                         factors       = c("GENDER", "STATE", "RACE", "WAVEC"), 
                         auxiliary = "INDEX", # List all variables that should be included to identify each case in long file. 
                         ID            = "SID",
                         thresholds    = TRUE) # indicate whether you would like to test measurement invariance of thresholds for ordinal indicators.

####################################################
# 2.	Plot items over time
####################################################
aMNLFA.itemplots(ob) 

#################################################### 
# 3.	Draw a calibration
#sample create Mplus input files for mean impact, variance impact, and
#################################################### 
aMNLFA.sample(ob)
# Creates calibration.dat. o	Running this code will output a data file with one
# record per ID (myID) chosen at random (“sample.dat”). This calibration sample
# will be used for obtaining parameter estimates to be used in scoring models.

####################################################
# 4.	Create Mplus input files for mean impact, variance impact, 
# and item-by-item measurement non-invariance (aMNLFA.initial)
####################################################
aMNLFA.initialmeriah(ob) # TRY THIS - if you get an error (cannot open file....), then run lines 49-430

##################################
# 4. Run Models 
################################
runModels() # This will run all models in the path you set. This will take some time. 

##################################
# 5. Incorporate all ‘marginally significant’ terms into a simultaneous Mplus input file 
################################
aMNLFA.simultaneous(ob)
#Running this code results in a single Mplus script file (round2calibration.inp)
#	All mean and variance impact terms with p<.10 are included 

##################################
# 6. Trim non-invariant terms 
################################
aMNLFA.final(ob)

##################################
# 7. Use parameter values generated from the last calibration model to fix
# parameter values in the scoring model using the full, longitudinal dataset
################################
aMNLFA.scores(ob) # note: documentation says to run aMNLFA.scoring(ob), but this function doesnt exist
#The resulting Mplus script uses the long (mr.dat) data file and outputs factor
#score estimates for each observation. Run the resulting script manually.

#################################
# 8. Generate score plots.
##################################
aMNLFA.scoreplots(ob)



