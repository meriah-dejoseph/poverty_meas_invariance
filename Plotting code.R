#Plotting eta scores
#8.15.19

#########################################################
#load libraries
library(readr)
library(ggplot2)
library(dplyr)

#Read in data
AllData <- read_csv("~/Desktop/ICD/First-year project/FINAL SUBMISSION TO CD/ANALYSES/Analysis files/Analyses/MNLFA_MERGED_5.16.20_wOUTCOMES.csv")

##########################################################


#Make moderators factors
AllData$GENDER <- factor(AllData$GENDER) 
AllData$STATE <- factor(AllData$STATE)
AllData$RACE <- factor(AllData$RACE)
AllData$BLACK <- factor(AllData$BLACK)
AllData$NC <- factor(AllData$NC)
AllData$FEMALE <- factor(AllData$FEMALE)
AllData$WAVE <- factor(AllData$WAVE)
AllData$INR1orbelow <- factor(AllData$INR1orbelow)
#str(AllData)

#Then create sub datasets that only have the relevant info and ensure same cases across MNLFA and means
ES <- AllData %>% 
  select(SID, WAVE, ESINDEX, REALAGE, AGE, AGE_2, INR, 
         BLACK, NC, FEMALE, RACE, STATE, GENDER,
         ESETA1, ESETA2, ES_Mn) %>% 
  filter(ES_Mn >= 0)
#head(ES)
#str(ES)

HOME <- AllData %>% 
  select(SID, WAVE, HOMEINDEX, REALAGE, AGE, AGE_2, INR, 
         BLACK, NC, FEMALE, RACE, STATE, GENDER,
         HOMEETA1, HOME_Mn) %>% 
  filter(HOME_Mn >= 0) #filtering to get exact same cases 
#head(HOME)

CTS <- AllData %>% 
  select(SID, WAVE, CTSINDEX, REALAGE, AGE, AGE_2, INR, 
         BLACK, NC, FEMALE, RACE, STATE, GENDER,
         CTSETA1, CTSETA2, CTS_Mn) %>% 
  filter(CTS_Mn >= 0)
#head(CTS)



##########################################################
#For all datasets, round age for better plotting

#ES
ES$roundAGE<-ifelse(ES$AGE <= 0, 0,ES$AGE) #less than or equal to zero, then zero
ES$roundAGE<-ifelse(ES$AGE > 0 & ES$AGE<= 1, 1,ES$roundAGE) #greater than zero and less than or equal to 1, then 1
ES$roundAGE<-ifelse(ES$AGE > 1 & ES$AGE<= 2, 2,ES$roundAGE) #greater than 1 and less than or equal to 2 then 2
ES$roundAGE<-ifelse(ES$AGE > 2 &ES$AGE<= 3, 3,ES$roundAGE)
ES$roundAGE<-ifelse(ES$AGE > 3 &ES$AGE<= 4, 4, ES$roundAGE)
ES$roundAGE<-ifelse(ES$AGE > 4 &ES$AGE<=5, 5,ES$roundAGE)
ES$roundAGE<-ifelse(ES$AGE > 5 &ES$AGE<= 6 ,6, ES$roundAGE)
ES$roundAGE<-ifelse(ES$AGE > 6 &ES$AGE<= 7, 7, ES$roundAGE)
ES$roundAGE<-ifelse(ES$AGE > 7 &ES$AGE<= 8, 8, ES$roundAGE)
ES$roundAGE<-ifelse(ES$AGE > 8 &ES$AGE<= 12, 12, ES$roundAGE)
ES$roundAGE<-ifelse(ES$AGE > 12 &ES$AGE<= 13, 13,ES$roundAGE)
ES$roundAGE<-ifelse(ES$AGE > 13 &ES$AGE<= 14, 14, ES$roundAGE)
ES$roundAGE<-ifelse(ES$AGE > 14, 15,ES$roundAGE)
ES$roundAGE = as.factor(ES$roundAGE)

#HOME
HOME$roundAGE<-ifelse(HOME$AGE <= 0, 0,HOME$AGE) #lHOMEs than or equal to zero, then zero
HOME$roundAGE<-ifelse(HOME$AGE > 0 & HOME$AGE<= 1, 1,HOME$roundAGE) #greater than zero and lHOMEs than or equal to 1, then 1
HOME$roundAGE<-ifelse(HOME$AGE > 1 & HOME$AGE<= 2, 2,HOME$roundAGE) #greater than 1 and lHOMEs than or equal to 2 then 2
HOME$roundAGE<-ifelse(HOME$AGE > 2 &HOME$AGE<= 3, 3,HOME$roundAGE)
HOME$roundAGE<-ifelse(HOME$AGE > 3 &HOME$AGE<= 4, 4, HOME$roundAGE)
HOME$roundAGE<-ifelse(HOME$AGE > 4 &HOME$AGE<=5, 5,HOME$roundAGE)
HOME$roundAGE<-ifelse(HOME$AGE > 5 &HOME$AGE<= 6 ,6, HOME$roundAGE)
HOME$roundAGE<-ifelse(HOME$AGE > 6 &HOME$AGE<= 7, 7, HOME$roundAGE)
HOME$roundAGE<-ifelse(HOME$AGE > 7 &HOME$AGE<= 8, 8, HOME$roundAGE)
HOME$roundAGE<-ifelse(HOME$AGE > 8 &HOME$AGE<= 12, 12, HOME$roundAGE)
HOME$roundAGE<-ifelse(HOME$AGE > 12 &HOME$AGE<= 13, 13,HOME$roundAGE)
HOME$roundAGE<-ifelse(HOME$AGE > 13 &HOME$AGE<= 14, 14, HOME$roundAGE)
HOME$roundAGE<-ifelse(HOME$AGE > 14, 15,HOME$roundAGE)
HOME$roundAGE = as.factor(HOME$roundAGE)

#CTS
CTS$roundAGE<-ifelse(CTS$AGE <= 0, 0,CTS$AGE) #less than or equal to zero, then zero
CTS$roundAGE<-ifelse(CTS$AGE > 0 & CTS$AGE<= 1, 1,CTS$roundAGE) #greater than zero and less than or equal to 1, then 1
CTS$roundAGE<-ifelse(CTS$AGE > 1 & CTS$AGE<= 2, 2,CTS$roundAGE) #greater than 1 and less than or equal to 2 then 2
CTS$roundAGE<-ifelse(CTS$AGE > 2 &CTS$AGE<= 3, 3,CTS$roundAGE)
CTS$roundAGE<-ifelse(CTS$AGE > 3 &CTS$AGE<= 4, 4, CTS$roundAGE)
CTS$roundAGE<-ifelse(CTS$AGE > 4 &CTS$AGE<=5, 5,CTS$roundAGE)
CTS$roundAGE<-ifelse(CTS$AGE > 5 &CTS$AGE<= 6 ,6, CTS$roundAGE)
CTS$roundAGE<-ifelse(CTS$AGE > 6 &CTS$AGE<= 7, 7, CTS$roundAGE)
CTS$roundAGE<-ifelse(CTS$AGE > 7 &CTS$AGE<= 8, 8, CTS$roundAGE)
CTS$roundAGE<-ifelse(CTS$AGE > 8 &CTS$AGE<= 12, 12, CTS$roundAGE)
CTS$roundAGE<-ifelse(CTS$AGE > 12 &CTS$AGE<= 13, 13,CTS$roundAGE)
CTS$roundAGE<-ifelse(CTS$AGE > 13 &CTS$AGE<= 14, 14, CTS$roundAGE)
CTS$roundAGE<-ifelse(CTS$AGE > 14, 15,CTS$roundAGE)
CTS$roundAGE = as.factor(CTS$roundAGE)

##########################################################

#########################################
##Plot distribution of ETA 
#########################################
ESETAhisto<-ggplot(data = ES, aes(x = ESETA1)) + 
  geom_histogram(color = 'black', bins = 50)
ggsave("ESETAhisto.png", width = 4, height = 4)

ESMNhisto <- ggplot(data = ES, aes(x = ES_Mn)) + 
  geom_histogram(color = 'black', bins = 50) 
ggsave("ESMNhisto.png", width = 4, height = 4)

#Do plot for aim 3
EShisto <- ES %>% 
  dplyr::filter(ES_Mn==1) %>% 
  dplyr::select(ESETA1, ES_Mn)

ESfilterhisto <- ggplot(data = EShisto, aes(x = ESETA1)) + 
  geom_histogram(color = 'black', bins = 50) +
  xlab("ES: MNLFA")
ggsave("ESfilterhisto.png", width = 4, height = 4)

#########

HOMEETAhisto <- ggplot(data = HOME, aes(x = HOMEETA1)) + 
  geom_histogram(color = 'black', bins = 50) 
ggsave("HOMEETAhisto.png", width = 4, height = 4)

HOMEMNhisto <- ggplot(data = HOME, aes(x = HOME_Mn)) + 
  geom_histogram(color = 'black', bins = 50) 
ggsave("HOMEMNhisto.png", width = 4, height = 4)

#Do plot for aim 3
HOMEhisto <- HOME %>% 
  dplyr::filter(HOME_Mn==0) %>% 
  dplyr::select(HOMEETA1, HOME_Mn)

HOMEfilterhisto <- ggplot(data = HOMEhisto, aes(x = HOMEETA1)) + 
  geom_histogram(color = 'black', bins = 50) +
  xlab("HOME: MNLFA")
ggsave("HOMEfilterhisto.png", width = 4, height = 4)

#########

CTSETAhisto <- ggplot(data = CTS, aes(x = CTSETA1)) + 
  geom_histogram(color = 'black', bins = 50) 
ggsave("CTSETAhisto.png", width = 4, height = 4)

CTSMNhisto <- ggplot(data = CTS, aes(x = CTS_Mn)) + 
  geom_histogram(color = 'black', bins = 50) 
ggsave("CTSMNhisto.png", width = 4, height = 4)

#Do plot for aim 3
CTShisto <- CTS %>% 
  dplyr::filter(CTS_Mn==0) %>% 
  dplyr::select(CTSETA1, CTS_Mn)

CTSfilterhisto <- ggplot(data = CTShisto, aes(x = CTSETA1)) + 
  geom_histogram(color = 'black', bins = 50) +
  xlab("CTS: MNLFA")
ggsave("CTSfilterhisto.png", width = 4, height = 4)



#########################################
##Plot ETA vs. mean scores 
#########################################

#ES
ESrainbow <- ggplot(data = ES, aes(x = ES_Mn, y = ESETA1, group_by(roundAGE), color = roundAGE)) + 
  geom_jitter() + 
  theme(legend.position = "none")+
  xlab('ES: Raw mean') + 
  ylab('ES: MNLFA')  
ggsave("ESrainbow .png", width = 4, height = 4)

#HOME
HOMErainbow <- ggplot(data = HOME, aes(x = HOME_Mn, y = HOMEETA1, group_by(roundAGE), color = roundAGE)) + 
  geom_jitter() + 
  theme(legend.position = "none")+
  xlab('HOME: Raw mean') + 
  ylab('HOME: MNLFA') 
ggsave("HOMErainbow .png", width = 4, height = 4)

#CTS
CTSrainbow <- ggplot(data = CTS, aes(x = CTS_Mn, y = CTSETA1, group_by(roundAGE), color = roundAGE)) + 
  geom_jitter() + 
  theme(legend.position = "none")+
  xlab('CTS: Raw mean') + 
  ylab('CTS: MNLFA')  
ggsave("CTSrainbow .png", width = 4, height = 4)




#########################################
##Plot as a function of income
#########################################

#Plot showing how stable INR is over time
ggplot(data = AllData, aes(x = AGE, y = INR, color=BLACK)) +
  geom_smooth(method="loess", lwd = 1.5, show.legend = FALSE) +
  #geom_line(aes(group = SID), show.legend = FALSE)+
  geom_point(aes(y=INR), alpha = 0.3, show.legend = FALSE) +
  #geom_line(aes(group = SID), show.legend = FALSE)+
  #stat_summary(aes(data=ES$ESyhat,group=BLACK),fun.y=mean,geom="line",lwd = 1.5) +
  theme_bw() +
  ylab("INR") +
  xlab("AGE (years)")
#So maybe we could show this to highlight how income is pretty stable across time 
#with stable race disparities. 
#The factor scores highlight that there is more change and variation occuring. 


#ES as a function of INR
ggplot(data = AllData, aes(x = AGE, y = ESETA1, color=INR1orbelow)) +
  geom_smooth(method="loess", lwd = 1.5) +
  #geom_line(aes(group = SID), show.legend = FALSE)+
  geom_point(aes(y=ESETA1), alpha = 0.3) +
  #geom_line(aes(group = SID), show.legend = FALSE)+
  #stat_summary(aes(data=ES$ESyhat,group=BLACK),fun.y=mean,geom="line",lwd = 1.5) +
  theme_bw() +
  ylab("ES: MNLFA") +
  xlab("AGE (years)")


#HOME as a function of INR
ggplot(data = AllData, aes(x = AGE, y = HOMEETA1, color=INR1orbelow)) +
  geom_smooth(method="loess", lwd = 1.5) +
  #geom_line(aes(group = SID), show.legend = FALSE)+
  geom_point(aes(y=HOMEETA1), alpha = 0.3) +
  #geom_line(aes(group = SID), show.legend = FALSE)+
  #stat_summary(aes(data=ES$ESyhat,group=BLACK),fun.y=mean,geom="line",lwd = 1.5) +
  theme_bw() +
  ylab("HOME: MNLFA") +
  xlab("AGE (years)")

#CTS as a function of INR
ggplot(data = AllData, aes(x = AGE, y = CTSETA1, color=INR1orbelow)) +
  geom_smooth(method="loess", lwd = 1.5) +
  #geom_line(aes(group = SID), show.legend = FALSE)+
  geom_point(aes(y=CTSETA1), alpha = 0.3) +
  #geom_line(aes(group = SID), show.legend = FALSE)+
  #stat_summary(aes(data=ES$ESyhat,group=BLACK),fun.y=mean,geom="line",lwd = 1.5) +
  theme_bw() +
  ylab("CTS: MNLFA") +
  xlab("AGE (years)")


#########################################
##Density plots highlighting overlap
#########################################

library(ggpubr)
# Basic density plot with mean line and marginal rug
ggdensity(ES, x = "ESETA1", 
          fill = "#0073C2FF", color = "#0073C2FF",
          add = "mean", rug = TRUE)

# Change outline and fill colors by groups INR at or below 1 = 1
# Use a custom palette

ESdens <-ggdensity(AllData, x = "ESETA1", 
          add = "mean", rug = TRUE,
          color = "INR1orbelow", fill = "INR1orbelow", 
          xlab="ES: MNLFA", 
          palette = c("#0073C2FF", "#FC4E07"))

ESdens <- ESdens + theme(legend.position = "none")
ESdens 
ggsave("ESdens.png", width = 4, height = 4)

HOMEdens <- ggdensity(AllData, x = "HOMEETA1",
          add = "mean", rug = TRUE,
          color = "INR1orbelow", fill = "INR1orbelow", 
          xlab="HOME: MNLFA", 
          palette = c("#0073C2FF", "#FC4E07"))

HOMEdens <- HOMEdens + theme(legend.position = "none")
HOMEdens 
ggsave("HOMEdens.png", width = 4, height = 4)

CTSdens <- ggdensity(AllData, x = "CTSETA1",
          add = "mean", rug = TRUE,
          color = "INR1orbelow", fill = "INR1orbelow", 
          xlab="CTS: MNLFA", 
          palette = c("#0073C2FF", "#FC4E07"))

CTSdens <- CTSdens + theme(legend.position = "none")
CTSdens 
ggsave("CTSdens.png", width = 4, height = 4)






#########################################
##Individual raw trajectories for supplement
#########################################

ids <- unique(AllData$SID)   # This creates a vector of identification numbers in the data

subdata <- subset(AllData,
                  SID %in% sample(ids,size=75))  # The second line randomly picks 6 identification numbers from "ids"


#ES ETA graph
ESETArawtraj <- ggplot(data=subdata,aes(x=REALAGE, y=ESETA1, group=factor(SID), color=factor(SID))) + 
  geom_line(aes(group = SID), show.legend = FALSE)+
  #stat_summary(fun.y = mean, geom = "line", size = 2, group = 1, colour="blue") + 
  geom_point(show.legend = FALSE) + 
  theme_bw() +
  ylab("ES: MNLFA") +
  xlab("AGE (years)")
ggsave("ESETArawtraj.png", width = 4, height = 4)

#HOME ETA graph
HOMEETArawtraj <- ggplot(data=subdata,aes(x=REALAGE, y=HOMEETA1, group=factor(SID), color=factor(SID))) + 
  geom_line(aes(group = SID), show.legend = FALSE)+
  #stat_summary(fun.y = mean, geom = "line", size = 2, group = 1, colour="blue") + 
  geom_point(show.legend = FALSE) + 
  theme_bw() +
  ylab("HOME: MNLFA") +
  xlab("AGE (years)")
ggsave("HOMEETArawtraj.png", width = 4, height = 4)

#CTS ETA graph
CTSETArawtraj <- ggplot(data=subdata,aes(x=REALAGE, y=CTSETA1, group=factor(SID), color=factor(SID))) + 
  geom_line(aes(group = SID), show.legend = FALSE)+
  #stat_summary(fun.y = mean, geom = "line", size = 2, group = 1, colour="blue") + 
  geom_point(show.legend = FALSE) + 
  theme_bw() +
  ylab("CTS: MNLFA") +
  xlab("AGE (years)")
ggsave("CTSETArawtraj.png", width = 4, height = 4)



###means

#ES mean graph
ESMNrawtraj <- ggplot(data=subdata,aes(x=REALAGE, y=ES_Mn, group=factor(SID), color=factor(SID))) + 
  geom_line(aes(group = SID), show.legend = FALSE)+
  #stat_summary(fun.y = mean, geom = "line", size = 2, group = 1, colour="blue") + 
  geom_point(show.legend = FALSE) + 
  theme_bw() +
  ylab("ES: Raw Mean") +
  xlab("AGE (years)")
ggsave("ESMNrawtraj.png", width = 4, height = 4)

#HOME mean graph
HOMEMNrawtraj <- ggplot(data=subdata,aes(x=REALAGE, y=HOME_Mn, group=factor(SID), color=factor(SID))) + 
  geom_line(aes(group = SID), show.legend = FALSE)+
  #stat_summary(fun.y = mean, geom = "line", size = 2, group = 1, colour="blue") + 
  geom_point(show.legend = FALSE) + 
  theme_bw() +
  ylab("HOME: Raw Mean") +
  xlab("AGE (years)")
ggsave("HOMEMNrawtraj.png", width = 4, height = 4)


#CTS mean graph
CTSMNrawtraj <- ggplot(data=subdata,aes(x=REALAGE, y=CTS_Mn, group=factor(SID), color=factor(SID))) + 
  geom_line(aes(group = SID), show.legend = FALSE)+
  #stat_summary(fun.y = mean, geom = "line", size = 2, group = 1, colour="blue") + 
  geom_point(show.legend = FALSE) + 
  theme_bw() +
  ylab("CTS: Raw Mean") +
  xlab("AGE (years)")
ggsave("CTSMNrawtraj.png", width = 4, height = 4)


