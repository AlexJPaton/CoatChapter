#Load libraries
library(ggplot2)
library(dplyr)
library(plyr)
library(caTools)
library(ROCR)
library(tidyverse)
library(caret)
library(leaps)
library(regclass)

#MAX_CORES <- 40
#Load data
CoatLum <- read.csv("CoatLum.csv")

CoatLum$sums <- rowSums(CoatLum[,3:8])
#Bin luminosity and look at this too (four categories?)

count(CoatLum$sums)
sum(CoatLum$sums)

CoatLum$phase <- as.factor(CoatLum$phase)
levels(CoatLum$phase)

#graphing phases 
BBPlot <- ggplot(data=CoatLum, aes(x=phase, y=BB)) +
  geom_bar(stat="identity") + theme_classic() + theme(axis.text.x = element_text(angle = 45)) + scale_x_discrete(limits=c("New","Waning crescent","Waxing crescent", 
                                                                                                                          "First quarter", "Last quarter", 
                                                                                                                          "Waning gibbous", "Waxing gibbous", "Full"))
BOPlot <- ggplot(data=CoatLum, aes(x=phase, y=BO)) +
  geom_bar(stat="identity") + theme_classic() + theme(axis.text.x = element_text(angle = 45)) +scale_x_discrete(limits=c("New","Waning crescent","Waxing crescent", 
                                                                                                                         "First quarter", "Last quarter", 
                                                                                                                         "Waning gibbous", "Waxing gibbous", "Full"))
TBPlot <- ggplot(data=CoatLum, aes(x=phase, y=TB)) +
  geom_bar(stat="identity") + theme_classic() + theme(axis.text.x = element_text(angle = 45)) +scale_x_discrete(limits=c("New","Waning crescent","Waxing crescent", 
                                                                                                                         "First quarter", "Last quarter", 
                                                                                                                         "Waning gibbous", "Waxing gibbous", "Full"))
TOPlot <- ggplot(data=CoatLum, aes(x=phase, y=TO)) +
  geom_bar(stat="identity") + theme_classic() + theme(axis.text.x = element_text(angle = 45)) +scale_x_discrete(limits=c("New","Waning crescent","Waxing crescent", 
                                                                                                                         "First quarter", "Last quarter", 
                                                                                                                         "Waning gibbous", "Waxing gibbous", "Full"))
TSPlot <- ggplot(data=CoatLum, aes(x=phase, y=TS)) +
  geom_bar(stat="identity") + theme_classic() + theme(axis.text.x = element_text(angle = 45)) +scale_x_discrete(limits=c("New","Waning crescent","Waxing crescent", 
                                                                                                                         "First quarter", "Last quarter", 
                                                                                                                         "Waning gibbous", "Waxing gibbous", "Full"))
SBPlot <- ggplot(data=CoatLum, aes(x=phase, y=SB)) +
  geom_bar(stat="identity") + theme_classic() + theme(axis.text.x = element_text(angle = 45)) +scale_x_discrete(limits=c("New","Waning crescent","Waxing crescent", 
                                                                                                                         "First quarter", "Last quarter", 
                                                                                                                         "Waning gibbous", "Waxing gibbous", "Full"))
ggarrange(BBPlot, BOPlot, TBPlot, TOPlot, SBPlot, TSPlot,
          ncol = 3, nrow = 2) + theme(axis.text.x = element_text(angle = 45))


#Plot totals 
TotalPlot <- ggplot(data=CoatLum, aes(x=phase, y=sums)) +
  geom_bar(stat="identity") + theme_classic() + theme(axis.text.x = element_text(angle = 45)) +scale_x_discrete(limits=c("New","Waning crescent","Waxing crescent", 
                                                                                                                         "First quarter", "Last quarter", 
                                                                                                                         "Waning gibbous", "Waxing gibbous", "Full"))
TotalPlot 

#bin values 
CoatLum$bins <- cut(CoatLum$lum, breaks=c(0,0.20,0.40,0.60, 0.80, 1), labels=c("20%","40%","60%", "80%", "100%"))

TotalBins <- ggplot(data=CoatLum, aes(x=bins, y=sums)) +
  geom_bar(stat="identity") + theme_classic() 

TotalBins






###Graphing with lumonosity categrories 

#graphing bins 
BBPlotL <- ggplot(data=CoatLum, aes(x=bins, y=BB)) +
  theme_classic() + geom_bar(stat="identity") 
BOPlotL <- ggplot(data=CoatLum, aes(x=bins, y=BO)) +
  geom_bar(stat="identity") + theme_classic()
TBPlotL <- ggplot(data=CoatLum, aes(x=bins, y=TB)) +
  geom_bar(stat="identity") + theme_classic() 
TOPlotL <- ggplot(data=CoatLum, aes(x=bins, y=TO)) +
  geom_bar(stat="identity") + theme_classic() 
TSPlotL <- ggplot(data=CoatLum, aes(x=bins, y=TS)) +
  geom_bar(stat="identity") + theme_classic()
SBPlotL <- ggplot(data=CoatLum, aes(x=bins, y=SB)) +
  geom_bar(stat="identity") + theme_classic() 


ggarrange(BBPlotL, BOPlotL, TBPlotL, TOPlotL, SBPlotL, TSPlotL,
          ncol = 3, nrow = 2) + theme(axis.text.x = element_text(angle = 45)) #photoshop on colours


#Graphing only occupancy 
#graphing phases 
BBPlotL <- ggplot(data=CoatLum, aes(x=bins, y=BBPA)) +
  theme_classic() + geom_bar(stat="identity") 
BOPlotL <- ggplot(data=CoatLum, aes(x=bins, y=BOPA)) +
  geom_bar(stat="identity") + theme_classic()
TBPlotL <- ggplot(data=CoatLum, aes(x=bins, y=TBPA)) +
  geom_bar(stat="identity") + theme_classic() 
TOPlotL <- ggplot(data=CoatLum, aes(x=bins, y=TOPA)) +
  geom_bar(stat="identity") + theme_classic() 
TSPlotL <- ggplot(data=CoatLum, aes(x=bins, y=TSPA)) +
  geom_bar(stat="identity") + theme_classic()
SBPlotL <- ggplot(data=CoatLum, aes(x=bins, y=SBPA)) +
  geom_bar(stat="identity") + theme_classic() 


ggarrange(BBPlotL, BOPlotL, TBPlotL, TOPlotL, SBPlotL, TSPlotL,
          ncol = 3, nrow = 2) + theme(axis.text.x = element_text(angle = 45)) #photoshop on colours




##$$$$$$$$$$$$$$  $$$$$$$$$###############
#Look at proportions? 
hist(CoatLum$lum)
#total number of sampling days for each lum, divide by captures for proportion 
#make dataframe for each then merge proportions? 

count(CoatLum$bins)
table(CoatLum$bins, CoatLum$sums)

xtabs(sums ~ bins, subset(CoatLum))

CoatSum <-c(3937, 2035, 2004, 2265, 4943) 
CoatTotals <- c(89847, 42679, 39126, 42601, 90759)
names <- c("100%", "80%", "60%", "40%", "20%")

PropDF <- as.data.frame(names)
PropDF$CoatSum <- CoatSum
PropDF$CoatTotals <- CoatTotals
PropDF$Prop <-(PropDF$CoatSum/PropDF$CoatTotals)*100

qplot(PropDF$names, PropDF$Prop)

TotalBins <- ggplot(data=PropDF, aes(x=reorder(names, -Prop), y=Prop))+ geom_bar(stat = "identity")+theme_classic() 
TotalBins

#Tables? Counts? Make new sum column with all cats, look across moon phases
sum(CoatLum$sums)
table(CoatLum$sums, CoatLum$bins)

table(CoatLum$BB, CoatLum$bins)


#Model this 
#Turn to occurance, present or absent
CoatLum$BBPA <- ifelse(CoatLum$BB > 0, 1, 0)
CoatLum$BOPA <- ifelse(CoatLum$BO > 0, 1, 0)
CoatLum$TBPA <- ifelse(CoatLum$TB > 0, 1, 0)
CoatLum$TOPA <- ifelse(CoatLum$TO > 0, 1, 0)
CoatLum$TSPA <- ifelse(CoatLum$TS > 0, 1, 0)
CoatLum$SBPA <- ifelse(CoatLum$SB > 0, 1, 0)
CoatLum$sumsPA <- ifelse(CoatLum$sums > 0, 1, 0)




#           PLOTTING FOR PROPORTIONS FOR EACH COAT              #
#---------------------------------------------------------------#

count(CoatLum$bins)
table(CoatLum$bins, CoatLum$BBPA)
BB1 <- c(320, 132, 165, 159, 357)
table(CoatLum$bins, CoatLum$BOPA)
BO1 <- c(28, 17, 15, 16, 45)
table(CoatLum$bins, CoatLum$TBPA)
TB1 <- c(644, 355, 345, 408, 803)
table(CoatLum$bins, CoatLum$TOPA)
TO1 <- c(65, 27, 24, 31, 88)
table(CoatLum$bins, CoatLum$TSPA)
TS1 <- c(39, 25, 22, 32, 77)
table(CoatLum$bins, CoatLum$SBPA)
SB1 <- c(795, 400, 428, 456, 1012)
CoatTotals <- c(67931, 32293, 29319, 31711, 67773)
names <- c("100%", "80%", "60%", "40%", "20%")

PropDFCoats <- as.data.frame(names)
PropDFCoats$BB <- BB1
PropDFCoats$BO <- BO1
PropDFCoats$TB <- TB1
PropDFCoats$TO <- TO1
PropDFCoats$TS <- TS1
PropDFCoats$SB <- SB1
PropDFCoats$CoatTotals <- CoatTotals


PropDFCoats$PropBB <-(PropDFCoats$BB/PropDF$CoatTotals)*100
PropDFCoats$PropBO <-(PropDFCoats$BO/PropDF$CoatTotals)*100
PropDFCoats$PropTO <-(PropDFCoats$TO/PropDF$CoatTotals)*100
PropDFCoats$PropTB <-(PropDFCoats$TB/PropDF$CoatTotals)*100
PropDFCoats$PropTS <-(PropDFCoats$TS/PropDF$CoatTotals)*100
PropDFCoats$PropSB <-(PropDFCoats$SB/PropDF$CoatTotals)*100

BBPP <- ggplot(data=PropDFCoats, aes(x=names, y=PropBB))+ geom_bar(stat = "identity")+theme_classic()+
  scale_x_discrete(limits=c("20%", "40%", "60%", "80%", "100%"))
BOPP <- ggplot(data=PropDFCoats, aes(x=names, y=PropBO))+ geom_bar(stat = "identity")+theme_classic()+
  scale_x_discrete(limits=c("20%", "40%", "60%", "80%", "100%"))
TBPP <- ggplot(data=PropDFCoats, aes(x=names, y=PropTB))+ geom_bar(stat = "identity")+theme_classic()+
  scale_x_discrete(limits=c("20%", "40%", "60%", "80%", "100%"))
TOPP <- ggplot(data=PropDFCoats, aes(x=names, y=PropTO))+ geom_bar(stat = "identity")+theme_classic()+
  scale_x_discrete(limits=c("20%", "40%", "60%", "80%", "100%"))
TSPP <- ggplot(data=PropDFCoats, aes(x=names, y=PropTS))+ geom_bar(stat = "identity")+theme_classic()+
  scale_x_discrete(limits=c("20%", "40%", "60%", "80%", "100%"))
SBPP <- ggplot(data=PropDFCoats, aes(x=names, y=PropSB))+ geom_bar(stat = "identity")+theme_classic()+ 
  scale_x_discrete(limits=c("20%", "40%", "60%", "80%", "100%"))


ggarrange(BBPP, BOPP, TBPP, TOPP, TSPP, SBPP, 
          ncol = 3, nrow = 2) + theme(axis.text.x = element_text(angle = 45)) #photoshop on colours





#factorise and relevel
CoatLum$BBPA <- as.factor(CoatLum$BBPA)
CoatLum$BOPA <- as.factor(CoatLum$BOPA)
CoatLum$TBPA <- as.factor(CoatLum$TBPA)
CoatLum$TOPA <- as.factor(CoatLum$TOPA)
CoatLum$TSPA <- as.factor(CoatLum$TSPA)
CoatLum$SBPA <- as.factor(CoatLum$SBPA)

CoatLum$BBPA <- relevel(CoatLum$BBPA, "0")
CoatLum$BOPA <- relevel(CoatLum$BOPA, "0")
CoatLum$TBPA <- relevel(CoatLum$TBPA, "0")
CoatLum$TOPA <- relevel(CoatLum$TOPA, "0")
CoatLum$TSPA <- relevel(CoatLum$TSPA, "0")
CoatLum$SBPA <- relevel(CoatLum$SBPA, "0")



#----------------------------Export CoatLum Subset------------------------------#

CoatLumSubset <- CoatLum[, c("date", "cam", "bins", "sumsPA", "BBPA", "BOPA", "TBPA", "TOPA", "SBPA", "TSPA")]
count(CoatLumSubset$bins)
CoatLumSubMoon <- CoatLumSubset[(CoatLumSubset$bins == "100%" | CoatLumSubset$bins == "20%"),]
count(CoatLumSubMoon$bins)

table(CoatLumSubMoon$bins, CoatLumSubMoon$sumsPA)
#write.csv(CoatLumSubMoon, "CoatLumSubMoon.csv")

#-------------------------------------------------------------------------------#



#Export new file 
write.csv(CoatLum, "CoatLumNew.csv")



#           LUMINOSITY FOR EACH COAT TYPE TESTED                #
#---------------------------------------------------------------#

#Sums model 

logistic_model <- glm(sumsPA ~ lum, 
                      data = CoatLum, 
                      family = "binomial")
summary(logistic_model)
exp(coef(logistic_model))
exp(confint(logistic_model)) # for all cats 


#Each type 

#Brown Blotched 
logistic_modelBB <- glm(BBPA ~ lum, 
                        data = CoatLum, 
                        family = "binomial")
#plot(logistic_model)
summary(logistic_modelBB)
exp(coef(logistic_modelBB))
exp(confint(logistic_modelBB))


#Tabby Brown
logistic_modelTB <- glm(TBPA ~ lum, 
                        data = CoatLum, 
                        family = "binomial")
#plot(logistic_model)
summary(logistic_modelTB)
exp(coef(logistic_modelTB))
exp(confint(logistic_modelTB))



#Blotched Orange
logistic_modelBO <- glm(BOPA ~ lum, 
                        data = CoatLum, 
                        family = "binomial")
#plot(logistic_model)
summary(logistic_modelBO)
exp(coef(logistic_modelBO))
exp(confint(logistic_modelBO))


#Tabby Orange
logistic_modelTO <- glm(TOPA ~ lum, 
                        data = CoatLum, 
                        family = "binomial")
#plot(logistic_model)
summary(logistic_modelTO)
exp(coef(logistic_modelTO))
exp(confint(logistic_modelTO))



#Tortoiseshell 
logistic_modelTS <- glm(TSPA ~ lum, 
                        data = CoatLum, 
                        family = "binomial")
#plot(logistic_model)
summary(logistic_modelTS)
exp(coef(logistic_modelTS))
exp(confint(logistic_modelTS))



#Solid Black 
logistic_modelSB <- glm(SBPA ~ lum, 
                        data = CoatLum, 
                        family = "binomial")
#plot(logistic_model)
summary(logistic_modelSB)
exp(coef(logistic_modelSB))
exp(confint(logistic_modelSB))



#PLOT ODDS RATIOS FOR EACH MODEL?#
OR <- c(0.908, 0.803, 0.717, 0.788, 0.470, 0.765)
CI2 <- c(0.803, 0.728, 0.476, 0.603, 0.351, 0.71)
CI9 <- c(1.027, 0.873, 1.076, 1.028, 0.628, 0.824)

ORPlot <- as.data.frame(OR)
ORPlot$CI2 <- CI2
ORPlot$CI9 <- CI9
ORPlot$names <- c("Blotched brown", "Tabby brown", "Blotched orange", "Tabby orange", "Tortoiseshell", "Solid black")
colnames(ORPlot) <- c("Odds.ratio", "CI2.5", "CI97.5", "Coat.type")

ggplot(ORPlot, aes(x=Coat.type, y=Odds.ratio)) + 
  geom_pointrange(aes(ymin=CI2.5, ymax=CI97.5)) + theme_classic() + 
  labs(x = "Coat type", y="Odds ratio")




#---------------------------------------Extremes only----------------------------------------------#
#modelling phases, new or full. 

CoatLumExtreme <- CoatLum[CoatLum$phase == "Full" | CoatLum$phase == "New",]
plyr::count(CoatLumExtreme)



#All cats 
logistic_modelAll <- glm(sumsPA ~ phase, 
                         data = CoatLumExtreme, 
                         family = "binomial")
#plot(logistic_model)
summary(logistic_modelAll)
exp(coef(logistic_modelAll))
exp(confint(logistic_modelAll))




#Blotched brown#

logistic_modelBB <- glm(BBPA ~ phase, 
                        data = CoatLumExtreme, 
                        family = "binomial")
#plot(logistic_model)
summary(logistic_modelBB)
exp(coef(logistic_modelBB))
exp(confint(logistic_modelBB))



#Blotched orange#

logistic_modelBO <- glm(BOPA ~ phase, 
                        data = CoatLumExtreme, 
                        family = "binomial")
#plot(logistic_model)
summary(logistic_modelBO)
exp(coef(logistic_modelBO))
exp(confint(logistic_modelBO))

#Tabby brown#

logistic_modelTB <- glm(TBPA ~ phase, 
                        data = CoatLumExtreme, 
                        family = "binomial")
#plot(logistic_model)
summary(logistic_modelTB)
exp(coef(logistic_modelTB))
exp(confint(logistic_modelTB)) #More likely on new moons 1.1232658909 1.428364647

#Tabby orange# 

logistic_modelTO <- glm(TOPA ~ phase, 
                        data = CoatLumExtreme, 
                        family = "binomial")
#plot(logistic_model)
summary(logistic_modelTO)
exp(coef(logistic_modelTO))
exp(confint(logistic_modelTO))

#Tortoiseshell# 

logistic_modelTS <- glm(TSPA ~ phase, 
                        data = CoatLumExtreme, 
                        family = "binomial")
#plot(logistic_model)
summary(logistic_modelTS)
exp(coef(logistic_modelTS))
exp(confint(logistic_modelTS))  #1.38553705771 3.25894679990


#Combine orange 
CoatLumExtreme$TOPA <- as.numeric(CoatLumExtreme$TOPA)
CoatLumExtreme$BOPA <- as.numeric(CoatLumExtreme$BOPA)

CoatLumExtreme$OrangePA <- ifelse(CoatLumExtreme$TOPA > 1 | CoatLumExtreme$BOPA > 1 , 1, 0)
plyr::count(CoatLumExtreme$OrangePA)

#Orange#
logistic_modelO <- glm(OrangePA ~ phase, 
                       data = CoatLumExtreme, 
                       family = "binomial")
#plot(logistic_model)
summary(logistic_modelO)
exp(coef(logistic_modelO))
exp(confint(logistic_modelO)) 


#Solid black#
logistic_modelSB <- glm(SBPA ~ phase, 
                        data = CoatLumExtreme, 
                        family = "binomial")
#plot(logistic_model)
summary(logistic_modelSB)
exp(coef(logistic_modelSB))
exp(confint(logistic_modelSB))  #1.1947477456 1.4800633241


plyr::count(CoatLumExtreme$TSPA)




#----------------------- 10/11/2023-----------------------#
#--------------New multinomial model----------------------#

CoatLumTemp <- read.csv("NewCoatLumNA.csv")
CoatLumTempBackup <- CoatLumTemp
#pairs(CoatLumTemp[, c('Solar', 'Rainfall', 'MaxTemp', 'MinTemp', 'sums')], 
      #main = "Scatterplot Matrix of Predictors")
#Min and max temp are correlated, so is solar and max temp, so
#remove max temp from model. 


#Keep only high or low luminosity

CoatLumTemp <- CoatLumTemp[CoatLumTemp$lum > 0.80 | CoatLumTemp$lum < .20,]
plyr::count(CoatLumTemp$phase)
hist(CoatLumTemp$lum)

CoatLumTemp$moon <- ifelse(CoatLumTemp$lum>.80, "Full", "New")
plyr::count(CoatLumTemp$moon)

#----------------------------Data frame modifying---------------#
#Can't use current dataframe with multinomial regression
#Two versions of dataframe to be created. One row per observation
#Add dummy variable for NoCats
#Dataframe per coat, then bind together 


#------------Dataframe making--------------#
BB <- CoatLumTemp[CoatLumTemp$BB > 0,]
BB$Coat <- "BB"

TB <- CoatLumTemp[CoatLumTemp$TB > 0,]
TB$Coat <- "TB"

TS <- CoatLumTemp[CoatLumTemp$TS > 0,]
TS$Coat <- "TS"

O <- CoatLumTemp[CoatLumTemp$TO > 0 | CoatLumTemp$BO > 0,]
O$Coat <- "O"

SB <- CoatLumTemp[CoatLumTemp$SB > 0,]
SB$Coat <- "SB"

NoCat <- CoatLumTemp[CoatLumTemp$sums < 1,]
NoCat$Coat <- "NoCat"

LumMultiDF <- rbind(BB, O, TB, TS, SB, NoCat)
LumMultiDF$moon <- as.factor(LumMultiDF$moon)

LumMultiDF$Coat <- as.factor(LumMultiDF$Coat)
LumMultiDF$Coat <- relevel(LumMultiDF$Coat, ref = "NoCat")
library(nnet)

# Fit multinomial logistic regression model
MultiModel <- multinom(Coat ~ moon + Rainfall + Solar + MinTemp + (Solar*moon), data = LumMultiDF)

#
library(lme4)

#Interaction model
LumMultiDF1 <- LumMultiDF
LumMultiDF1$Rainfall <- scale(LumMultiDF1$Rainfall)
LumMultiDF1$Solar <- scale(LumMultiDF1$Solar)
LumMultiDF1$MinTemp <- scale(LumMultiDF1$MinTemp)


MultiModel1 <-  glmer(Coat ~ moon + Rainfall + Solar + MinTemp + (Solar*moon) + (1|cam), data = LumMultiDF, family = binomial)


summary(MultiModel1)
# Obtain confidence intervals for coefficients
conf_intervals <- exp(confint(MultiModel1))
confintDF <- as.data.frame(conf_intervals)

#Can't be arsed figuring out how to do this in R.
#write.csv(confintDF, "Confint.csv")
# Extract coefficients
coeffDF <- as.data.frame(exp(coef(MultiModel)))
#write.csv(coeffDF, "coef.csv")

EditCofint <- read.csv("ConfintNoIntercept.csv")

str(EditCofint)
EditCofint$Coat <- as.factor(EditCofint$Coat)
EditCofint$Predictor <- as.factor(EditCofint$Predictor)

#Plot for this 

y_limits <- c(0.5, max(EditCofint$Upper))

library(ggplot2)
# Create a ggplot for each Coat type
plots <- ggplot(EditCofint, aes(x = reorder(Predictor, OddRatio), y = OddRatio, ymin = Lower, ymax = Upper, fill = Predictor)) +
  geom_errorbar(position = position_dodge(width = 0.7), width = 0.25) +
  geom_point(position = position_dodge(width = 0.7), size = 1, shape = 21, fill = "black") +
  facet_wrap(~Coat, scales = "free_y") +
  geom_hline(yintercept = 1, linetype = "dotted", color = "red") +  # Add a red dotted line through 1
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(x = "Temporal predictors", y = "Odds ratio")
plots

#OddsRatioMultinom
