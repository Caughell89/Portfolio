#setting working directory
setwd('/Users/paulcaughell/Documents/Syracuse - Data Science/Information Visualization - IST 719/Final_Project')

# Loading needed packages
library(ggplot2)
library(ggmap)
library(usmap)
library(dplyr)
library(RColorBrewer)

# Loading the dataset
rates <- read.csv("rates.csv", sep=",", header = TRUE)
census <- read.csv("Census_2015.csv", sep = ",", header = TRUE)

# Viewing structure of the data
str(rates)
str(census)

# Looking at the summary statistics
summary(rates)
summary(census)

# Looking at the first few rows
head(rates, 20)
head(census, 10)

# Selecting only the columns that will be used
rates <- select(rates, StateCode, Age, IndividualRate, IndividualTobaccoRate,
                Couple, PrimarySubscriberAndOneDependent, PrimarySubscriberAndTwoDependents,
                PrimarySubscriberAndThreeOrMoreDependents, CoupleAndOneDependent, 
                CoupleAndTwoDependents, CoupleAndThreeOrMoreDependents)

censusSelect <- select(census, TotalPop, IncomePerCap)

# Aggregating the census data to state level

# Converting vectors to numeric to enable more mathmatical functions
censusSelect$TotalPop <- as.numeric(censusSelect$TotalPop)
censusSelect$IncomePerCap <- as.numeric(censusSelect$IncomePerCap)

# Multipling each counties population by the income per capita to get
# total income at a county level
censusSelect$TotalIncome <- censusSelect$TotalPop * censusSelect$IncomePerCap

# Creating a dataframe that has the totals for each states population
# and total income
stateSelectAgg <- aggregate(censusSelect, by = list(census$State), 
                            FUN = sum)

# Finding the income per captia at the state level
stateSelectAgg$IncomePerCap <- stateSelectAgg$TotalIncome/stateSelectAgg$TotalPop

# Correcting the column name
names(stateSelectAgg)[1] <- "state"

# Invalid data found across rates as some indicate $0 and $9999
# Removing invalid plans

rates$IndividualRate[rates$IndividualRate==0] <- NA
rates$IndividualRate[rates$IndividualRate > 9000] <- NA
rates$Couple[rates$Couple == 0] <- NA
rates$PrimarySubscriberAndOneDependent[rates$PrimarySubscriberAndOneDependent == 0] <- NA
rates$PrimarySubscriberAndTwoDependents[rates$PrimarySubscriberAndTwoDependents == 0] <- NA
rates$PrimarySubscriberAndThreeOrMoreDependents[rates$PrimarySubscriberAndThreeOrMoreDependents == 0] <- NA
rates$CoupleAndOneDependent[rates$CoupleAndOneDependent == 0] <- NA
rates$CoupleAndTwoDependents[rates$CoupleAndTwoDependents == 0] <- NA
rates$CoupleAndThreeOrMoreDependents[rates$CoupleAndThreeOrMoreDependents == 0] <- NA

summary(rates)

############################################
##    Looking at Individual Rate Plans    ##
############################################

# Selecting only the State and Individual Data
indy <- select(rates, StateCode, IndividualRate)
indyRates <- select(rates, IndividualRate)

# Viewing the structure of the subset
str(indy)
str(indyRates)

# Viewing the summary stats
summary(indy)
summary(indyRates)

# Looking at the number of plans that offer individual rates
dim(indy[!complete.cases(indy),])
dim(indy[complete.cases(indy),])

# Removing the NAs from the smoker dataset
indy <- indy[complete.cases(indy),]
indyRates <- indyRates[complete.cases(indyRates),]

######################################
##    Plotting Options per State    ##
######################################

optionsPlot <- ggplot(indy, aes(x = reorder(StateCode, StateCode, function(x)-length(x)), fill=..count..,))
optionsPlot <- optionsPlot + geom_bar() + theme_classic() 
optionsPlot <- optionsPlot + scale_fill_gradient2(midpoint=100000, low="darkred", mid="white",
                                                   high="darkblue") +
  theme(legend.position="none")
optionsPlot

# Finding state averages
stateAvgIndy <- aggregate(indyRates, by = list(indy$StateCode), 
                          FUN = mean)
summary(stateAvgIndy)

# Making bins for ages
# Selecting only the State and Individual Data

ageBins <- select(rates, StateCode, IndividualRate, Age)

# Removing the NAs from the age bin dataset
ageBins <- ageBins[complete.cases(ageBins),]

ageBins$ageGroup <- as.character(ageBins$Age)

ageBins$ageGroup[ageBins$ageGroup == '0-14'] <- '14'
ageBins$ageGroup[ageBins$ageGroup == '64 and over'] <- '64'
ageBins <- ageBins[ageBins$Age != 'Family Option',]
ageBins$ageGroup <- as.integer(ageBins$ageGroup)

ageBins$ageGroup[ageBins$ageGroup <= 17] <- '17 & Under'
ageBins$ageGroup[ageBins$ageGroup >= 18 & ageBins$ageGroup < 25] <- '18 - 25'
ageBins$ageGroup[ageBins$ageGroup >= 25 & ageBins$ageGroup < 30] <- '25 - 29'
ageBins$ageGroup[ageBins$ageGroup >= 30 & ageBins$ageGroup < 35] <- '30 - 34'
ageBins$ageGroup[ageBins$ageGroup >= 35 & ageBins$ageGroup < 40] <- '35 - 39'
ageBins$ageGroup[ageBins$ageGroup >= 40 & ageBins$ageGroup < 45] <- '40 - 44'
ageBins$ageGroup[ageBins$ageGroup >= 45 & ageBins$ageGroup < 50] <- '45 - 49'
ageBins$ageGroup[ageBins$ageGroup >= 50 & ageBins$ageGroup < 55] <- '50 - 54'
ageBins$ageGroup[ageBins$ageGroup >= 55 & ageBins$ageGroup < 60] <- '55 - 59'
ageBins$ageGroup[ageBins$ageGroup >= 60 & ageBins$ageGroup < 64] <- '60 - 64'
ageBins$ageGroup[ageBins$ageGroup >= 64] <- '64 & Over'
ageBins$ageGroup <- as.factor(ageBins$ageGroup)
summary(ageBins)

############################################
##    Making Violin Plot for Age Groups   ##
############################################

violin <- ggplot(ageBins, aes(x = ageGroup, y = IndividualRate, fill = ageGroup)) +
  geom_violin(trim = TRUE)

violin <- violin + scale_fill_manual(values = 
  c("#2C2578", "#565093", "#807CAE", "#AAA7C9", "#D4D3E4", 
    "#FFFFFF", "#F0CCCC", "#E29999", "#D46666", "#C63232", "#B80000")) + 
  theme_classic()

violin

############################
##    Prepping the data   ##
############################

# Fixing the columns
names(stateAvgIndy) <- c('StateCode', 'IndividualRate')
stateAvgIndy

# Ordering based off of the rates
stateAvgIndy <- stateAvgIndy[order(stateAvgIndy$IndividualRate),]

# Building a Dataframe for all states basic info
states <- data.frame(state.abb)
names(states) <- "StateCode"
states$state <- state.name

# Merging the states info with the census aggregated info
statesCensus <- merge(x = states, y = stateSelectAgg, by = 'state', sort = TRUE)

# Merging the individual rates with the state census data
statesCensusRate <- merge(x = statesCensus, y = stateAvgIndy, by = 'StateCode', sort = TRUE)

# Tabeling the number of plans offered by each state
statePlans <- transform(table(rates$StateCode))

# Correcting the column names
names(statePlans) <- c('StateCode', 'PlansOffered')

# Merging the number of plans with the census and rate info
statesCensusRate <- merge(x = statesCensusRate, y = statePlans, by = 'StateCode', sort = TRUE)

# Finding the midpoint of the state individual rates for the scale
midpoint <- (max(statesCensusRate$IndividualRate) + min(statesCensusRate$IndividualRate)) / 2

##==============================================##
##     Plotting Plans Offered by Population     ##
##==============================================##

# Plotting rates vs population
sp1<-ggplot(statesCensusRate, aes(x=TotalPop, y=PlansOffered, size = IndividualRate, color=IndividualRate)) + 
  geom_point() + geom_text(aes(label=state),hjust=-.3, vjust=-.1) 
sp1 <- sp1 + theme_classic() + labs(x='Population', y='Number of Plans Offered')
  
sp1

# Adding color to points and state name text
sp1 <- sp1+scale_color_gradient2(midpoint=midpoint, low="darkblue", mid="grey",
                          high="darkred", space ="Lab" ) +
  scale_y_continuous(labels = scales::comma) +
  theme(axis.text.y=element_text(angle=35)) + scale_x_continuous(labels = scales::comma)
sp1

##========================================================##
##     Plotting Individual Rates by Income per Capita     ##
##========================================================##

# Plotting rates vs population
sp1<-ggplot(statesCensusRate, aes(x=IncomePerCap, y=IndividualRate, color = IndividualRate)) + 
  geom_point() + geom_text(aes(label=state),hjust=-.3, vjust=-.1) 

sp1 <- sp1 +scale_x_continuous(limits = c(20000,40000)) + 
  scale_y_continuous(limits = c(0,800))
sp1 <- sp1 + theme_bw() + labs(x='Income Per Capita', y='Average Individual Rate Premium')
sp1


# Adding color to points and state name text
sp1 <- sp1 + scale_color_gradient2(midpoint = midpoint, low = "darkblue", high = 'darkred', mid = 'grey')

sp1

##==========================================##
##     Making the Individual Rates Map      ##
##==========================================##

# Using the average Individual Rate Plan for scaling
mid <- mean(statesCensusRate$IndividualRate)

# Plotting the map
plot_usmap(data = statesCensusRate, values = "IndividualRate", 
           lines = "white") + 
  scale_fill_gradient2(midpoint = mid, low = "darkblue", mid = "white", high = "darkred", name = "Individual Rate\n Plan Average") + 
  theme(legend.position = "right") 


##============================================================##
##     Looking into the different types of plans offered      ##
##============================================================##

# Looking at the summary statistics
summary(rates)

# Selecting only values with individual rate plans
cleanIndyRates <- rates$IndividualRate[!is.na(rates$IndividualRate)]

# Ordering the vector by increasing individual rates
cleanIndyRates <- cleanIndyRates[order(cleanIndyRates, decreasing = FALSE)]

# Building the dataframe starting with the vector
lineRatesIndy <- data.frame(cleanIndyRates)

# Assigning a plan type column with specific label
lineRatesIndy$Type <- 'Individual'

# Correcting the names of the columns
names(lineRatesIndy) <- c('Rate', 'Type')

# Creaing a plan number counter vector
lineRatesIndy$PlanNumber <- as.numeric(rownames(lineRatesIndy))

# Selecting only values with smoker rate plans
cleanTobaccoRates <- rates$IndividualTobaccoRate[!is.na(rates$IndividualTobaccoRate)]

# Ordering smoker rates in increaing order
cleanTobaccoRates <- cleanTobaccoRates[order(cleanTobaccoRates, decreasing = FALSE)]

# Creating a dataframe starting with the tabacco rates
tempDF <- data.frame(cleanTobaccoRates)

# Creating a column for plan with specified label
tempDF$Type <- 'Smoker'

# Correcting column names
names(tempDF) <- c('Rate', 'Type')

# Creating a counter number for each plan
tempDF$PlanNumber <- as.numeric(rownames(tempDF))

# Combining the rows of individual rates with the smoker rates rows
lineRatesIndy <- rbind(lineRatesIndy, tempDF)

## Following the same process as above for the other plan types ##
cleanCoupleRates <- rates$Couple[!is.na(rates$Couple)]
cleanCoupleRates <- cleanCoupleRates[order(cleanCoupleRates, decreasing = FALSE)]

lineRates <- data.frame(cleanCoupleRates)
lineRates$Type <- 'Couple'
names(lineRates) <- c('Rate', 'Type')

lineRates$PlanNumber <- as.numeric(rownames(lineRates))

cleanP1Rates <- rates$PrimarySubscriberAndOneDependent[!is.na(rates$PrimarySubscriberAndOneDependent)]
cleanP1Rates <- cleanP1Rates[order(cleanP1Rates, decreasing = FALSE)]

tempDF <- data.frame(cleanP1Rates)
tempDF$Type <- 'P1'
names(tempDF) <- c('Rate', 'Type')

tempDF$PlanNumber <- as.numeric(rownames(tempDF))

lineRates <- rbind(lineRates, tempDF)

cleanP2Rates <- rates$PrimarySubscriberAndTwoDependents[!is.na(rates$PrimarySubscriberAndTwoDependents)]
cleanP2Rates <- cleanP2Rates[order(cleanP2Rates, decreasing = FALSE)]

tempDF <- data.frame(cleanP2Rates)
tempDF$Type <- 'P2'
names(tempDF) <- c('Rate', 'Type')

tempDF$PlanNumber <- as.numeric(rownames(tempDF))

lineRates <- rbind(lineRates, tempDF)

cleanP3Rates <- rates$PrimarySubscriberAndThreeOrMoreDependents[!is.na(rates$PrimarySubscriberAndThreeOrMoreDependents)]
cleanP3Rates <- cleanP3Rates[order(cleanP3Rates, decreasing = FALSE)]

tempDF <- data.frame(cleanP3Rates)
tempDF$Type <- 'P3'
names(tempDF) <- c('Rate', 'Type')

tempDF$PlanNumber <- as.numeric(rownames(tempDF))

lineRates <- rbind(lineRates, tempDF)

cleanC1Rates <- rates$CoupleAndOneDependent[!is.na(rates$CoupleAndOneDependent)]
cleanC1Rates <- cleanC1Rates[order(cleanC1Rates, decreasing = FALSE)]

tempDF <- data.frame(cleanC1Rates)
tempDF$Type <- 'C1'
names(tempDF) <- c('Rate', 'Type')

tempDF$PlanNumber <- as.numeric(rownames(tempDF))

lineRates <- rbind(lineRates, tempDF)

cleanC2Rates <- rates$CoupleAndTwoDependents[!is.na(rates$CoupleAndTwoDependents)]
cleanC2Rates <- cleanC2Rates[order(cleanC2Rates, decreasing = FALSE)]

tempDF <- data.frame(cleanC2Rates)
tempDF$Type <- 'C2'
names(tempDF) <- c('Rate', 'Type')

tempDF$PlanNumber <- as.numeric(rownames(tempDF))

lineRates <- rbind(lineRates, tempDF)

cleanC3Rates <- rates$CoupleAndThreeOrMoreDependents[!is.na(rates$CoupleAndThreeOrMoreDependents)]
cleanC3Rates <- cleanC3Rates[order(cleanC3Rates, decreasing = FALSE)]

tempDF <- data.frame(cleanC3Rates)
tempDF$Type <- 'C3'
names(tempDF) <- c('Rate', 'Type')

tempDF$PlanNumber <- as.numeric(rownames(tempDF))

lineRates <- rbind(lineRates, tempDF)

lineRates$Type <- as.factor(lineRates$Type)
lineRatesIndy$Type <- as.factor(lineRatesIndy$Type)

# Combining both dataframes to get counts of each plan type offering
fullLineRates <- rbind(lineRates, lineRatesIndy)

################################################################
##    Plotting bars for number of plans based on plan type    ## 
################################################################

ggplot(lineRatesIndy, aes(x=Type, fill=Type)) + geom_bar()


