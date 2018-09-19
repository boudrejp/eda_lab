#John's exploration
# done 9/19/18
require(car)
require(dplyr)
require(reshape2)
#data import
cancerdata <- read.csv("./project materials/cancer.csv")

#get dimensions, colnames
dim(cancerdata)
colnames(cancerdata)

#### UNIVARIATE ANALYSIS ####
#get the types of each of the columns, and compare against the summary statistics
#make sure that the values make sense
lapply(cancerdata, function(x){
  list(typeof(x), summary(x))
  })

#### NOTES #####
#X appears to be index
#avgAnnCount likely has skewed distribution- look into this
#medianIncome is an integer- check for recurring vals
#popEst2017 -> mean and median are very different, look at distribution
#povertyPercent is probably OK -> base stats look fine
#binnedInc -> this one seems strange, look further into it. seems like categorical
#medianAge -> there are some strange high values here
#medianAgeMale -> seems OK on first glance
#medianAgeFemale -> seems OK on first glance
#Geography -> check if this should be character or factor
#avgHouseholdSize -> OK on first glance
#PercentMarried -> seems OK, although 72.5% seems pretty high...
#PctNoHS18_24 -> check extreme vals
#PctHS18_24 -> check extreme vals
#PctComeCol18_24 -> NA's involved, needs treatment
#PctBachDeg18_24 -> check extreme vals
#PctBachDeg18_24 -> check extreme vals
#PctHS25_Over -> check extreme vals
#PctBachDeg25_Over -> check extreme vals
#PctEmployed16_Over -> NA's involved, needs treamtment
#PctPrivateCoverage -> check distribution
#PctEmpPrivCoverage -> check distribution
#PctPublicCoverage -> check distribution
#PctWhite -> 100 seems unrealistic... unless Montana or something
#PctBlack -> 0 seems unrealistic
#PctAsian -> 0 seems unrealistic
#PctOtherRace -> 0 seems unrealistic
#PctMarriedHouseholds -> seems ok, check dist
#BirthRate -> 0 seems unrealistic
#deathrate -> needs standard basis

#create logical field to make sure races add up to 100
adds.to.100 <- cancerdata$PctWhite + cancerdata$PctBlack + cancerdata$PctAsian + cancerdata$PctOtherRace
sum(adds.to.100) == nrow(cancerdata)
summary(adds.to.100)
hist(adds.to.100)
#seems like we are missing demographic data for some -> exactly how many?
cancerdata$sum.races <- adds.to.100
#we will say that getting to 95 is close enough -> can manipulate this cutoff if we would like
sum(cancerdata$sum.races < 95)
# 421 entries are suspect due to races not coming close enough to 100

#histograms for all of our variables
for(i in 1:ncol(cancerdata)){
  if(is.numeric(cancerdata[[i]])){
    hist(cancerdata[[i]], main = colnames(cancerdata[i]), breaks = 20)
    }
  }

#more detailed graphs for the ones that seem a little more peculiar
dev.off()
boxplot(cancerdata$avgAnnCount, main = "avgAnnCount") #some high looking vals
hist(cancerdata$medIncome, breaks = 50, main = "medIncome") #seems ok
boxplot(cancerdata$popEst2015, main = "popEst2015") #high outliers
boxplot(cancerdata$MedianAge, main = "medianAge") #above about 65 makes absolutely no sense
boxplot(cancerdata$AvgHouseholdSize, main = "AvgHouseholdSize") #has clustering at zero
boxplot(cancerdata$PctMarriedHouseholds, main = "PctMarriedHouseholds") #appears to have high outlier
boxplot(cancerdata$sum.races, main = "Sum of races") #vals that arent event close to 100


#deeper dive into geography
length(unique(levels(cancerdata$Geography))) == nrow(cancerdata)
#so we have a new entry per row... we should probably split this into a more sensical way.
#split by state?
cancerdata$Geography <- as.character(cancerdata$Geography)
split.geo <- strsplit(as.character(cancerdata$Geography), ", ")
states <- sapply(split.geo, 
                 function(x){
                   return(x[2])
                   })
cancerdata$state <- as.factor(states)

#treatment of na's, nonsensical vals, general data processing

cancerdata$popEst2015 <- log(cancerdata$popEst2015) #gives better scaling, more normal data dist
#change binnedInc into 10 different columns -> forget this function. reshape?
#correct the median age... let's assume normal distributiona and sample from the existing sensical values
cancerdata$MedianAge[cancerdata$MedianAge > 85] <- rnorm(mean = mean(cancerdata$MedianAge[cancerdata$MedianAge < 85]), 
                                                         sd = sd(cancerdata$MedianAge[cancerdata$MedianAge < 85]), 
                                                         n = length(cancerdata$MedianAge[cancerdata$MedianAge > 85]))
#over half of the values for PctSomeCol18_24 are missing.
#probably makes sense to either completely get rid of col or possibly just exclude NA's and go with data we have

#probablty ok now to look at multivar stuff




