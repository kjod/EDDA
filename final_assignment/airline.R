airline=read.csv("data/airline-safety.csv", header=TRUE)

### Analysis of data
boxplot(airline$incidents_85_99, airline$incidents_00_14)
boxplot(airline$fatal_accidents_85_99, airline$fatal_accidents_00_14)
boxplot(airline$fatalities_85_99, airline$fatalities_00_14)

### Two-Paired Samples (lecture 3)
####??? x-y are random sample of normal distribution
attach(airline)
qqnorm(incidents_85_99-incidents_00_14)
hist(incidents_85_99-incidents_00_14)
#no normal dist

qqnorm(fatal_accidents_85_99-fatal_accidents_00_14)
hist(fatal_accidents_85_99-fatal_accidents_00_14)
#no normal dist

qqnorm(fatalities_85_99-fatalities_00_14)
hist(fatalities_85_99-fatalities_00_14)
#fatalities seem to come from normal distribution

# Seems to be a significant difference between fatalities in the 20th century and the 21st century,
#??? being 85_99 significantly higher (with a confidence interval between 9 and 105)
t.test(fatalities_85_99, fatalities_00_14, paired=TRUE)
t.test(fatalities_85_99-fatalities_00_14)
