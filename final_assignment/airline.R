airline=read.csv("airline-safety.csv", header=TRUE)

### Analysis of data
boxplot(airline$incidents_85_99, airline$incidents_00_14)
boxplot(airline$fatal_accidents_85_99, airline$fatal_accidents_00_14)
boxplot(airline$fatalities_85_99, airline$fatalities_00_14)
