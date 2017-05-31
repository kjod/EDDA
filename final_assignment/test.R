air=read.csv("data/Airplane_Crashes_and_Fatalities_Since_1908.csv", header=TRUE)

library(stringr)
aplit=str_split_fixed(air$Location, ", ", 2)
countries=aplit[,2]
countries[countries==""]


### For permutation tests?
airline=read.csv("data/airline-safety.csv", header=TRUE)
attach(airline)
temp8599=rep("85_99", length(incidents_85_99))
temp0014=rep("00_14", length(incidents_00_14))
permutation_test_incidents = data.frame("incidents"=c(incidents_85_99, incidents_00_14),
                                        "period"=c(temp8599, temp0014))
permutation_test_fatal_accidents = data.frame("incidents"=c(fatal_accidents_85_99, fatal_accidents_00_14),
                                              "period"=c(temp8599, temp0014))