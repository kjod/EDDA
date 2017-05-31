air=read.csv("data/Airplane_Crashes_and_Fatalities_Since_1908.csv", header=TRUE)

library(stringr)
aplit=str_split_fixed(air$Location, ", ", 2)
countries=aplit[,2]
countries[countries==""]