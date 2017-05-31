install.packages("readxl")
library(readxl)
air = read_excel("data/Final Airplane Crash Data.xlsx")

library(stringr)
temp=str_split_fixed(air$Location, ", ", 2)
countries=temp[,2]
air$Location=countries
air = air[!air$Location=="",]
air = air[air$Classification=="Non Military",]

#aplit=str_split_fixed(air$Date_Final, "-", 2)
temp = as.Date(air$Date,'%m/%d/%Y')
air$Date = format(temp,'%Y')

fatalities = air$Fatalities[!is.na(air$Fatalities)]
