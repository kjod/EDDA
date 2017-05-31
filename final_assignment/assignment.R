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

hist(as.numeric(air$Date))
accidents_per_year = data.frame(table(air$Date))
plot(accidents_per_year) 
lines(lowess(accidents_per_year$Var1,accidents_per_year$Freq), col="blue") # lowess line (x,y)


