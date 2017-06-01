#install.packages("readxl")
#library(readxl)
#air = read_excel("data/Final Airplane Crash Data.xlsx")
air=read.csv("data/airline_data.csv", header=TRUE)


#delete Summary column
air=air[ , !(names(air)=='Summary')]
#transform Location column into countries
library(stringr)
temp=str_split_fixed(air$Location, ", ", 2)
countries=temp[,2]
air$Location=countries
#delete Empty Locations
air = air[!air$Location=="",]
#delete Military data
air = air[air$Classification=="Non Military",]
#transform Date to Years
temp = as.Date(air$Date,'%m/%d/%Y')
air$Date = format(temp,'%Y')

fatalities = air$Fatalities[!is.na(air$Fatalities)]

hist(as.numeric(air$Date))
accidents_per_year = data.frame(table(air$Date))
plot(accidents_per_year)
lines(lowess(accidents_per_year$Var1,accidents_per_year$Freq), col="blue") # lowess line (x,y)
