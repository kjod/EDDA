airline=read.csv("data/airline-safety.csv", header=TRUE)

### Analysis of data
boxplot(airline$incidents_85_99, airline$incidents_00_14)
boxplot(airline$fatal_accidents_85_99, airline$fatal_accidents_00_14)
boxplot(airline$fatalities_85_99, airline$fatalities_00_14)

### Two-Paired Samples (lecture 3)
####??? x-y are random sample of normal distribution
attach(airline)

qqnorm(fatalities_85_99-fatalities_00_14)
hist(fatalities_85_99-fatalities_00_14)
#fatalities seem to come from normal distribution - two-paired test
# Seems to be a significant difference between fatalities in the 20th century and the 21st century,
#??? being 85_99 significantly higher (with a confidence interval between 9 and 105)
t.test(fatalities_85_99, fatalities_00_14, paired=TRUE)
t.test(fatalities_85_99-fatalities_00_14)



### Permutation Tests (lecture 4) - No Normal distribution
permutation_test = function(mystat, col1, col2){
  B=1000
  tstar=numeric(B)
  for (i in 1:B){
    temp=t(apply(cbind(col1,col2),1,sample))
    tstar[i]=mystat(temp[,1],temp[,2])
  }
  myt=mystat(col1,col2)
  print(myt)
  pl=sum(tstar<myt)/B
  pr=sum(tstar>myt)/B
  p=2*min(pl,pr)
  print(p)
}




qqnorm(incidents_85_99-incidents_00_14)
hist(incidents_85_99-incidents_00_14)
mystat=function(x,y) {mean(x-y)}
permutation_test(mystat, airline$incidents_85_99, airline$incidents_00_14)
# there is a significant difference between mean in incidents in period 85-99 and period 00-14



qqnorm(fatal_accidents_85_99-fatal_accidents_00_14)
hist(fatal_accidents_85_99-fatal_accidents_00_14)
permutation_test(mystat, airline$fatal_accidents_85_99, airline$fatal_accidents_00_14)
