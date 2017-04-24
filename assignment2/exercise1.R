telephone=read.table("telephone.txt",header=TRUE)

# exponential distribution of 200 elements
exp_d = rexp(20000, rate=0.035)
exp_d_median = median(exp_d)

wilcox.test(telephone[,"Bills"], mu = exp_d_median)
# Rejected, median is not close to exponential median


par(mfrow=c(1,2))
hist(telephone[,"Bills"])
qqnorm(telephone[,"Bills"])

par(mfrow=c(1,1))
boxplot(telephone[,"Bills"])

# most people pay around 0-20,
# if you wanna be competitive you should start pricing around that value
# 30-80 is an unsuccesfull pricing, it starts to get good again at more than it.

# Conclusion:
# Either offer a cheap service of less than 20???
# Or get a good quality service with a price lower than 80, so you win clients
# that usually pay more