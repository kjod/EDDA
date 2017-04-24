klm = scan("klm.txt")
# First, we check distribution of the data.
# Assuming maximum delivery duration of this parts is 70,
# we remove outliers (parts that are more than 70 days)
klm <- klm[klm<71]
par(mfrow=c(1,2))
hist(klm)
qqnorm(klm)

# we can assume the data to be symmetric with a certain median.
# Therefore, we can use Wilcoxon test.
# now we test wether the median is 32 or less.
# First we will test if it's equal to 32
wilcox.test(klm,mu=32)
# p-value = 0.04627, therefore the hypothesis is rejected
# Testing lower values gives smaller p-values.
# e.g. with mu=31, p-value = 0.02354
#           mu=30, p-value = 0.004791
# Therefore, we can reject the hypothesis that the median is 32 or lower.




# 2.
klm = scan("klm.txt")
binom.test(sum(klm>70),sum(!!klm),p=0.1)
# confidence interval is between 13.38% and 36.04%
# Therefore, KLM criterion is not met