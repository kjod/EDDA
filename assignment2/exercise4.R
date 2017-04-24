clouds=read.table("clouds.txt",header=TRUE)

clouds_diff = clouds[,1]-clouds[,2]
hist(clouds_diff)
qqnorm(clouds_diff)
# Data is paired as it arises from the same individual (cloud) at different points in time.

# Two-paired test
t.test(clouds[,1],clouds[,2],paired=TRUE)
# According to a paired t-test, silver nitrate does have an effect,
# giving a confidence interval of difference of (8, 547)
# As it is a confidence interval with positive values, silver nitrate would result in more rain.

# Mann-Whitney test
wilcox.test(clouds[,1],clouds[,2])
# p-value = 0.01383
# As the p-value is low, we can reject the hypothesis that seeded and unseeded clouds come from the same population.
# This means that there is certainly a different between clouds with silver nitrate and without it.
# Underlying distribution of seeded clouds is shifted to the right from that of unseeded.
# (i.e. seeded clouds have bigger values than unseeded)

# Kolmogorov-Smirnov test
ks.test(clouds[,1],clouds[,2])
# Again, p-value is low and therefore the samples come from different populations, being the mean of seeded clouds higher than those unseeded.






# 2.
sqrt_clouds_1 = sqrt(clouds[,1])
sqrt_clouds_2 = sqrt(clouds[,2])
t.test(sqrt_clouds_1,sqrt_clouds_2,paired=TRUE)
# confidence interval is now smaller, but hypothesis is still rejected, with an even smaller p-value.
wilcox.test(sqrt_clouds_1,sqrt_clouds_2)
# p-value and W is the same as in the previous Mann-Whitney test
ks.test(sqrt_clouds_1,sqrt_clouds_2)
# We also get exactly the same value as in the previous Kolgomorov-Smirnov test.

# 3.
sqrt_clouds_1 = sqrt(sqrt_clouds_1)
sqrt_clouds_2 = sqrt(sqrt_clouds_2)
t.test(sqrt_clouds_1,sqrt_clouds_2,paired=TRUE)
wilcox.test(sqrt_clouds_1,sqrt_clouds_2)
ks.test(sqrt_clouds_1,sqrt_clouds_2)
# Mann-Whitney and Kolgomorov still give the same exact result, while Two-Paired test gives an even smaller confidence interval, although the p-value has also decreased significantly.