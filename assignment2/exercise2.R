light1 = scan("light1879.txt")
light2 = scan("light1882.txt")
light3 = scan("light.txt")

# first, we should have all 3 datasets with the same criteria.
# We will use 1879/82 criteria (km/s - 299000).
# Therefore we have to transform light3 into km/s and substract 299000:
# We know light3 measure is microseconds to perform 7442 km,
# with 24.8 milliseconds substracted.

# NOTE: I think the assignment clearly means milliseconds when talking
# about Newcomb measurements. After multiplying by 1000 you do get microsecs.

light3 = (light3/1000)+24.8 # milliseconds
light3 = (light3/1000) # seconds to perform 7442 km
light3 = (7442/light3) # in 1 second it will perform x km
light3 = light3 - 299000 # final 1879/82 criteria









# 1. Make histograms and box plots of the data sets. What do you observe?
par(mfrow=c(1,3))
hist(light1)
hist(light2)
hist(light3)

par(mfrow=c(1,1))
boxplot(light1, light2, light3)
# light 2 and light 3 are really similar, while light1 diverges from them




# 2. Determine confidence intervals for the speed of light in km/sec
# for all three data sets (use population means and medians).
t.test(light1)$conf.int
t.test(light2)$conf.int
t.test(light3)$conf.int

# 3. Comment on the intervals found.
# Again, 2 and 3 seem to coincide, while 1 differs, having a confidence interval
# with higher values, and being completely outside of the two other ones.

# 4. shows that light1 measurement was clearly off, while light2 and light3
# were closer to the actual solution.
most_precise_speed_of_light = 299792.458 - 299000



