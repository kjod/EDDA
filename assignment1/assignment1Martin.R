# Exercise 1 ######################################################################

load(file="assign1.RData")

# you will have data vectors x1, x2,..., x5 in your R working directory

# The command par(mfrow=c(1,2)) instructs
# R to place the two plots in 1 row of 2 columns. It remains in force until mfrow
# is changed, for instance back to normal with par(mfrow=c(1,1)).
par(mfrow=c(1,2))

# first, some tests of normal distributions with similar sizes as x1...x5
x=rnorm(20)
hist(x)
qqnorm(x)

hist(x1)
qqnorm(x1)
# x1 normal distribution?: yes

hist(x2)
qqnorm(x2)
# x2 normal distribution?: no

hist(x3)
qqnorm(x3)
# x3 normal distribution?: yes

hist(x4)
qqnorm(x4)
# x4 normal distribution?: yes

hist(x5)
qqnorm(x5)
# x5 normal distribution?: no





# Exercise 2 ######################################################################
mu=180; nu=175; m=n=30; sd=10; B=1000
x=rnorm(m,mu,sd)
y=rnorm(n,nu,sd)
t.test(x,y,var.equal=TRUE)
t.test(x,y,var.equal=TRUE)[[3]]

# repeatedly simulating data and computing the fraction of times that the t-test
# rejects the null hypothesis. This script assumes that we reject the null hypothesis
# if the p-value is smaller than 0.05. Thus the test is performed at confidence level 95 %.

B=1000
p=numeric(B)
for (b in 1:B) {
    x=rnorm(m,mu,sd)
    y=rnorm(n,nu,sd)
    p[b]=t.test(x,y,var.equal=TRUE)[[3]]
}
# percentage of samples with p<0.05
power=mean(p<0.05)


    par(mfrow=c(1,2))
    calculatePvalues <- function(m,n,mu,nu,sd,B){
        p=numeric(B)
        for (b in 1:B) {
            x=rnorm(m,mu,sd)
            y=rnorm(n,nu,sd)
            p[b]=t.test(x,y,var.equal=TRUE)[[3]]
        }
        return(p)
    }
    #1. Set mu=nu=180, m=n=30 and sd=10. Repeat the script 1000 times, and
    # record the 1000 p-values. How many p-values are smaller than 5%? How
    # many are smaller than 10%? What is the distribution of the p-values
    # (make a histogram)?
        mu=nu=180; m=n=30; sd=10; B=1000
        p=calculatePvalues(m,n,mu,nu,sd,B)
        # number of pvalues smaller than 5% (49)
        length(p[p<0.05])

        # number of pvalues smaller than 10% (98)
        length(p[p<0.1])

        # distribution of p-values: uniform distribution
        hist(p)
        qqnorm(p)


    #2. Set mu=nu=180, m=n=30 and sd=1. Answer the same questions.
        mu=nu=180; m=n=30; sd=1; B=1000
        p=calculatePvalues(m,n,mu,nu,sd,B)
        # number of pvalues smaller than 5% (51)
        length(p[p<0.05])

        # number of pvalues smaller than 10% (104)
        length(p[p<0.1])

        # distribution of p-values: uniform distribution
        hist(p)
        qqnorm(p)


    #3. Set mu=180, nu=175, m=n=30 and sd=6. Answer the same questions.
        mu=180; nu=175; m=n=30; sd=6; B=1000
        p=calculatePvalues(m,n,mu,nu,sd,B)
        # number of pvalues smaller than 5% (903)
        length(p[p<0.05])

        # number of pvalues smaller than 10% (943)
        length(p[p<0.1])

        # distribution of p-values: chi-square distribution with low degrees of freedom
        hist(p)
        qqnorm(p)


    #4. Explain the findings.
        '
        (H_0: difference in (m,n) means of population=0)

        2.1 and 2.2: we dont reject H_0, as the means are not significantly big

        2.3:         difference in means is big, so H_0 is usually rejected
        '






# Exercise 3 ######################################################################
    

    par(mfrow=c(1,1))
    calculatePvaluesNus <- function(m,n,mu,nus,sd,B){
        powers=numeric(length(nus))
        for (i in 1:length(nus)) {
            nu=nus[i]
            p=calculatePvalues(m,n,mu,nu,sd,B)
            powers[i]=mean(p<0.05)
        }
        return(powers)
    }
    # 1. Set mu=180, m=n=30 and sd=5. Calculate the power of the t-test for every
    # value of nu in the grid seq(175,185,by=0.1). Plot the power as a function
    # of nu.
        mu=180; m=n=30; sd=5; B=1000
        nus=seq(175,185,by=0.1)
        powers=calculatePvaluesNus(m,n,mu,nus,sd,B)
        plot(nus, powers)


    # 2. Set mu=180, m=n=100 and sd=5. Repeat the preceding exercise. Add the
    # plot to the preceding plot.
        mu=180; m=n=100; sd=5; B=1000
        nus=seq(175,185,by=0.1)
        powers=calculatePvaluesNus(m,n,mu,nus,sd,B)
        plot(nus, powers)


    # 3. Set mu=180, m=n=30 and sd=100. Repeat the preceding exercise.
        mu=180; m=n=30; sd=100; B=1000
        nus=seq(175,185,by=0.1)
        powers=calculatePvaluesNus(m,n,mu,nus,sd,B)
        plot(nus, powers)


    #4. Explain the findings
        '
        3.1 and 3.2: when sample is bigger, there is less uncertainty about the population,
                     therefore increasing in our example the amount of rejected hypothesis.
                     It can be seen how with 100 size samples (3.2) H_0 starts not to get rejected in 178 cm,
                     while for 30 size samples (3.1) H_0 starts not to get rejected at 176 cm.

        3.3:         standard deviation is too high, and therefore it is more difficult to make assumptions about the population.
                     This leads to random results, even when nu == 180 cm (only a shuttle difference can be detected)
        '