psi=read.table('psi.txt', header=TRUE)

###1.

###2. Fit a logistic regression model with both explanatory variables.
psi$psi=factor(psi$psi)
psi$passed=factor(psi$passed)
psilm = glm(passed~psi+gpa, data=psi, family=binomial)

drop1(psilm,test="Chisq")
summary(psilm)

###4.Estimate the probability that a student with a gpa equal to 3 who receives
###  psi passes the assignment. Estimate the same probability for a student who does not receive psi.
1/(1+exp(-(-11.602 + 2.338*1 + 3.063*3)))

1/(1+exp(-(-11.602 + 2.338*0 + 3.063*3)))

###5.Estimate the relative change in odds of passing the assignment rendered
###  by instructing students with psi rather than the standard method (for an arbitrary student)
###  What is the interpretation of this number? Is it dependent on gpa?
###
###   It means that having studied using psi method increases the chance of passing the assingment by 10.36049%.
###   The value is independent from gpa.
exp(2.338)


###6.
### 15 is the number of student who did not receive psi and didn't showed improvement.
### 6 is the number of student who received psi and didn't show impovement.
### The hypothesis tested by Fisher's exact test is if there is independence between the 2 factors (having psi and improving). This is rejected (p-value < 0.05) which proves that there is a dependence between them, and concluding that psi is more helpful to improve than the previous teaching method.
x=matrix(c(3,15,8,6),2,2)
fisher.test(x)

###7.
### It does not take into account the amount of improvement found per student, but besides that it is a correct experiment.

###8.
### 1st:
### advantage:
### disadvantage:
###
### 2nd:
### advantage: 
### disadvantage: It does not take into account the amount of improvement found per student