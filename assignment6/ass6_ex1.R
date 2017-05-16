###1.
fruitflies=read.table('fruitflies.txt', header=TRUE)
fruitflies$loglongevity <- log(fruitflies$longevity)

###2. Make an informative plot of the data.
###
###   Longevity seems to increase with torax. We don't know it's relation with activity
plot(loglongevity~thorax, pch=as.character(activity), col=as.numeric(activity), data=fruitflies)
abline(lm(loglongevity~thorax, data=fruitflies), col="blue")
# per activity:
boxplot(fruitflies$loglongevity~fruitflies$activity)
# thorax per activity (is there a significant difference between them?)
boxplot(fruitflies$thorax~fruitflies$activity)

###3. Investigate whether sexual activity influences longevity by performing a
###   statistical test, without taking the thorax length into account
###
###   Sexual activity significantly influences longevity
flieslm = lm(loglongevity~activity, data=fruitflies)
anova(flieslm)

### 4. Does sexual activity increase or decrease longevity? What are the estimated
###    longevities for the three conditions? To answer these questions, use
###    the analysis as under 3), without taking thorax length into account.
###
###    confidence intervals for high activity:   [3.4796296, 3.7246190]
###    confidence intervals for isolated - high: [0.3439909, 0.6904582]
###    confidence intervals for low - high:      [0.2244780, 0.5709453]
###    Therefore, high has the lowest longevity, and isolated the highest.
confint(flieslm)

### 5. Investigate whether sexual activity influences longevity by performing a
###    statistical test, now including thorax length as an explanatory variable
###    into the analysis.
###
### It shows that both factors have an influence.
flieslm2 = lm(loglongevity~thorax+activity, data=fruitflies)
anova(flieslm2)
# it can also be checked using drop1, to see the p-values of each explanatory variable in a simple linear model. We get really similar results on this.
drop1(flieslm2,test="F")

### 6. Does sexual activity increase or decrease longevity? What are the estimated
###    longevities for the three conditions, for a fly with average thorax
###    length? And what are they for a typical fly as small as the smallest in the
###    data set? To answer these questions, use the analysis as under 5), which
###    includes thorax length.
###
###    Similar result as before for sexual activity. Islated > Low > High
###    final model: 1.21893 + 2.97899*thorax + 0.40998 (isolated)
###                                          + 0.28570 (low)
###                                          - 0.69568 (high)
confint(flieslm2)
summary(flieslm2)
mean(fruitflies$thorax)
###    loglongevity_isolated= 1.21893 + 2.97899*0.8245333 + 0.40998 = 4.085186
###    loglongevity_low     = 1.21893 + 2.97899*0.8245333 + 0.28570 = 3.960906
###    loglongevity_high    = 1.21893 + 2.97899*0.8245333 - 0.69568 = 2.979526
min(fruitflies$thorax)
###    loglongevity_isolated= 1.21893 + 2.97899*0.64 + 0.40998 = 3.535464
###    loglongevity_low     = 1.21893 + 2.97899*0.64 + 0.28570 = 3.411184
###    loglongevity_high    = 1.21893 + 2.97899*0.64 - 0.69568 = 2.429804

###7. How does thorax length influence longevity? Graphically investigate whether
###   this dependence is similar under all three conditions of sexual activity.
###
### lines have similar slope, which denotes that thorax influences similarly between activities.
plot(loglongevity~thorax, pch=unclass(activity), col=as.numeric(activity), data=fruitflies)
for (i in 1:3) abline(lm(loglongevity~thorax, data=fruitflies[as.numeric(fruitflies$activity)==i,]), col=i)
legend("bottomright", legend=levels(fruitflies$activity), col=c(1,2,3), lty=1, cex=0.5)

###8. Which of the two analyses, without or with thorax length, do you prefer?
###   Is one of the analyses wrong? (To answer the last question, carefully
###   (re)read the description of the design of the experiment.)
# We prefer the analysi with thorax, as it is more complete. Also, it is possible to detect if there could be difference in longevity because of flies with significantly different thorax between groups.
# the analysis are fine, both factors do influence longevity

###9. Verify normality and heteroscedasticity by making a normal QQ-plot of
###   the residuals, and a residuals versus fitted plot, for the analysis that includes thorax length.
qqnorm(residuals(flieslm2))
plot(fitted(flieslm2),residuals(flieslm2))


###10. Perform the ancova analysis with the number of days as the outcome,
###    rather than its logarithm. Verify normality and heteroscedasticity of the
###    residuals of this analysis. Was it wise to use the logarithm as outcome?
flieslm3 = lm(longevity~thorax+activity, data=fruitflies)
qqnorm(residuals(flieslm3))
plot(fitted(flieslm3),residuals(flieslm3))
