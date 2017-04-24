peruvians=read.table("peruvians.txt",header=TRUE)
peruvians <- peruvians[,-c(5,6,7)]

# 1
pairs(migration~age, data=peruvians)
pairs(migration~weight, data=peruvians)
pairs(migration~length, data=peruvians)
pairs(migration~wrist, data=peruvians)
pairs(migration~systolic, data=peruvians)
pairs(migration~diastolic, data=peruvians)

# maybe correlated: age, weight, diastolic

# 2

# We don't have to check the normality of the data, as rank correlation test does not assume normality.
attach(peruvians)
cor.test(migration, age, method="spearman")
# p-value = 0.002189, there is correlation

cor.test(migration, weight, method="spearman")
# p-value = 0.02861, there is correlation

cor.test(migration, length, method="spearman")
# p-value = 0.6087, there is no correlation

cor.test(migration, wrist, method="spearman")
# p-value = 0.1797, there is no correlation

cor.test(migration, systolic, method="spearman")
# p-value = 0.3054, there is no correlation

cor.test(migration, diastolic, method="spearman")
# p-value = 0.6494, there is no correlation
