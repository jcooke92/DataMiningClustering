library("e1071")


# Fit linear model

fit <- lm(HAbalone$rings ~  HAbalone$sex + HAbalone$length + HAbalone$height + HAbalone$wholeWeight + HAbalone$shuckedWeight + HAbalone$visceraWeight + HAbalone$shellWeight, data=HAbalone)

print(summary(fit)$r.squared)
print(mean(fit$residuals^2))
plot(HAbalone$rings, fit$residuals, ylab="Residuals", xlab="Rings", main="HAbalone Linear Model")

fit2 <- svm(HAbalone$rings ~  HAbalone$sex + HAbalone$length + HAbalone$height + HAbalone$wholeWeight + HAbalone$shuckedWeight + HAbalone$visceraWeight + HAbalone$shellWeight, data=HAbalone)

print(mean(fit2$residuals^2))
plot(HAbalone$rings, fit2$residuals, ylab="Residuals", xlab="Rings", main="HAbalone SVM")

