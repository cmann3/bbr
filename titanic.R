#---------------------------------------------------------------------------#
#           BBR Scholars Seminar (22/2/23) on Regression Analysis 2         #
#                           Christopher Mann                                #
#---------------------------------------------------------------------------#
#                                                                           #
# During this seminar, we will explore a binary-response regression, or a   #
# regression where the dependent variable takes only two values. We will    #
# use this technique to predict whether or not someone survived the sinking #
# of the titanic.                                                           #
#---------------------------------------------------------------------------#

## Load Data
## ---------
load(url("https://github.com/cmann3/data/blob/main/titanic.rda?raw=true"))

# --------------------------------------------------------
# Data Description
# --------------------------------------------------------
# survived        - 0 (did not survive) or 1 (did survive)
# sex             - factor: male or female
# age             - number between 0.1667 & 80
# passengerClass  - 1st class, 2nd, or 3rd
# --------------------------------------------------------

# Plot each variable -> histogram for numeric, barplot for factors
hist( titanic$survived )
hist( titanic$age )
plot( titanic$sex )
plot( titanic$passengerClass )

# What does the average of "survived" mean?
mean( titanic$survived )

## Linear Regressions
## ------------------
## Estimate y = b*X

lpm1 <- lm(survived ~ 1, data = titanic)  # Only intercept, No X's
summary(lpm1)

lpm2 <- lm(survived ~ sex, data = titanic)
summary(lpm2)

lpm3 <- lm(survived ~ sex + age + passengerClass, data=titanic)
summary(lpm3)

plot(lpm3$fitted.values, lpm3$residuals)
abline(v=0:1, h = 0, col="red")

## Logit Regression
## ----------------
## Estimate y = e^(b*X) / [1 + e^(b*X)]
## This keeps the prediction for 'y' between 0 & 1
## coefficients represent the change in log odds-ratio = ln(p/(1-p))

lgt1 <- glm(survived ~ sex,
            data = titanic, family= binomial)
summary(lgt1)

lgt2 <- glm(survived ~ sex + age + passengerClass,
            data = titanic, family= binomial(link="logit"))
summary(lgt2)

plot(lgt2$fitted.values, lgt2$residuals)
abline(v=0:1, h = 0, col="red")

## Predict Probabilities of Survival
## ---------------------------------
to_predict <- data.frame(
  sex = as.factor(c(rep("female", 3), rep("male", 3))),
  age = rep(mean(titanic$age), 6),
  passengerClass = as.factor(rep(c("1st", "2nd", "3rd"), 2))
)
predict(lgt2, to_predict, type="response")
