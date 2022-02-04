#---------------------------------------------------------------------------#
#           BBR Scholars Seminar (22/2/4) on Regression Analysis 1          #
#                           Christopher Mann                                #
#---------------------------------------------------------------------------#
#                                                                           #
# During this seminar, we will discuss what a linear regression is, how to  #
# interpret one, and test its assumptions. As an example, we will estimate  #
# the Phillips Curve, which relates the unemployment rate to  inflation.    #
#---------------------------------------------------------------------------#

## Libraries
## ---------
## Go to "Tools > Install Packages" in RStudio to install any library listed
## below that you do not have
library(eFRED)                        # For downloading Macroeconomic Data
library(dplyr)                        # For wrangling data
library(ggplot2)                      # For plotting
library(lubridate)                    # For working with dates
library(lmtest)                       # For tests
library(sandwich)                     # For correcting variance


## Downloading & Wrangling Data
## ----------------------------
## use the 'fred' function from the 'eFRED' package to download unemployment
## rate (rename as 'u') and consumer price index (rename as 'cpi').
pc <- fred(u = "UNRATE", cpi = "CPIAUCSL")
pc

## or run the following code to download data set directly:
## load(url("https://github.com/cmann3/data/blob/main/pc.rda?raw=true"))

## Calculate annualized Inflation as the growth rate of CPI.
## 'lag' is from 'dplyr' package
pc <- mutate(pc, pi = (cpi - lag(cpi, 12))/lag(cpi,12)*100)
pc

## The first year of data "NA"; use 'year' from 'lubridate' to extract
## the year from each date, then 'filter' from 'dplyr' to subset the
## data.frame based on the year.
pc$year <- year(pc$date)
pc      <- filter(pc, year > 1947)


## Plotting Unemployment & Inflation
## ---------------------------------
## 'qplot' from 'ggplot2' is a popular plot function in R.

## Time-series Line Graph each variable
qplot(date, pi, data=pc, geom="line")               # inflation
qplot(date,  u, data=pc, geom="line")               # unemployment

## Scatterplot with 'u' for the x-axis and 'pi' for the y-axis,
## then add labels and a 'lm' (linear model) regression line.
qplot(u, pi, data=pc) +
  xlab("Unemployment Rate") +
  ylab("Price Inflation") +
  stat_smooth(method="lm", col="red")

## Perform Linear Regression
## -------------------------
## 'lm' (linear model) is the standard function for performing an OLS
## regression in R. The user specifies where the data is located (data=pc),
## and gives a formula: y ~ x1 + x2 + ...
reg <- lm(pi ~ u, data = pc)
reg

## Predicted pi at mean unemployment rate is same as mean pi
rcoef <- reg$coefficients
all.equal( sum(rcoef*c(1, mean(pc$u))), mean(pc$pi) )

## Use 'summary' to get a detailed look at the regression results
summary(reg)


## By Time/Decade
## --------------
## Add year and year^2 to regression
reg2 <- lm(pi ~ u + year + I(year^2), data=pc)
summary(reg2)

## Add predicted values and residuals from regression to pc
pc$fitted2 <- reg2$fitted.values
pc$resids2 <- reg2$residuals

qplot(year, resids2, data=pc) +
  stat_smooth(method="lm", se=FALSE, size=2)


## Calculate decade of each obs by rounding each year to the -1 digit.
## Convert each decade to a "factor" instead of a number
pc$decade <- round(pc$year, -1)
pc$decade <- as.factor(pc$decade)

## Same scatterplot as earlier, except color-code by decade
qplot(u, pi, color=decade, data=pc) +
  xlab("unemployment Rate") +
  ylab("Price Inflation") +
  stat_smooth(method="lm", se=FALSE, size=2)

## Perform regression with decade added
reg3 <- lm(pi ~ u + decade, data = pc)
summary(reg3)

## View the model.matrix to see how decade is converted to "dummies"
m <- model.matrix(reg3)
m

## Add regression prediction (fitted) and residuals to 'pc' for plotting
pc$fitted3 <- reg3$fitted.values
pc$resids3 <- reg3$residuals

## Plot unemployment & inflation, with predicted values as lines
qplot(u, pi, data=pc) +
  xlab("unemployment Rate") +
  ylab("Price Inflation") +
  geom_line(aes(u, fitted3, color=decade), lwd=1.5)


## Testing Regression Assumptions
## ------------------------------
## OLS regression has 5 assumptions:
##
## 1) y is 'linear' in X
## 2) residuals are not 'serial correlated'
## 3) residuals are normally distributed
## 4) residuals have constant variance
## 5) residuals are exogenous                       <- Too Complicated Here
##
## Common Tests can be found in the "lmtest" package

## Plot Residuals vs. Predicted Values
## Look for patterns, funnels, etc.
qplot(fitted3, resids3, color=decade, data=pc) +
  xlab("Predicted Values") +
  ylab("Residuals") +
  geom_hline(yintercept=0, color="red")

## H0: y is linear in X
## check if p-value < 0.05
## If rejected, consider 'log' transformation or adding squared terms
raintest(reg3)

## H0: errors are not serial correlated
dwtest(reg3)

## H0: errors have constant variance
bptest(reg3)

## H0: errors are normally distributed
shapiro.test(reg3$residuals)

## If any of the three previous tests are rejected, standard errors and
## p-values in regression are incorrect.
## Correct with White "Robust" Standard Errors
coeftest(reg3, vcov=vcovHC(reg3))


## Interaction Terms
## -----------------
## Let two terms in a regression "interact" using '*' instead of '+' in lm
## This allows the slope (d pi/d u) to change each decade
reg4 <- lm(pi ~ u * decade, data = pc)
summary(reg4)

## Plot new regression lines
pc$fitted4 <- reg4$fitted.values
pc$resids4 <- reg4$residuals
qplot(u, pi, data=pc) +
  xlab("unemployment Rate") +
  ylab("Price Inflation") +
  geom_line(aes(u, fitted4, color=decade), lwd=1.5)





