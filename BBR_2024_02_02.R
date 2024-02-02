#---------------------------------------------------------------------------#
#           BBR Scholars Seminar (2/2/24) on Regression Analysis            #
#                           Christopher Mann                                #
#---------------------------------------------------------------------------#
#                                                                           #
# During the first part of this seminar, we will discuss what a linear      #
# regression is and how to interpret the results. We will go through        #
# an example involving wage discrimination.                                 #
#                                                                           #
# During the second part of the seminar, we will focus on diagnosing        #
# regression problems, performing tests, & binary response models.          #
#---------------------------------------------------------------------------#

## FILE LOCATION:  https://github.com/cmann3/bbr/blob/main/BBR_2024_02_02.R
## BLNAK LOCATION: https://github.com/cmann3/bbr/blob/main/blank_2024_02_02.R

## Libraries
## ---------
## Go to "Tools > Install Packages" in RStudio to install any library listed
## below that you do not have
library(ggplot2)                # For plotting data
library(lmtest)                 # For testing linear regression assumptions
library(sandwich)               # Robust Standard Errors
library(dplyr)                  # Data prep
library(margins)                # Estimate Average Marginal Effect, Logit Model

## Load Data
## ---------
load(url("https://github.com/cmann3/bbr/raw/main/UNL.rda"))


## [1] EXPERIENCE
## --------------
## Plot
ggplot(UNL, aes(Experience, Salary)) + 
  geom_point() + 
  geom_smooth( method = "lm" )

## Regression
reg_exp <- lm(Salary ~ Experience, UNL)
summary(reg_exp)

## Plot Residuals
plot(reg_exp$fitted.values, reg_exp$residuals)
abline(h = 0)

## Test for Heteroskedasticity
##   H0: Errors have constant variance
bptest(reg_exp)   

## Correct for Heteroskedasticity
coeftest(reg_exp, vcovHC(reg_exp, type = "HC0"))


## [2] EDUCATION
## --------------------------
reg_ed <- lm(Salary ~ Experience + Education, UNL)
summary(reg_ed)

## Test for Nonlinearity
##   H0: Linear
resettest(reg_ed)  


## [3] LOG ADJUSTMENT & RANK
## -------------------------
reg_log <- lm(log(Salary) ~ log(Experience) + log(Education) + Rank, UNL)
summary(reg_log)

plot(reg_log$fitted.values, reg_log$residuals)
abline(h = 0)


## [4] GENDER
## ----------
reg_d <- lm(log(Salary) ~ log(Experience) +  Rank + Female, UNL)
summary(reg_d)

plot(reg_d$fitted.values, reg_d$residuals)
abline(h = 0)

## Test for Nonlinearity
resettest(reg_d)  

## Test for heteroskedasticity
bptest(reg_d)
coeftest(reg_d, vcovHC(reg_d, type = "HC0"))

## Influential Points? (outliers that have significant impact on regression)
cooks.distance(reg_d) > 1

## Test Residuals for Normality (H0: Normally Distributed)
shapiro.test(reg_d$residuals) 

## Multicollinearity (Any Correlations > 0.7?)
cor(UNL[, c("Experience", "Rank", "Female")]) 


## [5] JOBS
## --------
reg_job <- lm(log(Salary) ~ log(Experience) +  Rank + Female + SOC_Code, UNL)
summary(reg_job)

## Frequency Table 
tab_job <- table(UNL$SOC_Code, UNL$Female)
prop.table(tab_job, margin = 1) # % by rows


## [6] BINARY
## ----------
## Create New, BINARY VARIABLE
UNL$high_rank <- ifelse(UNL$Rank > 2, 1, 0)

## LPM Model
reg_lpm <- lm(high_rank ~ log(Experience) + log(Education) + Female, UNL)
summary(reg_lpm)

## LOGIT Model
## y = 1/(1 + e^(-bx)) 
reg_logit <- glm(high_rank ~ log(Experience) + log(Education) + Female, 
                family = binomial, UNL)
summary(reg_logit)

## Average Marginal Effect
margins(reg_logit)
