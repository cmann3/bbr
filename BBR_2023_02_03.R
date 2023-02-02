#---------------------------------------------------------------------------#
#           BBR Scholars Seminar (23/2/3) on Regression Analysis            #
#                           Christopher Mann                                #
#---------------------------------------------------------------------------#
#                                                                           #
# During the first part of this seminar, we will discuss what a linear      #
# regression is and how to interpret the results. We will go through        #
# multiple examples.                                                        #
#                                                                           #
# During the second part of the seminar, we will focus on diagnosing        #
# regression problems and performing tests.                                 #
#---------------------------------------------------------------------------#

## FILE LOCATION: https://github.com/cmann3/bbr/blob/main/BBR_2023_02_03.R

## Libraries
## ---------
## Go to "Tools > Install Packages" in RStudio to install any library listed
## below that you do not have
library(ggplot2)                # For plotting data
library(cowplot)                # For combining multiple plots on a single pane
library(lmtest)                 # For testing linear regression assumptions
library(sandwich)               # Robust Standard Errors
library(dplyr)                  # Data prep


## [1] How Reliable is Punxsutawney Phil?
## --------------------------------------
load(url("https://github.com/cmann3/bbr/raw/main/groundhog.rda"))
summary(groundhog)

## Plot Shadow versus Temperature
ggplot(groundhog, aes(Shadow, Temp_Mar)) +
  geom_point()

ggplot(groundhog, aes(as.factor(Shadow), Temp_Mar)) +
  geom_boxplot()

## Linear Regression
reg_ghog <- lm(Temp_Mar ~ Shadow, data = groundhog)
summary(reg_ghog)


## [2] Demand for Corn
## -------------------
load(url("https://github.com/cmann3/bbr/raw/main/Corn.rda"))
summary(Corn)

## Plot Quantity vs. Price
ggplot(Corn, aes(Q, P)) +
  geom_point() +
  geom_smooth(method="lm")

## Linear Regression
reg_corn  <- lm(Q ~ P, data = Corn)
summary(reg_corn)

reg_corn2 <- lm(Q ~ P + Income, data = Corn)
summary(reg_corn2)




## [3] Horror Films - Box Office
## -----------------------------
load(url("https://github.com/cmann3/bbr/raw/main/horror.rda"))
summary(horror)

## Plot Revenue vs Budget
ggplot(horror, aes(budget, revenue)) +
  geom_point() +
  geom_smooth(method="lm")

reg_horr  <- lm(revenue ~ budget, data = horror)
summary(reg_horr)

reg_horr2 <- lm(revenue ~ budget + vote_average + runtime, data = horror)
summary(reg_horr2)

horror %>%
  select(-id, -title, -release_date, -collection) %>%
  lm(revenue ~ ., data = .) ->
  reg_horr3
summary(reg_horr3)

## [4] Diagnostics
## ---------------
source(url("https://github.com/cmann3/bbr/raw/main/diagnose.R"))

# OLS Assumptions:
# 1) Model is linear in coefficients & error
# 2) All independent variables uncorrelated with the error
# 3) Observations of the error are uncorrelated with each other
# 4) Error has constant variance (homoskedasticity)
# 5) Error term is normally distributed

# Revisiting CORN
diagnose(reg_corn2)

reg_corn3 <- lm(Q ~ P + I(P^2) + Income, data = Corn)
diagnose(reg_corn3)

reg_corn4 <- lm(log(Q) ~ log(P) + log(Income), data = Corn)
diagnose(reg_corn4)

## White Standard Errors to correct Serial Correlation or Heteroskedasticity
diagnose(reg_corn4, se="robust")
# or
coeftest(reg_corn4, vcov=vcovHC(reg_corn4, type="HC0"))


# Revisiting HORROR
diagnose(reg_horr2)

reg_horr4 <- lm(log(revenue) ~ log(budget) + vote_average + runtime, data = horror)
diagnose(reg_horr4)

diagnose(reg_horr3)

horror %>%
  select(-id, -title, -release_date, -collection) %>%
  mutate(revenue = log(revenue), budget = log(budget)) %>%
  lm(revenue ~ ., data = .) ->
  reg_horr5
diagnose(reg_horr5)
