#---------------------------------------------------------------------------#
#           BBR Scholars Seminar (23/2/3) on Regression Analysis            #
#---------------------------------------------------------------------------#

## Libraries
## ---------
library(ggplot2)                # For plotting data
library(cowplot)                # For combining multiple plots on a single pane
library(lmtest)                 # For testing linear regression assumptions
library(sandwich)               # Robust Standard Errors
library(dplyr)                  # Data prep

## Data
## ----
load(url("https://github.com/cmann3/bbr/raw/main/groundhog.rda"))
load(url("https://github.com/cmann3/bbr/raw/main/horror.rda"))
load(url("https://github.com/cmann3/bbr/raw/main/Corn.rda"))

## Diagnose Function
## -----------------
source(url("https://github.com/cmann3/bbr/raw/main/diagnose.R"))

## Analysis
## --------










