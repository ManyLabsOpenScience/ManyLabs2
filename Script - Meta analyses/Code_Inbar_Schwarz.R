########################################################
##### MANY LABS 2: META-ANALYSES INBAR AND SCHWARZ #####
########################################################

rm(list=ls())

################
### PACKAGES ###
################

install.packages(c("stringr", "metafor"))
library(stringr)
library(metafor)

setwd("C:/Dropbox/Werk/Onderzoek/ML2") # Set working directory to location where data is stored

##### Inbar #####
dat_Inbar <- read.csv(file = "Inbar.1a.study.primary.include.csv") # Load data

### Extract Cohen's q
q <- as.numeric(str_extract_all(as.character(dat_Inbar$test.ConsoleOutput), "(?<=0\\) \n\\s{0,1})[-0-9.]+"))

### Compute sampling variance Cohen's q
vi <- 1/(dat_Inbar$stat.n1-3)+1/(dat_Inbar$stat.n2-3)

### Recode source.Setting variable (Merge "In a lab" and "In a classroom" and discard 
# "Other (please indicate)")
levels(dat_Inbar$source.Setting) <- list(online = c("Online (at home)"), lab = c("In a lab", "In a classroom"))

### RE meta-analysis without moderators
rma(yi = q, vi = vi)

### RE meta-analysis with source.Weird as moderator
rma(yi = q, vi = vi, mods = ~ dat_Inbar$source.Weird)

### RE meta-analysis with recoded source.Setting as moderator
rma(yi = q, vi = vi, mods = ~ dat_Inbar$source.Setting)

################################################################################

##### Schwarz #####
dat_Schwarz <- read.csv(file = "Schwarz.1a.study.primary.include.csv") # Load data

### Extract Cohen's q
q <- as.numeric(str_extract_all(as.character(dat_Schwarz$test.ConsoleOutput), "(?<=0\\) \n\\s{0,1})[-0-9.]+"))

### Compute sampling variance Cohen's q
vi <- 1/(dat_Schwarz$stat.n1-3)+1/(dat_Schwarz$stat.n2-3)

### Recode source.Setting variable (Merge "In a lab" and "In a classroom" and discard 
# "Other (please indicate)")
levels(dat_Schwarz$source.Setting) <- list(online = c("Online (at home)"), lab = c("In a lab", "In a classroom"))

### RE meta-analysis without moderators
rma(yi = q, vi = vi)

### RE meta-analysis with source.Weird as moderator
rma(yi = q, vi = vi, mods = ~ dat_Schwarz$source.Weird)

### RE meta-analysis with recoded source.Setting as moderator
rma(yi = q, vi = vi, mods = ~ dat_Schwarz$source.Setting)