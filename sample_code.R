###########################################################################
# R Workshop
# University of Pennsylvania
# 11 June 2017
# https://github.com/korykantenga/Stata_R_Workshop
###########################################################################
# Use CRTL+L to clear the Console

##################################################
# Housekeeping
##################################################
  
# Clear Workspace
rm(list=ls())

# Set Directory (change to your folder)
setwd("~/Dropbox/Programming/Stata_Workshop/Stata_R_Workshop")

############################
# Install and Load Packages
###########################

# Package to describe data
install.packages("Hmisc")
library(Hmisc)

#########################
# Setup
#########################
# Note: Copy Data from the website and store in folder. You will also need to 
# copy the codebook to understand what each variable is!
  
# Import Data and store as R Dataframe
myData <- read.csv("mainData.csv")

############################
  # Initial Variable Summary
############################

# Report on Memory (only on Windows Machines)
#memory.size()
#memory.limit()

# Summarize all variables to look for anomalies
summary(myData)

# Note: sex, race, first name, last name are missing so browse to see they are not numerical
# View(myData)

# Note: Usually data cleaning takes place at this stage. 
# Some common questions to answer:
# 1. What do you do with missings? Leave them? Delete the observation? Impute them?
# 2. If you impute, how do you impute? Replace with mean for similar people?

#####################################################
  # Data Analysis
#####################################################
  
# Tabulate the number of responses where positive, negative, or no info was given
table(myData$info_nil)
table(myData$info_pos,myData$info_neg)

# Tabulate the number of responses by sound of name
table(myData$pos_resp,myData$race)

# Since we are working with {0,1} variables, the mean is the % of positive responses
mean(myData$pos_resp[myData$race=="B"])
mean(myData$pos_resp[myData$race=="W"])

# Store the means in "local" variable
# Note: you will see the list of things that are stored in the "Environment Window"
meanB <- mean(myData$pos_resp[myData$race=="B"])
meanW <- mean(myData$pos_resp[myData$race=="W"])

# Calculate the difference
print(meanB-meanW)

### The average response rate for black sounding names is 8% lower.

### Let's restrict to observations where no information was given
meanB <- mean(myData$pos_resp[myData$race=="B" & myData$info_nil==1])
meanW <- mean(myData$pos_resp[myData$race=="W" & myData$info_nil==1])

print(meanB-meanW)

### With no information given, the average response rates for black sounding names
# is 9% lower.


#####################################################
# Regression Analysis
#####################################################
### We can confirm this using a regression!
# Note: The regression with a {0,1} gives the average difference in the outcome 
# between the 0 group and the 1 group

# Create a sub datafrarme with variable and data we need
myData.noinfo <- subset(myData, info_nil==1)

# Run regression
summary(lm(pos_resp ~ black, data = myData.noinfo)) 

### What is the average effect of positive and negative information?
summary(lm(pos_resp ~ info_pos + info_neg, data = myData)) 

### Positive Info increases the response rate by 4% on average and negative info
### decreases the response rate by 32%!

### What about giving a black sounding name and different kinds of information?
summary(lm(pos_resp ~ black+info_pos+info_neg+blackXinfo_pos+blackXinfo_neg, data = myData))

### The partial effect of a black sounding name and giving negative information versus
### not having a black sounding name and giving negative information is lower!
### For white sounding names, the partial effect is -33.7%.
### For black sounding names, the partial effect is -33.7% + 4.5% = -29.2%
### however the total effect is -33.7% + 4.5% - 9% lower.
### The information is weighted more heavily for those without black sounding names
### which may be evidence for statistical discrimination in the housing market.
### If there is no statistical discrimination, then information should have the same impact
### for black and white sounding names (coefficients on blackXinfo_pos and blackXinfo_neg should be zero). 
