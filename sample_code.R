###########################################################################
# Example OLS Function
###########################################################################
# This sample code loads a data package in R and shows how to load and call the OLS function
# a shown in <http://www.r-bloggers.com/standard-robust-and-clustered-standard-errors-computed-in-r/>
###########################################################################

############################## 
# Housekeeping
#############################

# Clear Workspace
rm(list=ls())

# Set Directory
setwd("/Users/korydkantenga/Dropbox/Teaching/Econometrics/104-920/Exams/Final_Exam_Kit/")

# Load Libraries
library("stargazer")

#############################
# Load OLS Estimator into R
#############################
# ols_function.R must be in current directory

source("ols_function.R")

#############################
# Load Preset Data
#############################

ETW2014 <- read.csv("mainData.csv")

# Convert to Data Frame
ETW2014.df <- data.frame(ETW2014)

#############################
# Regression Model
#############################

# Keep data only for which a response took place
ETW2014.info_nil <- subset(ETW2014.df, info_nil==1)

# Regression and Data Specification
form <- pos_resp ~ black
data <- ETW2014.info_nil

# Regression without Robust Standard Errors
lm.ols <- ols(form,data,robust = FALSE,cluster=NULL)

# Regression with Robust Standard Errors
lm.rse <- ols(form,data,robust = TRUE,cluster=NULL)

# Regression with Robust Clustered Standard Errors
lm.rcse <- ols(form,data,robust = TRUE,cluster="neighborhood_id")

#########################################
# Save and Format Regression Diagnostics
#########################################
lm.model <- lm(form,data)
lm.diag <- summary(lm.model)

# Sample Size
lm.ssample <- dim(data)[1]
lm.ssample <- matrix(as.numeric(lm.ssample),nrow=1)
rownames(lm.ssample) <- c("Sample Size")
colnames(lm.ssample) <- c("")

# Statistics
lm.diag <-rbind(lm.diag$sigma,lm.diag$r.squared,lm.diag$adj.r.squared,lm.diag$fstatistic[1],AIC(lm.model),BIC(lm.model),logLik(lm.model))
lm.diag <- matrix(as.numeric(sprintf(paste("%.",paste(3,"f",sep=""),sep=""),lm.diag)),nrow=dim(lm.diag)[1])
rownames(lm.diag) <- c("Residual S.D.","R-Squared","Adj R-Squared","F-Statistic","Akaike IC","Schwartz IC","Log-Likelihood")
colnames(lm.diag) <- c("")

#############################
# Display Results
#############################
stargazer(lm.ols, type="text",title="OLS")
stargazer(lm.rse, type="text",title="OLS with Robust SE")
stargazer(lm.rcse, type="text",title="OLS with Robust Clustered SE")
print(lm.diag)
print(lm.ssample)

