#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c("nnet", "MASS"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


#####################
# Problem 1
#####################
#Task1
# Load necessary libraries
if (!require(nnet)) install.packages("nnet")
library(nnet)

# Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load the GDP change data from the provided URL
gdp_data <- read.csv("/Users/iseli/Downloads/gdpChange.csv", stringsAsFactors = F)

# Inspect the structure of the data
str(gdp_data)

# View the first few rows of the data
head(gdp_data)

# Create a categorical variable 'GDPWdiff_cat' with levels "positive", "negative", "no change"
gdp_data$GDPWdiff_cat <- factor(ifelse(gdp_data$GDPWdiff > 0, "positive",
                                       ifelse(gdp_data$GDPWdiff < 0, "negative", "no change")))

# Check the levels of the new factor variable to ensure they are correct
levels(gdp_data$GDPWdiff_cat)

# Set "no change" as the reference level for the factor
gdp_data$GDPWdiff_cat <- relevel(gdp_data$GDPWdiff_cat, ref = "no change")

# Fit the unordered multinomial logit model
model <- multinom(GDPWdiff_cat ~ REG + OIL, data = gdp_data)

# Summarize the model to view the estimated coefficients and other statistics
summary(model)

# Interpret the coefficients
# The coefficients for REG and OIL represent the log odds of observing a "positive" or "negative" GDPWdiff
# relative to "no change", holding all other variables constant. A positive coefficient indicates that
# an increase in the predictor variable is associated with higher odds of the corresponding category of GDPWdiff,
# while a negative coefficient indicates lower odds.

#Task2
# Remove objects from the workspace and detach all non-default packages
rm(list=ls())
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0) for (package in package.list) detach(package, character.only=TRUE)
}
detachAllPackages()

# Load necessary libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
lapply(c("nnet", "MASS"), pkgTest)

setwd("/Users/iseli/Downloads/gdpChange.csv")

# Load the GDP change data
gdp_data <- read.csv("/Users/iseli/Downloads/gdpChange.csv", stringsAsFactors = F)

# Inspect the data structure
str(gdp_data)

# View the first few rows of the dataset
head(gdp_data)

# Create an ordered factor variable for GDPWdiff
gdp_data$GDPWdiff_cat <- factor(ifelse(gdp_data$GDPWdiff > 0, "positive",
                                       ifelse(gdp_data$GDPWdiff < 0, "negative", "no change")),
                                levels = c("negative", "no change", "positive"), ordered = TRUE)

# Fit the ordered multinomial logit model using polr from the MASS package
model_ordered <- polr(GDPWdiff_cat ~ REG + OIL, data = gdp_data, Hess=TRUE)

# Display the summary of the model
summary(model_ordered)

# Interpretation
# Coefficients indicate the change in log odds of moving to a higher category
# Thresholds (cutpoints) separate the categories and are part of the model output

#####################
# Problem 2
#####################
#Task(a)
# load data
mexico_elections <- read.csv("/Users/iseli/Documents/GitHub/StatsII_Spring2024/datasets/MexicoMuniData.csv")



# Load the MexicoMuniData.csv data
mexico_elections <- read.csv("/Users/iseli/Documents/GitHub/StatsII_Spring2024/datasets/MexicoMuniData.csv")

# Inspect the data structure
str(mexico_elections)

# View the first few rows of the dataset
head(mexico_elections)

# Run a Poisson regression
# PAN.visits.06 is the outcome variable
# competitive.district, marginality.06, and PAN.governor.06 are the predictors
poisson_model <- glm(PAN.visits.06 ~ competitive.district + marginality.06 + PAN.governor.06, 
                     data = mexico_elections, family = poisson())

# Display the summary of the model to view coefficients, test statistics, and p-values
summary(poisson_model)

# Extract and display the specific coefficient, test statistic, and p-value for competitive.district
coef_summary <- summary(poisson_model)$coefficients["competitive.district", ]
cat("Coefficient for competitive.district:", coef_summary["Estimate"], "\n")
cat("Test statistic for competitive.district:", coef_summary["z value"], "\n")
cat("P-value for competitive.district:", coef_summary["Pr(>|z|)"], "\n")

#Task(b)
# Fit the Poisson regression model
poisson_model <- glm(PAN.visits.06 ~ competitive.district + marginality.06 + PAN.governor.06, 
                     data = mexico_elections, family = poisson())

# Display the summary of the model
model_summary <- summary(poisson_model)

# Interpret the coefficients for marginality.06 and PAN.governor.06
marginality_coef <- model_summary$coefficients["marginality.06", ]
pan_governor_coef <- model_summary$coefficients["PAN.governor.06", ]

cat("Interpretation of marginality.06 coefficient:\n")
cat("Coefficient estimate:", marginality_coef["Estimate"], "\n")
cat("Standard error:", marginality_coef["Std. Error"], "\n")
cat("z-value:", marginality_coef["z value"], "\n")
cat("P-value:", marginality_coef["Pr(>|z|)"], "\n\n")

cat("Interpretation of PAN.governor.06 coefficient:\n")
cat("Coefficient estimate:", pan_governor_coef["Estimate"], "\n")
cat("Standard error:", pan_governor_coef["Std. Error"], "\n")
cat("z-value:", pan_governor_coef["z value"], "\n")
cat("P-value:", pan_governor_coef["Pr(>|z|)"], "\n")

#Task(c)
# Create a new data frame for the hypothetical district
hypothetical_district <- data.frame(competitive.district = 1, 
                                    marginality.06 = 0, 
                                    PAN.governor.06 = 1)

# Predict the expected count (mean number of visits) for the hypothetical district
predicted_visits <- predict(poisson_model, newdata = hypothetical_district, type = "response")

# Print the estimated mean number of visits
cat("Estimated mean number of visits for the hypothetical district:", predicted_visits, "\n")


