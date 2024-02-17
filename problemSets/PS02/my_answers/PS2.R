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

lapply(c(),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#####################
# Problem 1
#####################

# load data
load(url("https://github.com/ASDS-TCD/StatsII_Spring2024/blob/main/datasets/climateSupport.RData?raw=true"))

#fit an additive model using glm() with the family = binomial()
model_additive <- glm(choice ~ countries + sanctions, data = climateSupport, family = binomial)

#examine the summary output
summary(model_additive)

#####################
# Problem 2
#####################

#2b
#estimated probability that an individual will support the policy if there are 80 of 192 countries participating with no sanctions
# Ensure 'countries' and 'sanctions' are treated as numeric variables
climateSupport$countries <- as.numeric(climateSupport$countries)
climateSupport$sanctions <- as.numeric(climateSupport$sanctions)

# Fit the additive model
model_additive <- glm(choice ~ countries + sanctions, data = climateSupport, family = binomial)

# Predict the probability for 80 of 192 countries participating with no sanctions
pred_80_no_sanctions <- predict(model_additive, newdata = data.frame(countries = 80, sanctions = 0), type = "response")

#2c
# Ensure 'countries' and 'sanctions' are treated as numeric variables
climateSupport$countries <- as.numeric(climateSupport$countries)
climateSupport$sanctions <- as.numeric(climateSupport$sanctions)

# Fit the additive model
model_additive <- glm(choice ~ countries + sanctions, data = climateSupport, family = binomial)

# Fit the model with interaction term
model_interaction <- glm(choice ~ countries * sanctions, data = climateSupport, family = binomial)

# Perform ANOVA test
anova_result <- anova(model_additive, model_interaction)

# Check if interaction term is significant
if (anova_result$`Pr(>Chisq)`[2] < 0.05) {
  cat("The interaction term is significant.\n")
} else {
  cat("The interaction term is not significant.\n")
}






