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

set.seed(123)
# create empirical distribution of observed data
ECDF <- ecdf(data)
empiricalCDF <- ECDF(data)
# generate test statistic
D <- max(abs(empiricalCDF - pnorm(data)))

# Define function for the Kolmogorov-Smirnov test
ks_test <- function(data, theoretical_dist) {
  n <- length(data)
  empirical_cdf <- ecdf(data)
  D <- max(abs(empirical_cdf(data) - theoretical_dist(data)))
  p_value <- sqrt(2 * pi) / D * sum(exp(-(2 * (1:100) - 1)^2 * pi^2 / (8 * D^2)))
  return(list(test_statistic = D, p_value = p_value))
}

# Set the seed for reproducibility
set.seed(123)

# Generate 1,000 Cauchy random variables
cauchy_data <- rcauchy(1000, location = 0, scale = 1)

# Perform the Kolmogorov-Smirnov test using the normal distribution as the theoretical reference
result <- ks_test(cauchy_data, pnorm)

# View the test statistic and p-value
result$test_statistic
result$p_value

#####################
# Problem 2
#####################

set.seed (123)
data <- data.frame(x = runif(200, 1, 10))
data$y <- 0 + 2.75*data$x + rnorm(200, 0, 1.5)

# Set the seed for reproducibility
set.seed(123)

# Create the data
data <- data.frame(x = runif(200, 1, 10))
data$y <- 0 + 2.75*data$x + rnorm(200, 0, 1.5)

# Perform OLS regression using BFGS algorithm
model_bfgs <- optim(c(0, 0), function(beta) sum((data$y - beta[1] - beta[2]*data$x)^2), method = "BFGS")

# Extract the coefficients from the BFGS optimization
coefficients_bfgs <- model_bfgs$par

# Fit the OLS regression using lm
model_lm <- lm(y ~ x, data = data)

# Extract the coefficients from the lm model
coefficients_lm <- coef(model_lm)

# Compare the coefficients
coefficients_bfgs
coefficients_lm