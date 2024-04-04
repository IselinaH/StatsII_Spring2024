# Installing eha package
if (!requireNamespace("eha", quietly = TRUE)) {
  install.packages("eha")
}

# Load the eha package
library(eha)

# Load extra survival package, required for survival analysis functions
library(survival)

# Load the dataset
data(child)

# Inspect the structure of the dataset to make variables names visible
str(child)

# Fit the Cox Proportional Hazards model
cox_model <- coxph(Surv(exit, event) ~ m.age + sex, data = child)

# Print the summary of the Cox model
summary(cox_model)