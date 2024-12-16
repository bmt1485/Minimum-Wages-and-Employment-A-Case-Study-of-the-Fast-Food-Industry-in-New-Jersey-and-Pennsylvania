
##################################################
# ECON 418-518 Exam 3
# Ben Taylor
# The University of Arizona
# bmt1485@arizona.edu 
# 15 December 2024
###################################################

#####################
# Preliminaries
#####################
# Clear environment, console, and plot pane
rm(list = ls())
cat("\014")
graphics.off()

# Turn off scientific notation
options(scipen = 999)

# Load packages
pacman::p_load(data.table)

# Set sead
set.seed(418518)

# Set working directory 
setwd("/Users/bentaylor/Desktop/ECON 418 R/Data")

# Load data
data <- read.csv("ECON_418-518_Exam_3_Data.csv")
data <- as.data.table(data)


#####################
# (i)
#####################
#no work in r - answered in notebook

#####################
# (ii)
#####################
# Create indicator columns
data[, Post := ifelse(time_period == "Nov", 1, 0)]
data[, NJ := ifelse(state == 1, 1, 0)]

# Compute mean employment for each
mean_emp <- data[, .(Mean_Employment = mean(total_emp, na.rm = TRUE)), by = .(state, Post)]
print(mean_emp)


#####################
# (iii)
#####################
# Compute mean employment for each group
mean_values <- data[, .(Mean_Employment = mean(total_emp, na.rm = TRUE)), by = .(NJ, Post)]
print(mean_values)


# Calculate the DiD estimate
NJ_Post <- mean_values[NJ == 1 & Post == 1, Mean_Employment]
NJ_Pre <- mean_values[NJ == 1 & Post == 0, Mean_Employment]
PA_Post <- mean_values[NJ == 0 & Post == 1, Mean_Employment]
PA_Pre <- mean_values[NJ == 0 & Post == 0, Mean_Employment]

DiD_Estimate <- (NJ_Post - NJ_Pre) - (PA_Post - PA_Pre)
print(DiD_Estimate)

#####################
# (iv)
#####################
# Estimate the  model
model <- lm(total_emp ~ NJ + Post + NJ:Post, data = data)
summary(model)

# Extract coefficient and standard error
beta_3 <- coef(model)["NJ:Post"]
se_beta_3 <- summary(model)$coefficients["NJ:Post", "Std. Error"]

# Manually compute 95% CI
lower_bound <- beta_3 - 1.96 * se_beta_3
upper_bound <- beta_3 + 1.96 * se_beta_3
cat("95% Confidence Interval:", lower_bound, "to", upper_bound, "\n")
#####################
# (v).  No work in R necessary
##########################################
# (vi). No work in R necessary
##########################################
# (vii)
#####################
# Compute restaurant-level means for key variables
data[, mean_total_emp := mean(total_emp, na.rm = TRUE), by = restaurant_id]
data[, mean_NJ := mean(NJ, na.rm = TRUE), by = restaurant_id]
data[, mean_Post := mean(Post, na.rm = TRUE), by = restaurant_id]
data[, mean_interaction := mean(NJ * Post, na.rm = TRUE), by = restaurant_id]

# Step 2: Demean the Variables
data[, demeaned_total_emp := total_emp - mean_total_emp]
data[, demeaned_NJ := NJ - mean_NJ]
data[, demeaned_Post := Post - mean_Post]
data[, demeaned_interaction := (NJ * Post) - mean_interaction]

# Step 3: Manually Calculate the DiD Estimate
DiD_estimate <- mean(data[, demeaned_total_emp * demeaned_interaction]) / 
  mean(data[, demeaned_interaction^2])

# Print the DiD estimate
cat("DiD Estimate with Fixed Effects:", DiD_estimate, "\n")

