####################
## Statistics for Data Scientists
## Coursework 1 R Script
## KCP
## 18/12/2023
####################

setwd("/Users/katepoole/Documents/Kings/Statistics for Data Science - 7CCMMS61/Coursework 1")

# Load packages
library(ggplot2)
library(stringr)
library(dplyr)
library(e1071)
library(lindia)


## VIF neeed packages will not download :(
#library(devtools)
#install.packages("devtools")
#install.packages("car", dependencies = TRUE)
#install.packages("car", dependencies = "Imports")
#library("car")
#install_version("car", version = "3.0-2", repos = "http://cran.us.r-project.org")
#library(car)


# Read Data
data <- read.csv('mobile.csv', header=TRUE)


#####
# Exploratory Data Analysis
#####

## check NANs ##
check_nulls <- function(x) {
  any(is.nan(x))
}

sapply(data, check_nulls)
# no NAs, all look good 


## Descriptive stats ##

descriptive_stats <- function(){
  
  res_summary <- list()
  
  res_summary[["Mean"]] <- apply(data,2,mean) 
  # Take all columns & rows, compute the "mean" for each (margin=2; where margin=1 are rows and margin=2 are columns)
  res_summary[["Median"]] <- apply(data,2,median)
  res_summary[["SD"]] <- apply(data,2,sd)
  res_summary[["Min"]] <- apply(data,2,min)
  res_summary[["Max"]] <- apply(data,2,max)
  #res_summary[["Q1"]] <- apply(data,2,quantile,probs=0.75)
  #res_summary[["Q3"]] <- apply(data,2,quantile,probs=0.25) 
  #res_summary[["Skewness"]] <- apply(data,2,skewness)
  
  return(data.frame(res_summary))
  
}
# descriptive_stats function referenced to SDA week 2 lab script

# output:
print(descriptive_stats())


## Histograms ##

# attach data
attach(data)

# generate histograms for all variables 
par(mfrow = c(4,3))

for (column in names(data)) {
  if (column == "Price_AUS") {
    col_name <- "Price (AUS)"
  } else {
    col_name <- str_to_title(gsub("[._]", " ", column))
  }
  
  hist(data[[column]], 
  main = paste(col_name),
  xlab = col_name,
  col = "royalblue3")
}

# some histograms appear to display right skewness -- generate skew coefficients
sapply(data, skewness)
# as suspected, there are a few quite high skewness scores. Use log transformation for regression 


## Logarithmic transformation of skewed variables ## 
# weight, resolution, ram, internal memory, front cam, battery, thickness

# save raw data for reference
data_raw <- data

# add +1 constant to account for 0 values in log transformation
data$log_weight <- log(data$weight_gr +1)
data$log_resolution <- log(data$resoloution +1)
data$log_internal_mem <- log(data$internal.mem +1)
data$log_front_cam <- log(data$Front_Cam +1)
data$log_battery <- log(data$battery +1)
data$log_thickness <- log(data$thickness +1)

# rearrange data so raw and log columns are next to eachother
data <- data[ , c("Price_AUS", "weight_gr", "log_weight", "resoloution", "log_resolution", "ppi", "cpu.core", "cpu.freq", "internal.mem", "log_internal_mem",
               "ram", "RearCam", "Front_Cam", "log_front_cam", "battery", "log_battery", "thickness", "log_thickness")]
# reordering method sourced to https://stackoverflow.com/questions/5620885/how-does-one-reorder-columns-in-a-data-frame

# data frame with only non log data
data_raw <- data_raw[ , c("Price_AUS", "weight_gr", "resoloution", "ppi", "cpu.core", "cpu.freq", "internal.mem", "ram",
                          "RearCam", "Front_Cam", "battery", "thickness")]

# data frame with only log transformed and raw if no log fields 
data_log <- data[ , c("Price_AUS", "log_weight", "log_resolution", "ppi", "cpu.core", "cpu.freq", "log_internal_mem",
                      "ram", "RearCam", "log_front_cam", "log_battery", "log_thickness")]

# regenerate histograms to check distributions
par(mfrow = c(4,3))

for (column in names(data)) {
  if (column == "Price_AUS") {
    col_name <- "Price (AUS)"
  } else {
    col_name <- str_to_title(gsub("[._]", " ", column))
  }
  
  hist(data[[column]], 
       main = paste(col_name),
       xlab = col_name,
       col = "royalblue3")
}
# looks much better

# check skewness factors
sapply(data, skewness)
# sort of weird that resolution flipped so far negative, but still a less significant value than before. 
# Continue with transformed variable


## Scatterplots
gg_scatterplots <- function(data, y_variable = "Price_AUS") {
  # Get all variable names except price Y
  x_variables <- setdiff(names(data), y_variable)
  
  # build combined scatterplots for all variables X against Y
  plot_list <- lapply(x_variables, function(variable) {
    x_label <- str_to_title(gsub("[._]", " ",variable)) # capitalize and remove weird characters in X variables for chart titles
    plot_title <- paste(x_label, "vs Price")
    plot <- ggplot(data, aes_string(x = variable, y = y_variable)) + 
      geom_point(color = "royalblue3", pch = 18) + 
      geom_smooth(method = "lm", se = FALSE, color = "black", size = 0.5, linetype = "dashed") +  # add best fit line to visualize strength of linearity
      ggtitle(plot_title) + # add plot titles & labels 
      xlab(x_label) +
      ylab("Price (AUS)") + 
      theme(plot.title = element_text(size=8)) +
      theme_light() 
  })
  
  # combine all plots into a single output
  all_plots <- do.call(gridExtra::grid.arrange, c(plot_list, ncol = 3))
  return(all_plots)
  # ggplot combination code referenced to chatGPT
}

# output: 
gg_scatterplots(data)


## scatterplot matrix for every X variable against eachother 
# grab all variables from mobile data other than price -- correlating that by itself 
variables <- names(data_raw)[names(data_raw) != "Price_AUS"]

# Create scatterplot matrix
pairs(~., data = data_raw[, variables], 
      main = "Explanatory Variable X Scatterplot Matrix",
      col = "royalblue3",
      pch = 18)
# pairs scatterplot matrix code referenced to ChatGPT


# correlation coefficient between each variable
print(cor(data_log))



#####
# Regression Model
#####

# Regression w/ variable selection using backwards elimination 
  # 1. start with all predictor variables in model 
  # 2. Remove predictor with highest p-value greater than level of significance
  # 3. fit model and remove the next least significant predictor as long as pvalue is larger than alpha 
  # 4. continue until all non-significant predictors are removed 

# Round 1: All variables 
model <- lm(Price_AUS ~ log_weight + log_resolution + ppi + cpu.core + cpu.freq + log_internal_mem + ram + 
              RearCam + log_front_cam + log_battery + log_thickness,
            data = data_log)
summary(model)
# looks like variable rear camera has the largest p-value, take out 

# Round 2: removed rear camera 
model_2 <- lm(Price_AUS ~ log_weight + log_resolution + ppi + cpu.core + cpu.freq + log_internal_mem + ram + 
                log_front_cam + log_battery + log_thickness,
              data = data_log)
summary(model_2)
# take out front cam 

# Round 3: removed front cam 
model_3 <- lm(Price_AUS ~ log_weight + log_resolution + ppi + cpu.core + cpu.freq + log_internal_mem + ram + 
                log_battery + log_thickness,
              data = data_log)
summary(model_3)
# take out weight 

# Round 4: removed weight
model_4 <- lm(Price_AUS ~ log_resolution + ppi + cpu.core + cpu.freq + log_internal_mem + ram + log_battery + log_thickness,
              data = data_log)
summary(model_4)
# take out cpu frequency 

# Round 5: remove cpu frequency
model_5 <- lm(Price_AUS ~ log_resolution + ppi + cpu.core + log_internal_mem + ram + log_battery + log_thickness,
              data = data_log)
summary(model_5)
# all variables significant (alpha = 5%) now. 



########
# Diagnostic checks
########

## Residuals plot ##

# plot residuals against fitted values
par(mfrow = c(1,1))

ggplot(data_log, aes(x = model_5$fitted.values, y = model_5$residuals)) +
  geom_point(pch = 18) +
  ggtitle('Fitted vs Residuals Plot') + 
  xlab('Fitted Values') + 
  ylab('Residuals') + 
  theme_light()


## qq-plot ##

# make sure residuals are normally distributed with qqplot
gg_qqplot(model_5) + 
  theme_light()


## Test model for collinearity ##

# The needed cars package is depreciated and I was unable to run VIF calculations. I spent a generous amount of time trouble shooting
# to no avail. I attempted to write a calculate VIF function (with the help of chat GPT), however could not get that to work either.
# In a perfect world, I would want to run VIF on the final fitted model for collinearity testing!

# vif(model_5)
# cannot get to run, package depreciation in my R version 

# calculate_vif <- function(model) {
#   # Extracting model matrix
#   X <- model.matrix(model)
#   
#   # Initialize a vector to store VIF values
#   vif_values <- numeric(ncol(X))
#   
#   # Loop through each variable
#   for (i in seq_along(vif_values)) {
#     # Fit a linear regression model excluding the ith variable
#     reduced_model <- lm(as.vector(X[, i]) ~ X[, -i] + 0)
#     
#     # Calculate R-squared and VIF
#     vif_values[i] <- 1 / (1 - summary(reduced_model)$r.squared)
#   }
#   names(vif_values) <- colnames(X)
#   return(vif_values)
# }
# # Function to calculate VIF referenced to chatGPT
# 
# calculate_vif(model_5)





