# Machine Learning in Econometrics - Empirical Project for Exam

# Goal: Use a difference-in-differences analysis for identification and estimation of the causal effect of a policy reform increasing financial benefits to parents on outcomes such as income, employment, full-time employment, and job satisfaction

# Given: 
# (i) We are given a panel dataset containing observations on individuals living in different German states across the years 2005, 2010, 2015, 2020. 
# (ii) We have to use machine learning methods to estimate  estimate the causal effect while flexibly controlling for the available individual characteristics

# Author: Lolakshi Rajlakshmi (12339702)

# This submission consists of 3 files, to be run in the following order
# 1. Submission_preliminaries.R - Data exploration and preparation
# 2. Submission_estimation_v2.R - Estimation of the ATT, standard errors and heterogeneity analysis
# 3. Submission_other.R - Checking assumptions for DiD 

# (0) Preliminaries -----------------------------------

# Clear all
rm(list = ls())

# Disabling scientific notation - optional
options(scipen = 999)

# Loading required packages
dependencies <- c("tidyverse", "glmnet", "randomForest", "rpart", "xtable", "data.table", "caret", "doParallel", "neuralnet") 
install.packages(dependencies) # installs all packages mentioned in dependencies
lapply(dependencies, library, character.only = TRUE) # loads all packages from dependencies into current working environment

# Set working directory
setwd("/Users/BRAJENDRA2/Courses/ML_Econ/Exam") # Please insert your individual path here, if needed

# Loading the data 
load("~/Courses/ML_Econ/Exam/BavariaReform.RData") 

# (1) Data Exploration & Preparation -----------------------------------

# (1.1) Exploration ####
head(data) # Look at first few lines of data
View(data) # Open data in new tab 
summary(data) # Check data types and ranges for all variables
groups(data) # NOTE : Variables are grouped at the 'id' level

# (1.1.1) Going through dataset variable by variable, looking at unique values and distribution #

# id:
sort(unique(data$id)) # the unique id of any individual in this panel dataset

# State:
sort(unique(data$state)) # State contains character coded information on which state the individual lives in 
# TODO : Change to numeric factors to use with glm which requires covariate matrix to have a uniform variable class
ggplot(data, aes(x = state)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Frequency of Categories", x = "Category", y = "Frequency") +
  theme_minimal()

# Age: 
sort(unique(data$age))
# Individuals range from 25 - 55 in our sample, with a fairly uniform distribution

# Education: 
sort(unique(data$education)) # Number of years from 0 to 19 - Master's degree seems to be the highest degree obtained - (12 - 13 years in school + 3-4 years in Bachelors + 1-2 years in a Masters)
ggplot(data, aes(x = education)) +
  geom_histogram(fill = "steelblue") +
  labs(title = "Frequency of Categories", x = "Category", y = "Frequency") + 
  theme_minimal() 


# Married - dummy variable with 1 = married and 0 = not married
mean(data$married)   

# Female - dummy variable with 1 = female and 0 = male
mean(data$female) 

# Income  
ggplot(data, aes(x = income)) +
  geom_histogram(fill = "steelblue") +
  labs(title = "Frequency of Categories", x = "Category", y = "Frequency") +
  theme_minimal() # large number with 0 income

# Children
sort(unique(data$children))

ggplot(data, aes(x = children)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Frequency of Categories", x = "Category", y = "Frequency") +
  theme_minimal()

# Health status - self-reported indicator for health (poor, fair, good, excellent)
sort(unique(data$health_status))
ggplot(data, aes(x = health_status)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Frequency of Categories", x = "Category", y = "Frequency") +
  theme_minimal()

# Job_satisfaction
sort(unique(data$job_satisfaction))
ggplot(data, aes(x = job_satisfaction)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Frequency of Categories", x = "Category", y = "Frequency") +
  theme_minimal()

# Commuting time
sort(unique(data$commuting_time))
ggplot(data, aes(x = commuting_time)) +
   geom_histogram(fill = "steelblue") +
   labs(title = "Frequency of Categories", x = "Category", y = "Frequency") +
   theme_minimal()
View(data %>% filter(is.na(commuting_time))) # Warning from plot related to NAs due to employed = 0 
# TODO : Code commuting time, job satisfaction, any other variables that depend on employed = 0 for being NA
# NOTE: There are -ve values of commuting time!

# Homeownership - dummy variable with 1 = own the home and 0 =not own the home
mean(data$homeownership)


# Employed - dummy variable with 1 = employed and 0 = not employed
mean(data$employed) # 90% employed

# fulltime - dummy variable with 1 = working full-time and 0 = working part-time
mean(data$fulltime, na.rm = T) # 75.8%

# year
ggplot(data, aes(x = year)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Frequency of Categories", x = "Category", y = "Frequency") +
  theme_minimal() # Balanced panel! # NOTE : Verify

# treat - treatment indicator, = 1 if individual lived in Bavaria in 2020, = 0 otherwise
mean(data$treat) # 0.067 percent

# (1.1.2) Exploring some trends that might exist in the population, but do they hold in this sample #

# Do women work more part-time in the sample?
summary(data %>% select(id, female, employed, fulltime) %>% filter(female == 1 & employed == 1))
summary(data %>% select(id, female, employed, fulltime) %>% filter(female == 0 & employed == 1))
# shows that 76 % women work full time vs 75 % men in this sample

# Do women earn more or less than men in the sample? 
summary(data %>% select(id, female, income) %>% filter(female == 1))
summary(data %>% select(id, female, income) %>% filter(female == 0)) # no, both earn almost the same mean and range

# Do healthier people work more in the sample?
data %>% group_by(health_status) %>% summarise(mean = mean(employed)) # No

# Do women with children work more or less than women without? Same for men? 
data %>% group_by(children) %>% summarise(mean = mean(employed)) # No

# (1.2) Preparation ####
 # Changing to numeric factors for character/ factor variables such as health_code, state
# Creating numeric codes for health status from 1 to 4, 1 being Excellent and 4 being Poor
data <- data %>% mutate(health_code = case_when(
  health_status == "Excellent" ~ 1,
  health_status == "Fair" ~ 2,
  health_status == "Good" ~ 3,
  health_status == "Poor" ~ 4,
)) %>% mutate(state_code = case_when(
  state == "Baden-Wuerttemberg" ~ 2,
  state == "Bavaria" ~ 1,
  state == "Berlin" ~ 3,
  state == "Brandenburg" ~ 4,
  state == "Bremen" ~ 5,
  state == "Hamburg" ~ 6,
  state == "Hesse" ~ 7,
  state == "Lower Saxony" ~ 8,
  state == "Mecklenburg-Vorpommern" ~ 9,
  state == "North Rhine-Westphalia" ~ 10,
  state == "Rhineland-Palatinate" ~ 11,
  state == "Saarland" ~ 12,
  state == "Saxony" ~ 13,
  state == "Saxony-Anhalt" ~ 14,
  state == "Schleswig-Holstein" ~ 15,
  state == "Thuringia" ~ 16,
)) %>% select(-state, - health_status)

# Coding employment dependent variables - Some variables have NAs when employed = 0, since logically they do not have any meaning if the individual is unemployed. But here, we need to recode them because several commands do not accept NA
data <- data %>% mutate(fulltime = if_else(employed == 0, 0, fulltime),
  job_satisfaction = if_else(employed == 0, 0, job_satisfaction),
  commuting_time = if_else(employed == 0, 0, commuting_time)) 


save(data, file = "data_v2.RData")
rm(list = ls())

# (Next Steps) --------------------------
# With this dataset, we will compute the ATT for income, employment, full-time employment and job satisfaction. For this, we will use a Neyman-Orthogonal estimator dervied from the IInd moment condition, as described in the text.

# For this, we need to (separately for each outcome): 

# (i) Construct an estimator of gamma_delta_hat of gamma_delta: Using all observations, regress Y(2020)_i - Y(2015)_i on all covariates, where Y is the outcome of interest (For estimating 𝐸[del𝑌|𝐷=0,𝑋])

# (ii) Construct an estimator of gamma_p_hat of gamma_p: Using all observations, regress assignment to treatment D_i on all covariates, same across outcomes (For estimating E(D/X))

# (iii) Estimate E(D) = P(D = 1)

# (iv) Plug in these estimates to estimate ATT 

# For the conditional expectation functions, we will consider different modern ML regression methods, namely: Constant (= no controls); a linear combination of the controls; an expansion of the raw control variables including all second order interactions; Lasso (CV); Random Forest; and CV Tree. The methods indicated with CV have their tuning parameter selected by cross-validation.


