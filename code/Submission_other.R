# Load data for further analysis
load("~/Courses/ML_Econ/Exam/data_v2.RData") 

# Required libraries
library(dplyr)
library(lmtest)
library(sandwich)
library(ggplot2)

# (A) Evaluating the assumptions made for DID analysis ------

# (A. i) Conditional pre-trends assumption ####

# To test the conditional parallel trends assumption, we need to compare the trends in outcomes between the treatment and control groups before the reform (i.e., until 2015) while controlling for covariates.
# We will do this using an event study setup:

# Prepare the Data: Remove post-treatment year 2020 and create a dummy variable for each of the years to use in the event study.
data_pre_reform <- data %>% mutate(year_dummy = factor(year))

# Run the event studies: 
# For income
event_study_model_income <- lm(income ~ year_dummy * treat + age + education + married + female + children + job_satisfaction + commuting_time + homeownership + employed + fulltime,
                        data = data_pre_reform)
summary(event_study_model_income)

# For Employed
event_study_employed <- lm(employed ~ year_dummy * treat + age + education + married + female + income + children + job_satisfaction + commuting_time + homeownership + fulltime,
                           data = data_pre_reform)
summary(event_study_employed)

# For Fulltime Employment
event_study_fulltime <- lm(fulltime ~ year_dummy * treat + age + education + married + female + income + children + job_satisfaction + commuting_time + homeownership + employed,
                           data = data_pre_reform)
summary(event_study_fulltime)

# For Job Satisfaction
event_study_job_satisfaction <- lm(job_satisfaction ~ year_dummy * treat + age + education + married + female + income + children + employed + commuting_time + homeownership + fulltime,
                                    data = data_pre_reform)
summary(event_study_job_satisfaction)

# Plotting the results:
# Create a function to extract and plot coefficients
plot_event_study <- function(model, outcome_name) {
  coefs <- summary(model)$coefficients
  coefs_df <- as.data.frame(coefs)
  coefs_df$term <- rownames(coefs_df)
  coefs_df <- coefs_df %>% filter(grepl("year_dummy", term))
  
  ggplot(coefs_df, aes(x = term, y = Estimate, ymin = Estimate - 1.96 * `Std. Error`, ymax = Estimate + 1.96 * `Std. Error`)) +
    geom_point() +
    geom_errorbar() +
    labs(title = paste("Event Study Coefficients for", outcome_name),
         x = "Year",
         y = "Coefficient Estimate") +
    theme_minimal()
}

# Plot for each outcome and save the plot
# Generate the plot and assign it to a variable
income_plot_es <- plot_event_study(event_study_model_income, "Income")
employed_plot_es <- plot_event_study(event_study_employed, "Employed")
jobsat_plot_es <- plot_event_study(event_study_job_satisfaction, "Job Satisfaction")
fulltime_plot_es <- plot_event_study(event_study_fulltime, "Fulltime Employment")

# Save the plot as a PNG file
ggsave(filename = "event_study_income.png", plot = income_plot_es, width = 8, height = 6, dpi = 300)
ggsave(filename = "event_study_employed.png", plot = employed_plot_es, width = 8, height = 6, dpi = 300)
ggsave(filename = "event_study_jobsat.png", plot = jobsat_plot_es, width = 8, height = 6, dpi = 300)
ggsave(filename = "event_study_fulltime.png", plot = fulltime_plot_es, width = 8, height = 6, dpi = 300)


# (A. ii) Common support ####

# To check for common support, we need to ensure that for each treatment observation, there is a comparable control observation with a similar propensity score.

# Estimate propensity scores
prop_model <- glm(treat ~ age + education + married + female + income + children + job_satisfaction + commuting_time + homeownership + employed + fulltime,
                   family = binomial, data = data)

data$propensity_score <- predict(prop_model, type = "response")

# Plot common support
common_support_plot <- ggplot(data, aes(x = propensity_score, fill = factor(treat))) +
  geom_density(alpha = 0.5) +
  labs(title = "Propensity Score Distribution by Treatment Status",
       x = "Propensity Score",
       fill = "Treatment") +
  theme_minimal()

ggsave(filename = "commonsupport_plot.png", plot = common_support_plot, width = 8, height = 6, dpi = 300)

# (A. iii) Linear conditional means - not sure it is possible to check this ####
