## trying some plots


library(plm)
library(sandwich)
library(lmtest)
library(performance)
library(patchwork)
library(e1071)
library(tidyverse)
library(betareg)
library(ggplot2)
library(writexl)
library(readxl)
library(psych)
library(performance)
library(sjPlot) ## Tables
library(stargazer) ## tables


library(extrafont)
font_import(pattern = "Times", prompt = FALSE)
loadfonts(device = "pdf")
fonts()

## preliminary analysis: PART 1

## uploading the data

municipalities_2010_final <- read_excel("/Users/malucampos/Documents/personal_blog/faith_fertility/municipalities_2010_final.xlsx")

## understanding the data

psych::describe(municipalities_2010_final$early_pregnancy_rt, quant = c(.25, .75))


hist(municipalities_2010_final$early_pregnancy_rt,
     main = "Histogram Early Pregnancy Rate (10 to 19 years)", xlab = "Early Pregnancy Rate")


hist_early_pregnancy <- ggplot(municipalities_2010_final, aes(x = early_pregnancy_rt)) +
  geom_histogram(binwidth = 0.01, fill = "pink", color = "black") +
  labs(
    title = "Histogram Early Pregnancy Rate (10 to 19 years)",
    x = "Early Pregnancy Rate",
    y = "Frequency"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )
print(hist_early_pregnancy)

# linear beta model (simple model): PART 2

beta_evangelical <- betareg(early_pregnancy_rt ~ envangelical_pentecostal, 
                            data = municipalities_2010_final,
                            link = "logit")
summary(beta_evangelical)

## Once running a simple beta regression, the proportion of evangelicals in the municipality
## is significant at 5% to explain the log-odds of the early pregnancy rate between 10 to 19 years old. 

## TABLE

custom_labels <- c("(Intercept)" = "Intercept",
                   "envangelical_pentecostal" = "Proportion of Evangelicals Pentecostal")

tab_model(beta_evangelical,
          show.ci = NULL, 
          show.se = TRUE, 
          show.est = TRUE, 
          show.std = NULL,
          transform = NULL,
          pred.labels = custom_labels,  # Apply custom labels
          dv.labels = c("Early Pregnancy Rate"),
          title = "Beta Model Summaries")

## Let's make this results interpretable. 

# extracting coefficients

coefficients <- coef(beta_evangelical)

# calculating odds ratio

odds_ratio <- exp(coefficients["envangelical_pentecostal"])

## print results

print(odds_ratio)

## The odds ratio is equal to 2.16, which is above 1. An odds ratio above one indicates a 
## positive relationship: higher the proportion of evangelicals in the municipality, higher is the
## odds of early pregnancy. 

## Odds is different than probability: 
##### Probability is the ratio between favorable outcomes and total possible outcomes.
##### (favorable outcomes / favorable outcomes + unfavorable outcomes).
##### Probability is between 0 and 1.
#### Odds is the ratio between favorable outcomes and unfavorable outcomes. 
### Odds can be above 1. 

### So, in this case, the interpretation is: for a one-unit increase in the proportion of evangelicals 
## in a municipality, the odds of having a higher early pregnancy rate are, approximately, 2 times the odds of having 
### a higher early pregnancy rate with the original proportion of evangelicals.

## Calculating the percentage change

percentm_change <- (odds_ratio - 1) * 100

## The odds of early fertility increases by 116%. 

# DOING THE GRAPH

# passing the predicted values to the exponential form 

municipalities_2010_final <- municipalities_2010_final %>%
  mutate(predicted_logit = predict(beta_evangelical, type = "link"),
         predicted_odds = exp(predicted_logit))

ggplot(municipalities_2010_final, aes(x = envangelical_pentecostal, y = predicted_odds)) +
  geom_point(color = "black") +
  geom_smooth(method = "lm", se = FALSE, color = "pink") +
  labs(
    title = "Impact of the proportion of evangelicals on  the Odds Ratio of Early Pregnancy",
    x = "Proportion of evangelicals",
    y = "Predicted Odds Ratio of Early Pregnancy (15 to 19 years old)"
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman")
  )

## linear beta model with controls: PART 3

## putting some controls: log gdp, hdi, enrollments in basic education, access to health services

beta_evangelical_urban <- betareg(early_pregnancy_rt ~ envangelical_pentecostal + urban_female_enrollments
                            + log_gdp_pc + primary_health_care_coverage + n_health_units_with_notifications, 
                            data = municipalities_2010_final)
summary(beta_evangelical_urban)

beta_evangelical_rural <- betareg(early_pregnancy_rt ~ envangelical_pentecostal + rural_female_enrollments
                                  + log_gdp_pc + primary_health_care_coverage + n_health_units_with_notifications, 
                                  data = municipalities_2010_final)
summary(beta_evangelical_rural)

## making tables

custom_labels_final <- c("(Intercept)" = "Intercept",
                         "envangelical_pentecostal" = "Proportion of Evangelicals Pentecostal",
                         "urban_female_enrollments" = "Female Enrollments in Basic Education (Urban)",
                         "rural_female_enrollments" = "Female Enrollments in Basic Education (Rural)",
                         "log_gdp_pc" = "Log GDP Per Capita",
                         "primary_health_care_coverage" = "Primary Health Care Coverage",
                         "n_health_units_with_notifications" = "Health Units with sexual/domestic abuse notifications")

tab_model(beta_evangelical_urban, beta_evangelical_rural,
          show.ci = NULL, 
          show.se = TRUE, 
          show.est = TRUE, 
          show.std = NULL,
          transform = NULL,
          pred.labels = custom_labels_final,
          dv.labels = c("Urban", "Rural"),
          title = "Models With Some Covariates",
          digits = 3) 

## plotando

municipalities_2010_final <- municipalities_2010_final %>%
  mutate(predicted_logit_urban = predict(beta_evangelical_urban, type = "link"),
         predicted_odds_urban = exp(predicted_logit_urban))

municipalities_2010_final <- municipalities_2010_final %>%
  mutate(predicted_logit_rural = predict(beta_evangelical_rural, type = "link"),
         predicted_odds_rural = exp(predicted_logit_rural))

## graphs

plot_urban <- ggplot(municipalities_2010_final, aes(x = envangelical_pentecostal, y = predicted_odds_urban)) +
  geom_point() +
  geom_line(aes(y = predicted_odds_urban), color = "black") +
  geom_smooth(method = "loess", color = "pink", se = FALSE) +
  labs(title = "Scatter Plot with Beta Regression Fit (Urban Enrollments)",
       x = "Proportion of Evangelical Pentecostal",
       y = "Predicted Odds of the Early Pregnancy Rate") +
  theme_minimal(base_family = "Times New Roman")
print(plot_urban)

plot_rural <- ggplot(municipalities_2010_final, aes(x = envangelical_pentecostal, y = predicted_odds_rural)) +
  geom_point() +
  geom_line(aes(y = predicted_odds_rural), color = "black") +
  geom_smooth(method = "loess", color = "pink", se = FALSE) +
  labs(title = "Scatter Plot with Beta Regression Fit (Rural Enrollments)",
       x = "Proportion of Evangelical Pentecostal",
       y = "Predicted Odds of the Early Pregnancy Rate") +
  theme_minimal(base_family = "Times New Roman")
print(plot_rural)

# só a linha suavizada -------------------------------------------------------------------------

plot_urban_smooth <- ggplot(municipalities_2010_final, aes(x = envangelical_pentecostal, y = predicted_odds_urban)) +
  geom_point() +
  geom_smooth(method = "loess", color = "pink", se = FALSE) +
  labs(title = "Scatter Plot with Beta Regression Fit (Urban Enrollments)",
       x = "Proportion of Evangelical Pentecostal",
       y = "Predicted Odds of the Early Pregnancy Rate") +
  theme_minimal(base_family = "Times New Roman")
print(plot_urban_smooth)

plot_rural_smooth <- ggplot(municipalities_2010_final, aes(x = envangelical_pentecostal, y = predicted_odds_rural)) +
  geom_point() +
  geom_smooth(method = "loess", color = "pink", se = FALSE) +
  labs(title = "Scatter Plot with Beta Regression Fit (Rural Enrollments)",
       x = "Proportion of Evangelical Pentecostal",
       y = "Predicted Odds of the Early Pregnancy Rate") +
  theme_minimal(base_family = "Times New Roman")
print(plot_rural_smooth)


# odds ratio --------------------------------------------------------------

coefficients_urban <- coef(beta_evangelical_urban)
coefficients_rural <- coef(beta_evangelical_rural)

odds_ratio_urban <- exp(coefficients_urban["envangelical_pentecostal"])
odds_ratio_rural <- exp(coefficients_rural["envangelical_pentecostal"])

print(odds_ratio_urban)
print(odds_ratio_rural)



