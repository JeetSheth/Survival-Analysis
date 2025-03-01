# Survival-Analysis
Survival analysis on simulated data
set.seed(345)

#Simulate data #
n <- 1000

#Covariates #
age <- rnorm(n, mean = 60, sd = 10)
gender <- sample(c("Male", "Female"), n, replace = TRUE)
race <- sample(c("White", "Asian", "Black", "Hispanic", "Arab"), n, replace = TRUE)
treatment <- sample(c("Placebo", "Drug A", "Drug B"), n, replace = TRUE)

#simulate using wiebull distribution #
shape <- 2
scale <- 200

surv_time <- rweibull(n ,shape = shape, scale = scale)

# Simulate censoring (event occurs or is censored)#

censoring_prob <- 0.3
censoring_status <- rbinom(n, 1, 1 - censoring_prob) 

#Dataset#

survival_data <- data.frame(
  age = age,
  gender = gender,
  race = race,
  treatment = treatment,
  surv_time = surv_time,
  censoring_status = censoring_status
)

head(survival_data)

#Kaplan-meier survival curves#
library(survival)

km_fit <-  survfit(Surv(surv_time, censoring_status) ~ treatment, data = survival_data)
ggsurvplot(km_fit) 

#Log-rank test for different treatment groups#

survdiff(Surv(surv_time, censoring_status) ~ treatment, data = survival_data)


#Cox- proportional hazards model#
cox_model <- coxph(Surv(surv_time, censoring_status) ~ age + gender + treatment + race, data = survival_data)
summary(cox_model)


#Visualizing#
install.packages("survminer")
library(survminer)

ggcoxdiagnostics(cox_model, type = "deviance", ggtheme = theme_bw())

ggsurvplot(km_fit)

ggcoxdiagnostics(cox_model)


